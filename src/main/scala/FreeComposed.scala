package main

import scalaz.{Coproduct, Id, Free, Functor, Inject, ~>}
import scalaz.Inject._

object Example {

  sealed trait ConsoleOps[A]
  case class Print(out: String) extends ConsoleOps[Unit]

  sealed trait StoreOps[A]
  case class Get(key: String) extends StoreOps[Option[String]]
  case class Set(key: String, value: String) extends StoreOps[Unit]

  def lift[F[_], G[_], A](fa: F[A])(implicit i: Inject[F, G]):
  Free[G, A] = Free.liftF(i.inj(fa))

  class Console[F[_]](implicit i: Inject[ConsoleOps, F]) {
    def print(s: String): Free[F, Unit] = lift(Print(s))
  }

  class Store[F[_]](implicit i: Inject[StoreOps, F]) {
    def get(key: String): Free[F, Option[String]] =
      lift(Get(key))
    def set(key: String, value: String): Free[F, Unit] =
      lift(Set(key, value))
  }

  implicit class NTOps[F[_], G[_]](nt: F ~> G) {
    def or[H[_]](ont: H ~> G):
    ({type cp[P]=Coproduct[F,H,P]})#cp ~> G =
      new (({type cp[P]=Coproduct[F,H,P]})#cp ~> G) {
        def apply[A](fa: Coproduct[F, H, A]): G[A] =
          fa.run.fold(nt(_), ont(_))
      }
  }

  type Prog[A] = Coproduct[ConsoleOps, StoreOps, A]
  val store = new Store[Prog]()
  val console = new Console[Prog]()

  val prog: Free[Prog, Unit] = for {
     _ <- console.print("Starting app")
     _ <- store.set("foo", "bar")
     res <- store.get("foo")
     _ <- console.print("Looked up foo: " ++ res.toString)
  } yield ()

  val runConsole = new (ConsoleOps ~> Id.Id) {
    def apply[A](op: ConsoleOps[A]) = op match {
      case Print(s) => println(s)
    }
  }

  def runStore = new (StoreOps ~> Id.Id) {
    private var m = scala.collection.mutable.Map[String, String]()
    def apply[A](op: StoreOps[A]) = op match {
      case Set(k, v) => {
        m.put(k, v)
        ()
      }
      case Get(k) => m.get(k)
    }
  }

  def run = prog.foldMap(runConsole.or(runStore))
}
