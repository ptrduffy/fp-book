module Ch19 where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Tuple (Tuple (..))

type Config = { debugModeOn :: Boolean }
type Counter = Int

newtype Reader r a = Reader (r -> a)
newtype Writer w a = Writer (Tuple a w)
newtype State s a = State (s -> Tuple a s)

type RWSResult r w s = 
  { r :: r
  , w :: w
  , s :: s
  }

newtype RWS r w s a = RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

instance functorRWS :: Functor (RWS r w s) where
  --map :: ∀ b. ((RWSResult r w s) -> b) -> (RWS r w s) a -> (RWS r w s) b
  map f (RWS g) = RWS \rwsr -> g rwsr # \(Tuple x rwsr') -> Tuple (f x) rwsr'

instance applyRWS :: Monoid w => Apply (RWS r w s) where
  apply = ap

instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
  pure x = RWS \{r, s} -> Tuple x { r, w: mempty, s}  -- when lifting x, don't want to add to writer, hence 'w: mempty'

instance bindRWS :: Monoid w => Bind (RWS r w s) where
  -- bind :: m a -> (a -> m b) -> m b
  bind (RWS g) f = RWS \rwsr -> g rwsr
    # \(Tuple x rwsr'@{ w }) -> runRWS (f x) rwsr'
      # \(Tuple y rwsr''@{ w:w' }) -> Tuple y rwsr'' { w = w <> w'}

instance monadRWS :: Monoid w => Monad (RWS r w s)

runRWS
  :: ∀ r w s a
  . RWS r w s a
  -> (RWSResult r w s -> Tuple a (RWSResult r w s))
runRWS (RWS g) = g

tell :: ∀ r w s. w -> RWS r w s Unit
tell w = RWS \{r, s} -> Tuple unit {r, w, s}

ask :: ∀ r w s. Monoid w => RWS r w s r
ask = RWS \{ r, s } -> Tuple r {r, w: mempty, s}

get :: ∀ r w s. Monoid w => RWS r w s s
get = RWS \{ r, s } -> Tuple s {r, w: mempty, s}

put :: ∀ r w s. Monoid w => s -> RWS r w s Unit
put s = RWS \{ r } -> Tuple unit {r, w: mempty, s}

rwsTest :: RWS Config (Array String) Counter Unit
rwsTest = do
  tell ["test the log"]
  tell ["test the log2", "test the log3"]
  config <- ask
  tell ["the config is " <> show config]
  counter <- get
  tell ["old counter is " <> show counter]
  put $ counter + 1
  newCounter <- get
  tell ["new counter is " <> show newCounter]
  pure unit

test :: Effect Unit
test = do
  log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 }

