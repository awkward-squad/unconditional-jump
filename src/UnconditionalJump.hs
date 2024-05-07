module UnconditionalJump
  ( -- * Labels
    Label,
    label,
    goto,

    -- ** Derived @label@ variants
    label',
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, catch, throwIO)
import Data.Functor.Contravariant (Contravariant (contramap))
import IntSupply (IntSupply)
import IntSupply qualified
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

newtype Label a
  = Label (forall x. a -> IO x)

instance Contravariant Label where
  contramap f (Label g) =
    Label (g . f)

-- | Create a label.
label :: (Label a -> IO a) -> IO a
label f = do
  i <- IntSupply.next supply
  catch (f (Label (throwIO . X i))) \err@(X j x) ->
    if i == j
      then pure (unsafeCoerce x)
      else throwIO err

-- | Like 'label', but for the common use case of distinguishing between returning early from a block and reaching
-- the end of a block at the value level.
--
-- For example, you may wish to tag early-returned values with a @Left@:
--
-- @
-- result <-
--   label' Left Right \\l ->
--     ...
--
-- case result of
--   Left _ -> {- returned early -}
--   Right _ -> {- reached the end of the block -}
-- @
label' :: (a -> c) -> (b -> c) -> (Label a -> IO b) -> IO c
label' f g action =
  label (fmap g . action . contramap f)

-- | Go to a label.
goto :: Label a -> a -> IO x
goto (Label f) x =
  f x

data X = forall a. X {-# UNPACK #-} !Int a

instance Exception X where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

instance Show X where
  show _ = "«unconditional-jump»"

supply :: IntSupply
supply = unsafePerformIO IntSupply.new
{-# NOINLINE supply #-}
