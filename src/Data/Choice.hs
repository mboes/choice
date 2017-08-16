-- | Represent do\/don't, is\/isn't, with\/without flags with 'Choice'.
--
-- <https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/ Boolean blindness>
-- refers to the problem that boolean literals on their own aren't very
-- informative. In any given context, what does 'True' mean? What does 'False'
-- mean? Instead of passing arguments of type 'Bool' to functions, consider
-- using 'Choice'.
--
-- 'Choice' is the type of labeled booleans. Use it as follows:
--
-- @
-- {-\# LANGUAGE OverloadedLabels \#-}
--
-- import Data.Choice (Choice, Do, Don't)
--
-- -- Blocking read: block until N bytes available.
-- -- Non-blocking: return as many bytes as are available.
-- readBytes :: Handle -> Choice "block" -> Int -> IO ByteString
-- readBytes = ...
--
-- action1 = print =<< readBytes h (Don't #block) 1024
-- @
--
-- For GHC < 8.0, where overloaded labels are not available, substitute
-- @(Label :: Label "block")@ for @#block@.
--
-- __A comment on labels:__ why use labels? We could as well ask the user to
-- define ad hoc constructors. But unlike constructors, labels are guaranteed to
-- be singletons. That's exactly what we want: a label doesn't carry any runtime
-- information, it's just a type annotation. Better yet, with labels, there is
-- no need to ensure that constructor names are unique, nor to pollute the
-- precious constructor namespace in a large module with many flags.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
#endif

module Data.Choice
  ( Choice
  -- * Conversion
  , fromBool
  , toBool
  -- * Choice aliases
  , pattern Do
  , pattern Don't
  , pattern Is
  , pattern Isn't
  , pattern With
  , pattern Without
  , pattern Must
  , pattern Mustn't
  , pattern Needn't
  , pattern Can
  , pattern Can't
  -- * Internal
  -- $label-export
  , Label(..)
  ) where

#if MIN_VERSION_base(4,9,0)
import GHC.OverloadedLabels (IsLabel(..))
#endif
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)

-- $label-export
--
-- The 'Label' data type is only exported in full for compatibility with
-- versions of GHC older than 8.0.

-- | A synonym for 'Data.Proxy.Proxy'.
data Label (a :: Symbol) = Label deriving (Eq, Ord, Show)

#if MIN_VERSION_base(4,10,0)
instance x ~ x' => IsLabel x (Label x') where
  fromLabel = Label
#elif MIN_VERSION_base(4,9,0)
instance x ~ x' => IsLabel x (Label x') where
  fromLabel _ = Label
#endif

-- | A labeled boolean choice.
data Choice (a :: Symbol)
  = Off {-# UNPACK #-} !(Label a)
  | On {-# UNPACK #-} !(Label a)
  deriving (Eq, Ord, Generic, Typeable)

instance Show (Choice a) where
  show x = "fromBool " ++ show (toBool x)

instance Enum (Choice a) where
  toEnum 0 = Off Label
  toEnum 1 = On Label
  toEnum _ = error "Prelude.Enum.Choice.toEnum: bad argument"

  fromEnum (Off _) = 0
  fromEnum (On _) = 1

instance Bounded (Choice a) where
  minBound = Off Label
  maxBound = On Label

-- | Alias for 'True', e.g. @Do #block@.
pattern Do x = On x

-- | Alias for 'False', e.g. @Don't #block@.
pattern Don't x = Off x

-- | Alias for 'True', e.g. @Is #ordered@.
pattern Is x = On x

-- | Alias for 'False', e.g. @Isn't #ordered@.
pattern Isn't x = Off x

-- | Alias for 'True', e.g. @With #ownDirectory@.
pattern With x = On x

-- | Alias for 'False', e.g. @Without #ownDirectory@.
pattern Without x = Off x

-- | Alias for 'True', e.g. @Must #succeed@.
pattern Must x = On x

-- | Alias for 'False', e.g. @Mustn't #succeed@.
pattern Mustn't x = Off x

-- | Alias for 'False', e.g. @Needn't #succeed@.
pattern Needn't x = Off x

-- | Alias for 'True', e.g. @Can #fail@.
pattern Can x = On x

-- | Alias for 'False', e.g. @Can't #fail@.
pattern Can't x = Off x

toBool :: Choice a -> Bool
toBool (Off _) = False
toBool (On _) = True

fromBool :: Bool -> Choice a
fromBool False = Off Label
fromBool True = On Label
