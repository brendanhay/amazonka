{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Network.AWS.S3.Headers
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Network.AWS.S3.Headers where

import           Data.Monoid
import           Data.Text    (Text)
import qualified Data.Text    as Text
import           GHC.TypeLits

class ToHeader a where
    header :: a -> Text -> (Text, Text)

instance ToHeader v => ToHeader (Text, v) where
    header (k, v) = header v . (`mappend` k)

instance ToHeader Text where
    header s = (, s)

instance ToHeader String where
    header s = (, Text.pack s)

instance ToHeader Integer where
    header n = header (show n)

data Hdr (k :: Symbol) v = Hdr v

instance Functor (Hdr k) where
    fmap f (Hdr x) = Hdr $ f x

instance (SingI k, ToHeader v) => ToHeader (Hdr k v) where
    header h@(Hdr v) = header v . mappend (withSing $ f h)
      where
        f :: Hdr k v -> Sing k -> Text
        f _ = Text.pack . fromSing

instance (SingI k, ToHeader v) => Show (Hdr k v) where
    show = show . (`header` mempty)

data Header where
    Header :: ToHeader a => a -> Header

instance Show Header where
    show (Header h) = show $ header h mempty

pack :: ToHeader a => a -> Header
pack = Header
