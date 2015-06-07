{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

-- Module      : Network.AWS.Paginate
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Paginate
    ( AWSPage     (..)
    , AWSContinue (..)
    , stop
    , index
    , choice
    ) where

import           Control.Applicative
import           Control.Lens          hiding (index)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as Map
import           Data.Text             (Text)
import           Network.AWS.Data.Text (ToText (..))
import           Network.AWS.Types

-- | Specify how an 'AWSRequest' and it's associated 'Rs' response can
-- generate a subsequent request, if available.
class AWSRequest a => AWSPage a where
    page :: a -> Rs a -> Maybe a

-- | Generalise IsTruncated and other optional/required
-- response pagination fields.
class AWSContinue a where
    continue :: a -> Bool

instance AWSContinue Bool where
    continue = id

instance AWSContinue (Maybe Bool) where
    continue (Just x) = x
    continue Nothing  = False

instance AWSContinue (Maybe Text) where
    continue (Just _) = True
    continue Nothing  = False

instance AWSContinue [a] where
    continue = not . null

instance AWSContinue (HashMap k v) where
    continue = not . Map.null

stop :: AWSContinue a => a -> Bool
stop = not . continue

index :: ToText c => Getter a [b] -> Getter b c -> Getter a (Maybe Text)
index f g = f . to lastMay . to (fmap (toText . view g))

choice :: Alternative f => (a -> f b) -> (a -> f b) -> a -> f b
choice f g x = f x <|> g x

lastMay :: [a] -> Maybe a
lastMay []     = Nothing
lastMay (x:xs) = Just (go x xs)
  where
    go y []     = y
    go _ (y:ys) = go y ys
