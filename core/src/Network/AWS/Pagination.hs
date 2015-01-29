{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

-- Module      : Network.AWS.Pagination
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Pagination
    ( more
    , stop
    , index
    , choice
    ) where

import           Control.Applicative
import           Control.Lens        hiding (index)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Text           (Text)
import           Network.AWS.Data    (ToText(..))

-- | Generalise IsTruncated and other optional/required
-- response pagination fields.
class AWSMore a where
    more :: a -> Bool

instance AWSMore Bool where
    more = id

instance AWSMore (Maybe Bool) where
    more (Just x) = x
    more Nothing  = False

instance AWSMore (Maybe Text) where
    more (Just _) = True
    more Nothing  = False

instance AWSMore [a] where
    more = not . null

instance AWSMore (HashMap k v) where
    more = not . Map.null

stop :: AWSMore a => a -> Bool
stop = not . more

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
