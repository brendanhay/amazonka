{-# LANGUAGE RankNTypes #-}

-- Module      : Network.AWS.Pagination
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Pagination where

import Control.Applicative
import Control.Lens
import Data.Text           (Text)
import Network.AWS.Data
-- import Network.AWS.Types

-- | Generalise IsTruncated and other optional/required
-- response pagination fields.
class AWSMore a where
    more :: a -> Bool

instance AWSMore Bool where
    more = id

instance AWSMore a => AWSMore (Maybe a) where
    more (Just x) = more x
    more Nothing  = False

stop :: AWSMore a => a -> Bool
stop = not . more

-- class AWSNext a where
--     type Next a

--     next :: a -> Next a

-- instance AWSNext a => AWSNext (Maybe a) where

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
