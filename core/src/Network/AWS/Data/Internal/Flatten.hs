{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Module      : Network.AWS.Data.Internal.Flatten
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Flatten where

import Control.Lens
import Data.Monoid
import Data.Semigroup
import Data.String
import Network.AWS.Data.Internal.ByteString
import Network.AWS.Data.Internal.Text

-- import Network.AWS.Data.Internal.JSON
-- import Network.AWS.Data.Internal.Query
-- import Network.AWS.Data.Internal.XML

newtype Flatten a = Flatten { flatten :: a }
    deriving (Eq, Ord, Show, IsString, Monoid, Semigroup)

_Flatten :: Iso' (Flatten a) a
_Flatten = iso flatten Flatten
