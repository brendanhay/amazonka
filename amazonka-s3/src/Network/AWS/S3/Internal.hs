{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Module      : Network.AWS.S3.Internal
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Internal
    ( module Network.AWS.S3.Internal
    , Region
    ) where

import Data.String
import GHC.Generics
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype BucketName = BucketName Text
    deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

newtype ObjectKey = ObjectKey Text
    deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

newtype ObjectVersionId = ObjectVersionId Text
    deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

newtype ETag = ETag Text
    deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )
