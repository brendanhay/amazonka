{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.CreateSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateSnapshot operation creates a copy of an entire cache cluster at a
-- specific moment in time.
module Network.AWS.ElastiCache.V2014_03_24.CreateSnapshot where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElastiCache.V2014_03_24.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CreateSnapshot = CreateSnapshot
    { _csoCacheClusterId :: Text
      -- ^ The identifier of an existing cache cluster. The snapshot will be
      -- created from this cache cluster.
    , _csoSnapshotName :: Text
      -- ^ A name for the snapshot being created.
    } deriving (Generic)

instance ToQuery CreateSnapshot where
    toQuery = genericToQuery def

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = ElastiCache
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = post "CreateSnapshot"
    response _ = xmlResponse

data CreateSnapshotResponse = CreateSnapshotResponse
    { _sssssssssssssssssssssrSnapshot :: Maybe Snapshot
      -- ^ Represents a copy of an entire cache cluster as of the time when
      -- the snapshot was taken.
    } deriving (Generic)

instance FromXML CreateSnapshotResponse where
    fromXMLOptions = xmlOptions
