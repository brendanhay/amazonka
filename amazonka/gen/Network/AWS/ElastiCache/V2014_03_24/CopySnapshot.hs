{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.CopySnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CopySnapshot operation makes a copy of an existing snapshot.
module Network.AWS.ElastiCache.V2014_03_24.CopySnapshot where

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

data CopySnapshot = CopySnapshot
    { _csmTargetSnapshotName :: Text
      -- ^ A name for the copied snapshot.
    , _csmSourceSnapshotName :: Text
      -- ^ The name of an existing snapshot from which to copy.
    } deriving (Generic)

instance ToQuery CopySnapshot where
    toQuery = genericToQuery def

instance AWSRequest CopySnapshot where
    type Sv CopySnapshot = ElastiCache
    type Rs CopySnapshot = CopySnapshotResponse

    request = post "CopySnapshot"
    response _ = xmlResponse

data CopySnapshotResponse = CopySnapshotResponse
    { _sssssssssssssssrSnapshot :: Maybe Snapshot
      -- ^ Represents a copy of an entire cache cluster as of the time when
      -- the snapshot was taken.
    } deriving (Generic)

instance FromXML CopySnapshotResponse where
    fromXMLOptions = xmlOptions
