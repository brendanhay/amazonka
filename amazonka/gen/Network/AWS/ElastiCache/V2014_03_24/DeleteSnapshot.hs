{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DeleteSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteSnapshot operation deletes an existing snapshot. When you receive
-- a successful response from this operation, ElastiCache immediately begins
-- deleting the snapshot; you cannot cancel or revert this operation.
module Network.AWS.ElastiCache.V2014_03_24.DeleteSnapshot where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data DeleteSnapshot = DeleteSnapshot
    { _dsnSnapshotName :: Text
      -- ^ The name of the snapshot to be deleted.
    } deriving (Generic)

makeLenses ''DeleteSnapshot

instance ToQuery DeleteSnapshot where
    toQuery = genericToQuery def

data DeleteSnapshotResponse = DeleteSnapshotResponse
    { _sssssssssssssssssszSnapshot :: Maybe Snapshot
      -- ^ Represents a copy of an entire cache cluster as of the time when
      -- the snapshot was taken.
    } deriving (Generic)

makeLenses ''DeleteSnapshotResponse

instance FromXML DeleteSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteSnapshot where
    type Sv DeleteSnapshot = ElastiCache
    type Rs DeleteSnapshot = DeleteSnapshotResponse

    request = post "DeleteSnapshot"
    response _ = xmlResponse
