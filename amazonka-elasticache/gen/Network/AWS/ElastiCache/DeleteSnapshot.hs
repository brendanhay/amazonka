{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
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
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DeleteSnapshot
-- &SnapshotName=my-manual-snapshot &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- my-redis-primary 6379 cache.m1.small default.redis2.8 redis us-east-1d
-- 2014-04-01T18:46:57.972Z 2.8.6 manual true wed:09:00-wed:10:00
-- my-manual-snapshot 5 2014-04-01T18:54:12Z 2014-04-01T18:46:57.972Z 0001 3
-- MB deleting 1 07:30-08:30 694d7017-b9d2-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.DeleteSnapshot
    (
    -- * Request
      DeleteSnapshot
    -- ** Request constructor
    , deleteSnapshot
    -- ** Request lenses
    , dsSnapshotName

    -- * Response
    , DeleteSnapshotResponse
    -- ** Response constructor
    , deleteSnapshotResponse
    -- ** Response lenses
    , dsrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DeleteSnapshotMessage operation.
newtype DeleteSnapshot = DeleteSnapshot
    { _dsSnapshotName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSnapshot' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotName ::@ @Text@
--
deleteSnapshot :: Text -- ^ 'dsSnapshotName'
                 -> DeleteSnapshot
deleteSnapshot p1 = DeleteSnapshot
    { _dsSnapshotName = p1
    }

-- | The name of the snapshot to be deleted.
dsSnapshotName :: Lens' DeleteSnapshot Text
dsSnapshotName = lens _dsSnapshotName (\s a -> s { _dsSnapshotName = a })

instance ToQuery DeleteSnapshot where
    toQuery = genericQuery def

newtype DeleteSnapshotResponse = DeleteSnapshotResponse
    { _dsrSnapshot :: Maybe Snapshot
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Snapshot ::@ @Maybe Snapshot@
--
deleteSnapshotResponse :: DeleteSnapshotResponse
deleteSnapshotResponse = DeleteSnapshotResponse
    { _dsrSnapshot = Nothing
    }

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
dsrSnapshot :: Lens' DeleteSnapshotResponse (Maybe Snapshot)
dsrSnapshot = lens _dsrSnapshot (\s a -> s { _dsrSnapshot = a })

instance FromXML DeleteSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteSnapshot where
    type Sv DeleteSnapshot = ElastiCache
    type Rs DeleteSnapshot = DeleteSnapshotResponse

    request = post "DeleteSnapshot"
    response _ = xmlResponse
