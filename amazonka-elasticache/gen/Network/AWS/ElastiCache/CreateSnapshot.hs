{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /CreateSnapshot/ action creates a copy of an entire cache cluster at
-- a specific moment in time.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateSnapshot.html>
module Network.AWS.ElastiCache.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , csCacheClusterId
    , csSnapshotName

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , createSnapshotResponse
    -- ** Response lenses
    , creSnapshot
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElastiCache.Types

-- | /See:/ 'createSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csCacheClusterId'
--
-- * 'csSnapshotName'
data CreateSnapshot = CreateSnapshot'{_csCacheClusterId :: Text, _csSnapshotName :: Text} deriving (Eq, Read, Show)

-- | 'CreateSnapshot' smart constructor.
createSnapshot :: Text -> Text -> CreateSnapshot
createSnapshot pCacheClusterId pSnapshotName = CreateSnapshot'{_csCacheClusterId = pCacheClusterId, _csSnapshotName = pSnapshotName};

-- | The identifier of an existing cache cluster. The snapshot will be
-- created from this cache cluster.
csCacheClusterId :: Lens' CreateSnapshot Text
csCacheClusterId = lens _csCacheClusterId (\ s a -> s{_csCacheClusterId = a});

-- | A name for the snapshot being created.
csSnapshotName :: Lens' CreateSnapshot Text
csSnapshotName = lens _csSnapshotName (\ s a -> s{_csSnapshotName = a});

instance AWSRequest CreateSnapshot where
        type Sv CreateSnapshot = ElastiCache
        type Rs CreateSnapshot = CreateSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CreateSnapshotResult"
              (\ s h x ->
                 CreateSnapshotResponse' <$> (x .@? "Snapshot"))

instance ToHeaders CreateSnapshot where
        toHeaders = const mempty

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery CreateSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateSnapshot" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheClusterId" =: _csCacheClusterId,
               "SnapshotName" =: _csSnapshotName]

-- | /See:/ 'createSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creSnapshot'
newtype CreateSnapshotResponse = CreateSnapshotResponse'{_creSnapshot :: Maybe Snapshot} deriving (Eq, Read, Show)

-- | 'CreateSnapshotResponse' smart constructor.
createSnapshotResponse :: CreateSnapshotResponse
createSnapshotResponse = CreateSnapshotResponse'{_creSnapshot = Nothing};

-- | FIXME: Undocumented member.
creSnapshot :: Lens' CreateSnapshotResponse (Maybe Snapshot)
creSnapshot = lens _creSnapshot (\ s a -> s{_creSnapshot = a});
