{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , cSnapshot
    , cStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /CreateSnapshot/ action.
--
-- /See:/ 'createSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csCacheClusterId'
--
-- * 'csSnapshotName'
data CreateSnapshot = CreateSnapshot'
    { _csCacheClusterId :: !Text
    , _csSnapshotName   :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateSnapshot' smart constructor.
createSnapshot :: Text -> Text -> CreateSnapshot
createSnapshot pCacheClusterId pSnapshotName =
    CreateSnapshot'
    { _csCacheClusterId = pCacheClusterId
    , _csSnapshotName = pSnapshotName
    }

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
                 CreateSnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

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
-- * 'cSnapshot'
--
-- * 'cStatus'
data CreateSnapshotResponse = CreateSnapshotResponse'
    { _cSnapshot :: !(Maybe Snapshot)
    , _cStatus   :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateSnapshotResponse' smart constructor.
createSnapshotResponse :: Int -> CreateSnapshotResponse
createSnapshotResponse pStatus =
    CreateSnapshotResponse'
    { _cSnapshot = Nothing
    , _cStatus = pStatus
    }

-- | FIXME: Undocumented member.
cSnapshot :: Lens' CreateSnapshotResponse (Maybe Snapshot)
cSnapshot = lens _cSnapshot (\ s a -> s{_cSnapshot = a});

-- | FIXME: Undocumented member.
cStatus :: Lens' CreateSnapshotResponse Int
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});
