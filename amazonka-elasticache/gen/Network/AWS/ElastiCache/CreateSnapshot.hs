{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /CreateSnapshot/ action creates a copy of an entire cache cluster at
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
    , csrqCacheClusterId
    , csrqSnapshotName

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , createSnapshotResponse
    -- ** Response lenses
    , csrsSnapshot
    , csrsStatus
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
-- * 'csrqCacheClusterId'
--
-- * 'csrqSnapshotName'
data CreateSnapshot = CreateSnapshot'
    { _csrqCacheClusterId :: !Text
    , _csrqSnapshotName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshot' smart constructor.
createSnapshot :: Text -> Text -> CreateSnapshot
createSnapshot pCacheClusterId pSnapshotName =
    CreateSnapshot'
    { _csrqCacheClusterId = pCacheClusterId
    , _csrqSnapshotName = pSnapshotName
    }

-- | The identifier of an existing cache cluster. The snapshot will be
-- created from this cache cluster.
csrqCacheClusterId :: Lens' CreateSnapshot Text
csrqCacheClusterId = lens _csrqCacheClusterId (\ s a -> s{_csrqCacheClusterId = a});

-- | A name for the snapshot being created.
csrqSnapshotName :: Lens' CreateSnapshot Text
csrqSnapshotName = lens _csrqSnapshotName (\ s a -> s{_csrqSnapshotName = a});

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
               "CacheClusterId" =: _csrqCacheClusterId,
               "SnapshotName" =: _csrqSnapshotName]

-- | /See:/ 'createSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrsSnapshot'
--
-- * 'csrsStatus'
data CreateSnapshotResponse = CreateSnapshotResponse'
    { _csrsSnapshot :: !(Maybe Snapshot)
    , _csrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshotResponse' smart constructor.
createSnapshotResponse :: Int -> CreateSnapshotResponse
createSnapshotResponse pStatus =
    CreateSnapshotResponse'
    { _csrsSnapshot = Nothing
    , _csrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
csrsSnapshot :: Lens' CreateSnapshotResponse (Maybe Snapshot)
csrsSnapshot = lens _csrsSnapshot (\ s a -> s{_csrsSnapshot = a});

-- | FIXME: Undocumented member.
csrsStatus :: Lens' CreateSnapshotResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
