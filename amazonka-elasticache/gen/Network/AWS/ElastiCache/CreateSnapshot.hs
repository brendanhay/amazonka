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
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /CreateSnapshot/ action creates a copy of an entire cache cluster at
-- a specific moment in time.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateSnapshot.html AWS API Reference> for CreateSnapshot.
module Network.AWS.ElastiCache.CreateSnapshot
    (
    -- * Creating a Request
      CreateSnapshot
    , createSnapshot
    -- * Request Lenses
    , csCacheClusterId
    , csSnapshotName

    -- * Destructuring the Response
    , CreateSnapshotResponse
    , createSnapshotResponse
    -- * Response Lenses
    , crersSnapshot
    , crersStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshot' smart constructor.
createSnapshot :: Text -> Text -> CreateSnapshot
createSnapshot pCacheClusterId_ pSnapshotName_ =
    CreateSnapshot'
    { _csCacheClusterId = pCacheClusterId_
    , _csSnapshotName = pSnapshotName_
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
        request = postQuery
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
-- * 'crersSnapshot'
--
-- * 'crersStatus'
data CreateSnapshotResponse = CreateSnapshotResponse'
    { _crersSnapshot :: !(Maybe Snapshot)
    , _crersStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshotResponse' smart constructor.
createSnapshotResponse :: Int -> CreateSnapshotResponse
createSnapshotResponse pStatus_ =
    CreateSnapshotResponse'
    { _crersSnapshot = Nothing
    , _crersStatus = pStatus_
    }

-- | Undocumented member.
crersSnapshot :: Lens' CreateSnapshotResponse (Maybe Snapshot)
crersSnapshot = lens _crersSnapshot (\ s a -> s{_crersSnapshot = a});

-- | Undocumented member.
crersStatus :: Lens' CreateSnapshotResponse Int
crersStatus = lens _crersStatus (\ s a -> s{_crersStatus = a});
