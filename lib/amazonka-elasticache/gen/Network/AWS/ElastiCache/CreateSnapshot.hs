{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an entire cluster or replication group at a specific moment in time.
module Network.AWS.ElastiCache.CreateSnapshot
  ( -- * Creating a Request
    createSnapshot,
    CreateSnapshot,

    -- * Request Lenses
    cCacheClusterId,
    cKMSKeyId,
    cReplicationGroupId,
    cSnapshotName,

    -- * Destructuring the Response
    createSnapshotResponse,
    CreateSnapshotResponse,

    -- * Response Lenses
    crersSnapshot,
    crersResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @CreateSnapshot@ operation.
--
--
--
-- /See:/ 'createSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { _cCacheClusterId ::
      !(Maybe Text),
    _cKMSKeyId :: !(Maybe Text),
    _cReplicationGroupId :: !(Maybe Text),
    _cSnapshotName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCacheClusterId' - The identifier of an existing cluster. The snapshot is created from this cluster.
--
-- * 'cKMSKeyId' - The ID of the KMS key used to encrypt the snapshot.
--
-- * 'cReplicationGroupId' - The identifier of an existing replication group. The snapshot is created from this replication group.
--
-- * 'cSnapshotName' - A name for the snapshot being created.
createSnapshot ::
  -- | 'cSnapshotName'
  Text ->
  CreateSnapshot
createSnapshot pSnapshotName_ =
  CreateSnapshot'
    { _cCacheClusterId = Nothing,
      _cKMSKeyId = Nothing,
      _cReplicationGroupId = Nothing,
      _cSnapshotName = pSnapshotName_
    }

-- | The identifier of an existing cluster. The snapshot is created from this cluster.
cCacheClusterId :: Lens' CreateSnapshot (Maybe Text)
cCacheClusterId = lens _cCacheClusterId (\s a -> s {_cCacheClusterId = a})

-- | The ID of the KMS key used to encrypt the snapshot.
cKMSKeyId :: Lens' CreateSnapshot (Maybe Text)
cKMSKeyId = lens _cKMSKeyId (\s a -> s {_cKMSKeyId = a})

-- | The identifier of an existing replication group. The snapshot is created from this replication group.
cReplicationGroupId :: Lens' CreateSnapshot (Maybe Text)
cReplicationGroupId = lens _cReplicationGroupId (\s a -> s {_cReplicationGroupId = a})

-- | A name for the snapshot being created.
cSnapshotName :: Lens' CreateSnapshot Text
cSnapshotName = lens _cSnapshotName (\s a -> s {_cSnapshotName = a})

instance AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = CreateSnapshotResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "CreateSnapshotResult"
      ( \s h x ->
          CreateSnapshotResponse'
            <$> (x .@? "Snapshot") <*> (pure (fromEnum s))
      )

instance Hashable CreateSnapshot

instance NFData CreateSnapshot

instance ToHeaders CreateSnapshot where
  toHeaders = const mempty

instance ToPath CreateSnapshot where
  toPath = const "/"

instance ToQuery CreateSnapshot where
  toQuery CreateSnapshot' {..} =
    mconcat
      [ "Action" =: ("CreateSnapshot" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "CacheClusterId" =: _cCacheClusterId,
        "KmsKeyId" =: _cKMSKeyId,
        "ReplicationGroupId" =: _cReplicationGroupId,
        "SnapshotName" =: _cSnapshotName
      ]

-- | /See:/ 'createSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { _crersSnapshot ::
      !(Maybe Snapshot),
    _crersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersSnapshot' - Undocumented member.
--
-- * 'crersResponseStatus' - -- | The response status code.
createSnapshotResponse ::
  -- | 'crersResponseStatus'
  Int ->
  CreateSnapshotResponse
createSnapshotResponse pResponseStatus_ =
  CreateSnapshotResponse'
    { _crersSnapshot = Nothing,
      _crersResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
crersSnapshot :: Lens' CreateSnapshotResponse (Maybe Snapshot)
crersSnapshot = lens _crersSnapshot (\s a -> s {_crersSnapshot = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateSnapshotResponse Int
crersResponseStatus = lens _crersResponseStatus (\s a -> s {_crersResponseStatus = a})

instance NFData CreateSnapshotResponse
