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
-- Module      : Network.AWS.ElastiCache.StartMigration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the migration of data.
module Network.AWS.ElastiCache.StartMigration
  ( -- * Creating a Request
    startMigration,
    StartMigration,

    -- * Request Lenses
    smReplicationGroupId,
    smCustomerNodeEndpointList,

    -- * Destructuring the Response
    startMigrationResponse,
    StartMigrationResponse,

    -- * Response Lenses
    smrsReplicationGroup,
    smrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startMigration' smart constructor.
data StartMigration = StartMigration'
  { _smReplicationGroupId ::
      !Text,
    _smCustomerNodeEndpointList :: ![CustomerNodeEndpoint]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMigration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smReplicationGroupId' - The ID of the replication group to which data should be migrated.
--
-- * 'smCustomerNodeEndpointList' - List of endpoints from which data should be migrated. For Redis (cluster mode disabled), list should have only one element.
startMigration ::
  -- | 'smReplicationGroupId'
  Text ->
  StartMigration
startMigration pReplicationGroupId_ =
  StartMigration'
    { _smReplicationGroupId = pReplicationGroupId_,
      _smCustomerNodeEndpointList = mempty
    }

-- | The ID of the replication group to which data should be migrated.
smReplicationGroupId :: Lens' StartMigration Text
smReplicationGroupId = lens _smReplicationGroupId (\s a -> s {_smReplicationGroupId = a})

-- | List of endpoints from which data should be migrated. For Redis (cluster mode disabled), list should have only one element.
smCustomerNodeEndpointList :: Lens' StartMigration [CustomerNodeEndpoint]
smCustomerNodeEndpointList = lens _smCustomerNodeEndpointList (\s a -> s {_smCustomerNodeEndpointList = a}) . _Coerce

instance AWSRequest StartMigration where
  type Rs StartMigration = StartMigrationResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "StartMigrationResult"
      ( \s h x ->
          StartMigrationResponse'
            <$> (x .@? "ReplicationGroup") <*> (pure (fromEnum s))
      )

instance Hashable StartMigration

instance NFData StartMigration

instance ToHeaders StartMigration where
  toHeaders = const mempty

instance ToPath StartMigration where
  toPath = const "/"

instance ToQuery StartMigration where
  toQuery StartMigration' {..} =
    mconcat
      [ "Action" =: ("StartMigration" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "ReplicationGroupId" =: _smReplicationGroupId,
        "CustomerNodeEndpointList"
          =: toQueryList "member" _smCustomerNodeEndpointList
      ]

-- | /See:/ 'startMigrationResponse' smart constructor.
data StartMigrationResponse = StartMigrationResponse'
  { _smrsReplicationGroup ::
      !(Maybe ReplicationGroup),
    _smrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMigrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smrsReplicationGroup' - Undocumented member.
--
-- * 'smrsResponseStatus' - -- | The response status code.
startMigrationResponse ::
  -- | 'smrsResponseStatus'
  Int ->
  StartMigrationResponse
startMigrationResponse pResponseStatus_ =
  StartMigrationResponse'
    { _smrsReplicationGroup = Nothing,
      _smrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
smrsReplicationGroup :: Lens' StartMigrationResponse (Maybe ReplicationGroup)
smrsReplicationGroup = lens _smrsReplicationGroup (\s a -> s {_smrsReplicationGroup = a})

-- | -- | The response status code.
smrsResponseStatus :: Lens' StartMigrationResponse Int
smrsResponseStatus = lens _smrsResponseStatus (\s a -> s {_smrsResponseStatus = a})

instance NFData StartMigrationResponse
