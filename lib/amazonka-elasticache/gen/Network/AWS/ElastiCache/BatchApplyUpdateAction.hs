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
-- Module      : Network.AWS.ElastiCache.BatchApplyUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Apply the service update. For more information on service updates and applying them, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/applying-updates.html Applying Service Updates> .
module Network.AWS.ElastiCache.BatchApplyUpdateAction
  ( -- * Creating a Request
    batchApplyUpdateAction,
    BatchApplyUpdateAction,

    -- * Request Lenses
    bauaCacheClusterIds,
    bauaReplicationGroupIds,
    bauaServiceUpdateName,

    -- * Destructuring the Response
    updateActionResultsMessage,
    UpdateActionResultsMessage,

    -- * Response Lenses
    uarmUnprocessedUpdateActions,
    uarmProcessedUpdateActions,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchApplyUpdateAction' smart constructor.
data BatchApplyUpdateAction = BatchApplyUpdateAction'
  { _bauaCacheClusterIds ::
      !(Maybe [Text]),
    _bauaReplicationGroupIds :: !(Maybe [Text]),
    _bauaServiceUpdateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchApplyUpdateAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bauaCacheClusterIds' - The cache cluster IDs
--
-- * 'bauaReplicationGroupIds' - The replication group IDs
--
-- * 'bauaServiceUpdateName' - The unique ID of the service update
batchApplyUpdateAction ::
  -- | 'bauaServiceUpdateName'
  Text ->
  BatchApplyUpdateAction
batchApplyUpdateAction pServiceUpdateName_ =
  BatchApplyUpdateAction'
    { _bauaCacheClusterIds = Nothing,
      _bauaReplicationGroupIds = Nothing,
      _bauaServiceUpdateName = pServiceUpdateName_
    }

-- | The cache cluster IDs
bauaCacheClusterIds :: Lens' BatchApplyUpdateAction [Text]
bauaCacheClusterIds = lens _bauaCacheClusterIds (\s a -> s {_bauaCacheClusterIds = a}) . _Default . _Coerce

-- | The replication group IDs
bauaReplicationGroupIds :: Lens' BatchApplyUpdateAction [Text]
bauaReplicationGroupIds = lens _bauaReplicationGroupIds (\s a -> s {_bauaReplicationGroupIds = a}) . _Default . _Coerce

-- | The unique ID of the service update
bauaServiceUpdateName :: Lens' BatchApplyUpdateAction Text
bauaServiceUpdateName = lens _bauaServiceUpdateName (\s a -> s {_bauaServiceUpdateName = a})

instance AWSRequest BatchApplyUpdateAction where
  type Rs BatchApplyUpdateAction = UpdateActionResultsMessage
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "BatchApplyUpdateActionResult"
      (\s h x -> parseXML x)

instance Hashable BatchApplyUpdateAction

instance NFData BatchApplyUpdateAction

instance ToHeaders BatchApplyUpdateAction where
  toHeaders = const mempty

instance ToPath BatchApplyUpdateAction where
  toPath = const "/"

instance ToQuery BatchApplyUpdateAction where
  toQuery BatchApplyUpdateAction' {..} =
    mconcat
      [ "Action" =: ("BatchApplyUpdateAction" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "CacheClusterIds"
          =: toQuery (toQueryList "member" <$> _bauaCacheClusterIds),
        "ReplicationGroupIds"
          =: toQuery (toQueryList "member" <$> _bauaReplicationGroupIds),
        "ServiceUpdateName" =: _bauaServiceUpdateName
      ]
