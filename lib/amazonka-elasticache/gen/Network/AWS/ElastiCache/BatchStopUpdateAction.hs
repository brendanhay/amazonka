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
-- Module      : Network.AWS.ElastiCache.BatchStopUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop the service update. For more information on service updates and stopping them, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/stopping-self-service-updates.html Stopping Service Updates> .
module Network.AWS.ElastiCache.BatchStopUpdateAction
  ( -- * Creating a Request
    batchStopUpdateAction,
    BatchStopUpdateAction,

    -- * Request Lenses
    bsuaCacheClusterIds,
    bsuaReplicationGroupIds,
    bsuaServiceUpdateName,

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

-- | /See:/ 'batchStopUpdateAction' smart constructor.
data BatchStopUpdateAction = BatchStopUpdateAction'
  { _bsuaCacheClusterIds ::
      !(Maybe [Text]),
    _bsuaReplicationGroupIds :: !(Maybe [Text]),
    _bsuaServiceUpdateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStopUpdateAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsuaCacheClusterIds' - The cache cluster IDs
--
-- * 'bsuaReplicationGroupIds' - The replication group IDs
--
-- * 'bsuaServiceUpdateName' - The unique ID of the service update
batchStopUpdateAction ::
  -- | 'bsuaServiceUpdateName'
  Text ->
  BatchStopUpdateAction
batchStopUpdateAction pServiceUpdateName_ =
  BatchStopUpdateAction'
    { _bsuaCacheClusterIds = Nothing,
      _bsuaReplicationGroupIds = Nothing,
      _bsuaServiceUpdateName = pServiceUpdateName_
    }

-- | The cache cluster IDs
bsuaCacheClusterIds :: Lens' BatchStopUpdateAction [Text]
bsuaCacheClusterIds = lens _bsuaCacheClusterIds (\s a -> s {_bsuaCacheClusterIds = a}) . _Default . _Coerce

-- | The replication group IDs
bsuaReplicationGroupIds :: Lens' BatchStopUpdateAction [Text]
bsuaReplicationGroupIds = lens _bsuaReplicationGroupIds (\s a -> s {_bsuaReplicationGroupIds = a}) . _Default . _Coerce

-- | The unique ID of the service update
bsuaServiceUpdateName :: Lens' BatchStopUpdateAction Text
bsuaServiceUpdateName = lens _bsuaServiceUpdateName (\s a -> s {_bsuaServiceUpdateName = a})

instance AWSRequest BatchStopUpdateAction where
  type Rs BatchStopUpdateAction = UpdateActionResultsMessage
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "BatchStopUpdateActionResult"
      (\s h x -> parseXML x)

instance Hashable BatchStopUpdateAction

instance NFData BatchStopUpdateAction

instance ToHeaders BatchStopUpdateAction where
  toHeaders = const mempty

instance ToPath BatchStopUpdateAction where
  toPath = const "/"

instance ToQuery BatchStopUpdateAction where
  toQuery BatchStopUpdateAction' {..} =
    mconcat
      [ "Action" =: ("BatchStopUpdateAction" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "CacheClusterIds"
          =: toQuery (toQueryList "member" <$> _bsuaCacheClusterIds),
        "ReplicationGroupIds"
          =: toQuery (toQueryList "member" <$> _bsuaReplicationGroupIds),
        "ServiceUpdateName" =: _bsuaServiceUpdateName
      ]
