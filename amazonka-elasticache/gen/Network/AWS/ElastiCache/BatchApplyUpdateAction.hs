{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.BatchApplyUpdateAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Apply the service update. For more information on service updates and
-- applying them, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/applying-updates.html Applying Service Updates>.
module Network.AWS.ElastiCache.BatchApplyUpdateAction
  ( -- * Creating a Request
    BatchApplyUpdateAction (..),
    newBatchApplyUpdateAction,

    -- * Request Lenses
    batchApplyUpdateAction_cacheClusterIds,
    batchApplyUpdateAction_replicationGroupIds,
    batchApplyUpdateAction_serviceUpdateName,

    -- * Destructuring the Response
    UpdateActionResultsMessage (..),
    newUpdateActionResultsMessage,

    -- * Response Lenses
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchApplyUpdateAction' smart constructor.
data BatchApplyUpdateAction = BatchApplyUpdateAction'
  { -- | The cache cluster IDs
    cacheClusterIds :: Core.Maybe [Core.Text],
    -- | The replication group IDs
    replicationGroupIds :: Core.Maybe [Core.Text],
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchApplyUpdateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterIds', 'batchApplyUpdateAction_cacheClusterIds' - The cache cluster IDs
--
-- 'replicationGroupIds', 'batchApplyUpdateAction_replicationGroupIds' - The replication group IDs
--
-- 'serviceUpdateName', 'batchApplyUpdateAction_serviceUpdateName' - The unique ID of the service update
newBatchApplyUpdateAction ::
  -- | 'serviceUpdateName'
  Core.Text ->
  BatchApplyUpdateAction
newBatchApplyUpdateAction pServiceUpdateName_ =
  BatchApplyUpdateAction'
    { cacheClusterIds =
        Core.Nothing,
      replicationGroupIds = Core.Nothing,
      serviceUpdateName = pServiceUpdateName_
    }

-- | The cache cluster IDs
batchApplyUpdateAction_cacheClusterIds :: Lens.Lens' BatchApplyUpdateAction (Core.Maybe [Core.Text])
batchApplyUpdateAction_cacheClusterIds = Lens.lens (\BatchApplyUpdateAction' {cacheClusterIds} -> cacheClusterIds) (\s@BatchApplyUpdateAction' {} a -> s {cacheClusterIds = a} :: BatchApplyUpdateAction) Core.. Lens.mapping Lens._Coerce

-- | The replication group IDs
batchApplyUpdateAction_replicationGroupIds :: Lens.Lens' BatchApplyUpdateAction (Core.Maybe [Core.Text])
batchApplyUpdateAction_replicationGroupIds = Lens.lens (\BatchApplyUpdateAction' {replicationGroupIds} -> replicationGroupIds) (\s@BatchApplyUpdateAction' {} a -> s {replicationGroupIds = a} :: BatchApplyUpdateAction) Core.. Lens.mapping Lens._Coerce

-- | The unique ID of the service update
batchApplyUpdateAction_serviceUpdateName :: Lens.Lens' BatchApplyUpdateAction Core.Text
batchApplyUpdateAction_serviceUpdateName = Lens.lens (\BatchApplyUpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@BatchApplyUpdateAction' {} a -> s {serviceUpdateName = a} :: BatchApplyUpdateAction)

instance Core.AWSRequest BatchApplyUpdateAction where
  type
    AWSResponse BatchApplyUpdateAction =
      UpdateActionResultsMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "BatchApplyUpdateActionResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable BatchApplyUpdateAction

instance Core.NFData BatchApplyUpdateAction

instance Core.ToHeaders BatchApplyUpdateAction where
  toHeaders = Core.const Core.mempty

instance Core.ToPath BatchApplyUpdateAction where
  toPath = Core.const "/"

instance Core.ToQuery BatchApplyUpdateAction where
  toQuery BatchApplyUpdateAction' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("BatchApplyUpdateAction" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "CacheClusterIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> cacheClusterIds),
        "ReplicationGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> replicationGroupIds
            ),
        "ServiceUpdateName" Core.=: serviceUpdateName
      ]
