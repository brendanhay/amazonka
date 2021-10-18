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
-- Module      : Network.AWS.ElastiCache.BatchStopUpdateAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop the service update. For more information on service updates and
-- stopping them, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/stopping-self-service-updates.html Stopping Service Updates>.
module Network.AWS.ElastiCache.BatchStopUpdateAction
  ( -- * Creating a Request
    BatchStopUpdateAction (..),
    newBatchStopUpdateAction,

    -- * Request Lenses
    batchStopUpdateAction_cacheClusterIds,
    batchStopUpdateAction_replicationGroupIds,
    batchStopUpdateAction_serviceUpdateName,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchStopUpdateAction' smart constructor.
data BatchStopUpdateAction = BatchStopUpdateAction'
  { -- | The cache cluster IDs
    cacheClusterIds :: Prelude.Maybe [Prelude.Text],
    -- | The replication group IDs
    replicationGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStopUpdateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterIds', 'batchStopUpdateAction_cacheClusterIds' - The cache cluster IDs
--
-- 'replicationGroupIds', 'batchStopUpdateAction_replicationGroupIds' - The replication group IDs
--
-- 'serviceUpdateName', 'batchStopUpdateAction_serviceUpdateName' - The unique ID of the service update
newBatchStopUpdateAction ::
  -- | 'serviceUpdateName'
  Prelude.Text ->
  BatchStopUpdateAction
newBatchStopUpdateAction pServiceUpdateName_ =
  BatchStopUpdateAction'
    { cacheClusterIds =
        Prelude.Nothing,
      replicationGroupIds = Prelude.Nothing,
      serviceUpdateName = pServiceUpdateName_
    }

-- | The cache cluster IDs
batchStopUpdateAction_cacheClusterIds :: Lens.Lens' BatchStopUpdateAction (Prelude.Maybe [Prelude.Text])
batchStopUpdateAction_cacheClusterIds = Lens.lens (\BatchStopUpdateAction' {cacheClusterIds} -> cacheClusterIds) (\s@BatchStopUpdateAction' {} a -> s {cacheClusterIds = a} :: BatchStopUpdateAction) Prelude.. Lens.mapping Lens._Coerce

-- | The replication group IDs
batchStopUpdateAction_replicationGroupIds :: Lens.Lens' BatchStopUpdateAction (Prelude.Maybe [Prelude.Text])
batchStopUpdateAction_replicationGroupIds = Lens.lens (\BatchStopUpdateAction' {replicationGroupIds} -> replicationGroupIds) (\s@BatchStopUpdateAction' {} a -> s {replicationGroupIds = a} :: BatchStopUpdateAction) Prelude.. Lens.mapping Lens._Coerce

-- | The unique ID of the service update
batchStopUpdateAction_serviceUpdateName :: Lens.Lens' BatchStopUpdateAction Prelude.Text
batchStopUpdateAction_serviceUpdateName = Lens.lens (\BatchStopUpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@BatchStopUpdateAction' {} a -> s {serviceUpdateName = a} :: BatchStopUpdateAction)

instance Core.AWSRequest BatchStopUpdateAction where
  type
    AWSResponse BatchStopUpdateAction =
      UpdateActionResultsMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "BatchStopUpdateActionResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable BatchStopUpdateAction

instance Prelude.NFData BatchStopUpdateAction

instance Core.ToHeaders BatchStopUpdateAction where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath BatchStopUpdateAction where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchStopUpdateAction where
  toQuery BatchStopUpdateAction' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("BatchStopUpdateAction" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheClusterIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> cacheClusterIds
            ),
        "ReplicationGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> replicationGroupIds
            ),
        "ServiceUpdateName" Core.=: serviceUpdateName
      ]
