{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchApplyUpdateAction' smart constructor.
data BatchApplyUpdateAction = BatchApplyUpdateAction'
  { -- | The cache cluster IDs
    cacheClusterIds :: Prelude.Maybe [Prelude.Text],
    -- | The replication group IDs
    replicationGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  BatchApplyUpdateAction
newBatchApplyUpdateAction pServiceUpdateName_ =
  BatchApplyUpdateAction'
    { cacheClusterIds =
        Prelude.Nothing,
      replicationGroupIds = Prelude.Nothing,
      serviceUpdateName = pServiceUpdateName_
    }

-- | The cache cluster IDs
batchApplyUpdateAction_cacheClusterIds :: Lens.Lens' BatchApplyUpdateAction (Prelude.Maybe [Prelude.Text])
batchApplyUpdateAction_cacheClusterIds = Lens.lens (\BatchApplyUpdateAction' {cacheClusterIds} -> cacheClusterIds) (\s@BatchApplyUpdateAction' {} a -> s {cacheClusterIds = a} :: BatchApplyUpdateAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The replication group IDs
batchApplyUpdateAction_replicationGroupIds :: Lens.Lens' BatchApplyUpdateAction (Prelude.Maybe [Prelude.Text])
batchApplyUpdateAction_replicationGroupIds = Lens.lens (\BatchApplyUpdateAction' {replicationGroupIds} -> replicationGroupIds) (\s@BatchApplyUpdateAction' {} a -> s {replicationGroupIds = a} :: BatchApplyUpdateAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique ID of the service update
batchApplyUpdateAction_serviceUpdateName :: Lens.Lens' BatchApplyUpdateAction Prelude.Text
batchApplyUpdateAction_serviceUpdateName = Lens.lens (\BatchApplyUpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@BatchApplyUpdateAction' {} a -> s {serviceUpdateName = a} :: BatchApplyUpdateAction)

instance Prelude.AWSRequest BatchApplyUpdateAction where
  type
    Rs BatchApplyUpdateAction =
      UpdateActionResultsMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "BatchApplyUpdateActionResult"
      (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable BatchApplyUpdateAction

instance Prelude.NFData BatchApplyUpdateAction

instance Prelude.ToHeaders BatchApplyUpdateAction where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath BatchApplyUpdateAction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchApplyUpdateAction where
  toQuery BatchApplyUpdateAction' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("BatchApplyUpdateAction" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheClusterIds"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> cacheClusterIds
            ),
        "ReplicationGroupIds"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> replicationGroupIds
            ),
        "ServiceUpdateName" Prelude.=: serviceUpdateName
      ]
