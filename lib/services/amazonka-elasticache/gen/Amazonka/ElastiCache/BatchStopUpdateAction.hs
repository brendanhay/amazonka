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
-- Module      : Amazonka.ElastiCache.BatchStopUpdateAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop the service update. For more information on service updates and
-- stopping them, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/stopping-self-service-updates.html Stopping Service Updates>.
module Amazonka.ElastiCache.BatchStopUpdateAction
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
    updateActionResultsMessage_unprocessedUpdateActions,
    updateActionResultsMessage_processedUpdateActions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
batchStopUpdateAction_cacheClusterIds = Lens.lens (\BatchStopUpdateAction' {cacheClusterIds} -> cacheClusterIds) (\s@BatchStopUpdateAction' {} a -> s {cacheClusterIds = a} :: BatchStopUpdateAction) Prelude.. Lens.mapping Lens.coerced

-- | The replication group IDs
batchStopUpdateAction_replicationGroupIds :: Lens.Lens' BatchStopUpdateAction (Prelude.Maybe [Prelude.Text])
batchStopUpdateAction_replicationGroupIds = Lens.lens (\BatchStopUpdateAction' {replicationGroupIds} -> replicationGroupIds) (\s@BatchStopUpdateAction' {} a -> s {replicationGroupIds = a} :: BatchStopUpdateAction) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the service update
batchStopUpdateAction_serviceUpdateName :: Lens.Lens' BatchStopUpdateAction Prelude.Text
batchStopUpdateAction_serviceUpdateName = Lens.lens (\BatchStopUpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@BatchStopUpdateAction' {} a -> s {serviceUpdateName = a} :: BatchStopUpdateAction)

instance Core.AWSRequest BatchStopUpdateAction where
  type
    AWSResponse BatchStopUpdateAction =
      UpdateActionResultsMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "BatchStopUpdateActionResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable BatchStopUpdateAction where
  hashWithSalt _salt BatchStopUpdateAction' {..} =
    _salt `Prelude.hashWithSalt` cacheClusterIds
      `Prelude.hashWithSalt` replicationGroupIds
      `Prelude.hashWithSalt` serviceUpdateName

instance Prelude.NFData BatchStopUpdateAction where
  rnf BatchStopUpdateAction' {..} =
    Prelude.rnf cacheClusterIds
      `Prelude.seq` Prelude.rnf replicationGroupIds
      `Prelude.seq` Prelude.rnf serviceUpdateName

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
