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
-- Module      : Network.AWS.EMR.ModifyCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of steps that can be executed concurrently for the
-- cluster specified using ClusterID.
module Network.AWS.EMR.ModifyCluster
  ( -- * Creating a Request
    ModifyCluster (..),
    newModifyCluster,

    -- * Request Lenses
    modifyCluster_stepConcurrencyLevel,
    modifyCluster_clusterId,

    -- * Destructuring the Response
    ModifyClusterResponse (..),
    newModifyClusterResponse,

    -- * Response Lenses
    modifyClusterResponse_stepConcurrencyLevel,
    modifyClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { -- | The number of steps that can be executed concurrently. You can specify a
    -- maximum of 256 steps.
    stepConcurrencyLevel :: Core.Maybe Core.Int,
    -- | The unique identifier of the cluster.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepConcurrencyLevel', 'modifyCluster_stepConcurrencyLevel' - The number of steps that can be executed concurrently. You can specify a
-- maximum of 256 steps.
--
-- 'clusterId', 'modifyCluster_clusterId' - The unique identifier of the cluster.
newModifyCluster ::
  -- | 'clusterId'
  Core.Text ->
  ModifyCluster
newModifyCluster pClusterId_ =
  ModifyCluster'
    { stepConcurrencyLevel = Core.Nothing,
      clusterId = pClusterId_
    }

-- | The number of steps that can be executed concurrently. You can specify a
-- maximum of 256 steps.
modifyCluster_stepConcurrencyLevel :: Lens.Lens' ModifyCluster (Core.Maybe Core.Int)
modifyCluster_stepConcurrencyLevel = Lens.lens (\ModifyCluster' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@ModifyCluster' {} a -> s {stepConcurrencyLevel = a} :: ModifyCluster)

-- | The unique identifier of the cluster.
modifyCluster_clusterId :: Lens.Lens' ModifyCluster Core.Text
modifyCluster_clusterId = Lens.lens (\ModifyCluster' {clusterId} -> clusterId) (\s@ModifyCluster' {} a -> s {clusterId = a} :: ModifyCluster)

instance Core.AWSRequest ModifyCluster where
  type
    AWSResponse ModifyCluster =
      ModifyClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            Core.<$> (x Core..?> "StepConcurrencyLevel")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyCluster

instance Core.NFData ModifyCluster

instance Core.ToHeaders ModifyCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ModifyCluster" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyCluster where
  toJSON ModifyCluster' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StepConcurrencyLevel" Core..=)
              Core.<$> stepConcurrencyLevel,
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ModifyCluster where
  toPath = Core.const "/"

instance Core.ToQuery ModifyCluster where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { -- | The number of steps that can be executed concurrently.
    stepConcurrencyLevel :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepConcurrencyLevel', 'modifyClusterResponse_stepConcurrencyLevel' - The number of steps that can be executed concurrently.
--
-- 'httpStatus', 'modifyClusterResponse_httpStatus' - The response's http status code.
newModifyClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyClusterResponse
newModifyClusterResponse pHttpStatus_ =
  ModifyClusterResponse'
    { stepConcurrencyLevel =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of steps that can be executed concurrently.
modifyClusterResponse_stepConcurrencyLevel :: Lens.Lens' ModifyClusterResponse (Core.Maybe Core.Int)
modifyClusterResponse_stepConcurrencyLevel = Lens.lens (\ModifyClusterResponse' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@ModifyClusterResponse' {} a -> s {stepConcurrencyLevel = a} :: ModifyClusterResponse)

-- | The response's http status code.
modifyClusterResponse_httpStatus :: Lens.Lens' ModifyClusterResponse Core.Int
modifyClusterResponse_httpStatus = Lens.lens (\ModifyClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterResponse' {} a -> s {httpStatus = a} :: ModifyClusterResponse)

instance Core.NFData ModifyClusterResponse
