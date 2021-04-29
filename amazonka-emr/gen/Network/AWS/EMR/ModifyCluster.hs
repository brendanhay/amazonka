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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { -- | The number of steps that can be executed concurrently. You can specify a
    -- maximum of 256 steps.
    stepConcurrencyLevel :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the cluster.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ModifyCluster
newModifyCluster pClusterId_ =
  ModifyCluster'
    { stepConcurrencyLevel =
        Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The number of steps that can be executed concurrently. You can specify a
-- maximum of 256 steps.
modifyCluster_stepConcurrencyLevel :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Int)
modifyCluster_stepConcurrencyLevel = Lens.lens (\ModifyCluster' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@ModifyCluster' {} a -> s {stepConcurrencyLevel = a} :: ModifyCluster)

-- | The unique identifier of the cluster.
modifyCluster_clusterId :: Lens.Lens' ModifyCluster Prelude.Text
modifyCluster_clusterId = Lens.lens (\ModifyCluster' {clusterId} -> clusterId) (\s@ModifyCluster' {} a -> s {clusterId = a} :: ModifyCluster)

instance Prelude.AWSRequest ModifyCluster where
  type Rs ModifyCluster = ModifyClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            Prelude.<$> (x Prelude..?> "StepConcurrencyLevel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCluster

instance Prelude.NFData ModifyCluster

instance Prelude.ToHeaders ModifyCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.ModifyCluster" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ModifyCluster where
  toJSON ModifyCluster' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StepConcurrencyLevel" Prelude..=)
              Prelude.<$> stepConcurrencyLevel,
            Prelude.Just ("ClusterId" Prelude..= clusterId)
          ]
      )

instance Prelude.ToPath ModifyCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { -- | The number of steps that can be executed concurrently.
    stepConcurrencyLevel :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ModifyClusterResponse
newModifyClusterResponse pHttpStatus_ =
  ModifyClusterResponse'
    { stepConcurrencyLevel =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of steps that can be executed concurrently.
modifyClusterResponse_stepConcurrencyLevel :: Lens.Lens' ModifyClusterResponse (Prelude.Maybe Prelude.Int)
modifyClusterResponse_stepConcurrencyLevel = Lens.lens (\ModifyClusterResponse' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@ModifyClusterResponse' {} a -> s {stepConcurrencyLevel = a} :: ModifyClusterResponse)

-- | The response's http status code.
modifyClusterResponse_httpStatus :: Lens.Lens' ModifyClusterResponse Prelude.Int
modifyClusterResponse_httpStatus = Lens.lens (\ModifyClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterResponse' {} a -> s {httpStatus = a} :: ModifyClusterResponse)

instance Prelude.NFData ModifyClusterResponse
