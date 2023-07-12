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
-- Module      : Amazonka.EMR.ModifyCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of steps that can be executed concurrently for the
-- cluster specified using ClusterID.
module Amazonka.EMR.ModifyCluster
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { -- | The number of steps that can be executed concurrently. You can specify a
    -- minimum of 1 step and a maximum of 256 steps. We recommend that you do
    -- not change this parameter while steps are running or the
    -- @ActionOnFailure@ setting may not behave as expected. For more
    -- information see Step$ActionOnFailure.
    stepConcurrencyLevel :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the cluster.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepConcurrencyLevel', 'modifyCluster_stepConcurrencyLevel' - The number of steps that can be executed concurrently. You can specify a
-- minimum of 1 step and a maximum of 256 steps. We recommend that you do
-- not change this parameter while steps are running or the
-- @ActionOnFailure@ setting may not behave as expected. For more
-- information see Step$ActionOnFailure.
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
-- minimum of 1 step and a maximum of 256 steps. We recommend that you do
-- not change this parameter while steps are running or the
-- @ActionOnFailure@ setting may not behave as expected. For more
-- information see Step$ActionOnFailure.
modifyCluster_stepConcurrencyLevel :: Lens.Lens' ModifyCluster (Prelude.Maybe Prelude.Int)
modifyCluster_stepConcurrencyLevel = Lens.lens (\ModifyCluster' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@ModifyCluster' {} a -> s {stepConcurrencyLevel = a} :: ModifyCluster)

-- | The unique identifier of the cluster.
modifyCluster_clusterId :: Lens.Lens' ModifyCluster Prelude.Text
modifyCluster_clusterId = Lens.lens (\ModifyCluster' {clusterId} -> clusterId) (\s@ModifyCluster' {} a -> s {clusterId = a} :: ModifyCluster)

instance Core.AWSRequest ModifyCluster where
  type
    AWSResponse ModifyCluster =
      ModifyClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            Prelude.<$> (x Data..?> "StepConcurrencyLevel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCluster where
  hashWithSalt _salt ModifyCluster' {..} =
    _salt
      `Prelude.hashWithSalt` stepConcurrencyLevel
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData ModifyCluster where
  rnf ModifyCluster' {..} =
    Prelude.rnf stepConcurrencyLevel
      `Prelude.seq` Prelude.rnf clusterId

instance Data.ToHeaders ModifyCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.ModifyCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyCluster where
  toJSON ModifyCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StepConcurrencyLevel" Data..=)
              Prelude.<$> stepConcurrencyLevel,
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath ModifyCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { -- | The number of steps that can be executed concurrently.
    stepConcurrencyLevel :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData ModifyClusterResponse where
  rnf ModifyClusterResponse' {..} =
    Prelude.rnf stepConcurrencyLevel
      `Prelude.seq` Prelude.rnf httpStatus
