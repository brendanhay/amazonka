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
-- Module      : Network.AWS.ECS.UpdateServicePrimaryTaskSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies which task set in a service is the primary task set. Any
-- parameters that are updated on the primary task set in a service will
-- transition to the service. This is used when a service uses the
-- @EXTERNAL@ deployment controller type. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.UpdateServicePrimaryTaskSet
  ( -- * Creating a Request
    UpdateServicePrimaryTaskSet (..),
    newUpdateServicePrimaryTaskSet,

    -- * Request Lenses
    updateServicePrimaryTaskSet_cluster,
    updateServicePrimaryTaskSet_service,
    updateServicePrimaryTaskSet_primaryTaskSet,

    -- * Destructuring the Response
    UpdateServicePrimaryTaskSetResponse (..),
    newUpdateServicePrimaryTaskSetResponse,

    -- * Response Lenses
    updateServicePrimaryTaskSetResponse_taskSet,
    updateServicePrimaryTaskSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateServicePrimaryTaskSet' smart constructor.
data UpdateServicePrimaryTaskSet = UpdateServicePrimaryTaskSet'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service that the task set exists in.
    cluster :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service that
    -- the task set exists in.
    service :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the task set to set
    -- as the primary task set in the deployment.
    primaryTaskSet :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServicePrimaryTaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'updateServicePrimaryTaskSet_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task set exists in.
--
-- 'service', 'updateServicePrimaryTaskSet_service' - The short name or full Amazon Resource Name (ARN) of the service that
-- the task set exists in.
--
-- 'primaryTaskSet', 'updateServicePrimaryTaskSet_primaryTaskSet' - The short name or full Amazon Resource Name (ARN) of the task set to set
-- as the primary task set in the deployment.
newUpdateServicePrimaryTaskSet ::
  -- | 'cluster'
  Prelude.Text ->
  -- | 'service'
  Prelude.Text ->
  -- | 'primaryTaskSet'
  Prelude.Text ->
  UpdateServicePrimaryTaskSet
newUpdateServicePrimaryTaskSet
  pCluster_
  pService_
  pPrimaryTaskSet_ =
    UpdateServicePrimaryTaskSet'
      { cluster = pCluster_,
        service = pService_,
        primaryTaskSet = pPrimaryTaskSet_
      }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task set exists in.
updateServicePrimaryTaskSet_cluster :: Lens.Lens' UpdateServicePrimaryTaskSet Prelude.Text
updateServicePrimaryTaskSet_cluster = Lens.lens (\UpdateServicePrimaryTaskSet' {cluster} -> cluster) (\s@UpdateServicePrimaryTaskSet' {} a -> s {cluster = a} :: UpdateServicePrimaryTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the service that
-- the task set exists in.
updateServicePrimaryTaskSet_service :: Lens.Lens' UpdateServicePrimaryTaskSet Prelude.Text
updateServicePrimaryTaskSet_service = Lens.lens (\UpdateServicePrimaryTaskSet' {service} -> service) (\s@UpdateServicePrimaryTaskSet' {} a -> s {service = a} :: UpdateServicePrimaryTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the task set to set
-- as the primary task set in the deployment.
updateServicePrimaryTaskSet_primaryTaskSet :: Lens.Lens' UpdateServicePrimaryTaskSet Prelude.Text
updateServicePrimaryTaskSet_primaryTaskSet = Lens.lens (\UpdateServicePrimaryTaskSet' {primaryTaskSet} -> primaryTaskSet) (\s@UpdateServicePrimaryTaskSet' {} a -> s {primaryTaskSet = a} :: UpdateServicePrimaryTaskSet)

instance Core.AWSRequest UpdateServicePrimaryTaskSet where
  type
    AWSResponse UpdateServicePrimaryTaskSet =
      UpdateServicePrimaryTaskSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServicePrimaryTaskSetResponse'
            Prelude.<$> (x Core..?> "taskSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServicePrimaryTaskSet

instance Prelude.NFData UpdateServicePrimaryTaskSet

instance Core.ToHeaders UpdateServicePrimaryTaskSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.UpdateServicePrimaryTaskSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateServicePrimaryTaskSet where
  toJSON UpdateServicePrimaryTaskSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("cluster" Core..= cluster),
            Prelude.Just ("service" Core..= service),
            Prelude.Just
              ("primaryTaskSet" Core..= primaryTaskSet)
          ]
      )

instance Core.ToPath UpdateServicePrimaryTaskSet where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateServicePrimaryTaskSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServicePrimaryTaskSetResponse' smart constructor.
data UpdateServicePrimaryTaskSetResponse = UpdateServicePrimaryTaskSetResponse'
  { taskSet :: Prelude.Maybe TaskSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServicePrimaryTaskSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskSet', 'updateServicePrimaryTaskSetResponse_taskSet' - Undocumented member.
--
-- 'httpStatus', 'updateServicePrimaryTaskSetResponse_httpStatus' - The response's http status code.
newUpdateServicePrimaryTaskSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServicePrimaryTaskSetResponse
newUpdateServicePrimaryTaskSetResponse pHttpStatus_ =
  UpdateServicePrimaryTaskSetResponse'
    { taskSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateServicePrimaryTaskSetResponse_taskSet :: Lens.Lens' UpdateServicePrimaryTaskSetResponse (Prelude.Maybe TaskSet)
updateServicePrimaryTaskSetResponse_taskSet = Lens.lens (\UpdateServicePrimaryTaskSetResponse' {taskSet} -> taskSet) (\s@UpdateServicePrimaryTaskSetResponse' {} a -> s {taskSet = a} :: UpdateServicePrimaryTaskSetResponse)

-- | The response's http status code.
updateServicePrimaryTaskSetResponse_httpStatus :: Lens.Lens' UpdateServicePrimaryTaskSetResponse Prelude.Int
updateServicePrimaryTaskSetResponse_httpStatus = Lens.lens (\UpdateServicePrimaryTaskSetResponse' {httpStatus} -> httpStatus) (\s@UpdateServicePrimaryTaskSetResponse' {} a -> s {httpStatus = a} :: UpdateServicePrimaryTaskSetResponse)

instance
  Prelude.NFData
    UpdateServicePrimaryTaskSetResponse
