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
-- Module      : Amazonka.Proton.CancelServicePipelineDeployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel a service pipeline deployment on an
-- UpdateServicePipeline action, if the deployment is @IN_PROGRESS@. For
-- more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-svc-pipeline-update.html Update a service pipeline>
-- in the /Proton User guide/.
--
-- The following list includes potential cancellation scenarios.
--
-- -   If the cancellation attempt succeeds, the resulting deployment state
--     is @CANCELLED@.
--
-- -   If the cancellation attempt fails, the resulting deployment state is
--     @FAILED@.
--
-- -   If the current UpdateServicePipeline action succeeds before the
--     cancellation attempt starts, the resulting deployment state is
--     @SUCCEEDED@ and the cancellation attempt has no effect.
module Amazonka.Proton.CancelServicePipelineDeployment
  ( -- * Creating a Request
    CancelServicePipelineDeployment (..),
    newCancelServicePipelineDeployment,

    -- * Request Lenses
    cancelServicePipelineDeployment_serviceName,

    -- * Destructuring the Response
    CancelServicePipelineDeploymentResponse (..),
    newCancelServicePipelineDeploymentResponse,

    -- * Response Lenses
    cancelServicePipelineDeploymentResponse_httpStatus,
    cancelServicePipelineDeploymentResponse_pipeline,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelServicePipelineDeployment' smart constructor.
data CancelServicePipelineDeployment = CancelServicePipelineDeployment'
  { -- | The name of the service with the service pipeline deployment to cancel.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelServicePipelineDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'cancelServicePipelineDeployment_serviceName' - The name of the service with the service pipeline deployment to cancel.
newCancelServicePipelineDeployment ::
  -- | 'serviceName'
  Prelude.Text ->
  CancelServicePipelineDeployment
newCancelServicePipelineDeployment pServiceName_ =
  CancelServicePipelineDeployment'
    { serviceName =
        pServiceName_
    }

-- | The name of the service with the service pipeline deployment to cancel.
cancelServicePipelineDeployment_serviceName :: Lens.Lens' CancelServicePipelineDeployment Prelude.Text
cancelServicePipelineDeployment_serviceName = Lens.lens (\CancelServicePipelineDeployment' {serviceName} -> serviceName) (\s@CancelServicePipelineDeployment' {} a -> s {serviceName = a} :: CancelServicePipelineDeployment)

instance
  Core.AWSRequest
    CancelServicePipelineDeployment
  where
  type
    AWSResponse CancelServicePipelineDeployment =
      CancelServicePipelineDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelServicePipelineDeploymentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "pipeline")
      )

instance
  Prelude.Hashable
    CancelServicePipelineDeployment
  where
  hashWithSalt
    _salt
    CancelServicePipelineDeployment' {..} =
      _salt `Prelude.hashWithSalt` serviceName

instance
  Prelude.NFData
    CancelServicePipelineDeployment
  where
  rnf CancelServicePipelineDeployment' {..} =
    Prelude.rnf serviceName

instance
  Core.ToHeaders
    CancelServicePipelineDeployment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.CancelServicePipelineDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelServicePipelineDeployment where
  toJSON CancelServicePipelineDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("serviceName" Core..= serviceName)]
      )

instance Core.ToPath CancelServicePipelineDeployment where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelServicePipelineDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelServicePipelineDeploymentResponse' smart constructor.
data CancelServicePipelineDeploymentResponse = CancelServicePipelineDeploymentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service pipeline detail data that\'s returned by Proton.
    pipeline :: ServicePipeline
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelServicePipelineDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelServicePipelineDeploymentResponse_httpStatus' - The response's http status code.
--
-- 'pipeline', 'cancelServicePipelineDeploymentResponse_pipeline' - The service pipeline detail data that\'s returned by Proton.
newCancelServicePipelineDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'pipeline'
  ServicePipeline ->
  CancelServicePipelineDeploymentResponse
newCancelServicePipelineDeploymentResponse
  pHttpStatus_
  pPipeline_ =
    CancelServicePipelineDeploymentResponse'
      { httpStatus =
          pHttpStatus_,
        pipeline = pPipeline_
      }

-- | The response's http status code.
cancelServicePipelineDeploymentResponse_httpStatus :: Lens.Lens' CancelServicePipelineDeploymentResponse Prelude.Int
cancelServicePipelineDeploymentResponse_httpStatus = Lens.lens (\CancelServicePipelineDeploymentResponse' {httpStatus} -> httpStatus) (\s@CancelServicePipelineDeploymentResponse' {} a -> s {httpStatus = a} :: CancelServicePipelineDeploymentResponse)

-- | The service pipeline detail data that\'s returned by Proton.
cancelServicePipelineDeploymentResponse_pipeline :: Lens.Lens' CancelServicePipelineDeploymentResponse ServicePipeline
cancelServicePipelineDeploymentResponse_pipeline = Lens.lens (\CancelServicePipelineDeploymentResponse' {pipeline} -> pipeline) (\s@CancelServicePipelineDeploymentResponse' {} a -> s {pipeline = a} :: CancelServicePipelineDeploymentResponse)

instance
  Prelude.NFData
    CancelServicePipelineDeploymentResponse
  where
  rnf CancelServicePipelineDeploymentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pipeline
