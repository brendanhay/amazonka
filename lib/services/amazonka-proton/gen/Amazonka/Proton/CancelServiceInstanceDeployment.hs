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
-- Module      : Amazonka.Proton.CancelServiceInstanceDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel a service instance deployment on an
-- UpdateServiceInstance action, if the deployment is @IN_PROGRESS@. For
-- more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-svc-instance-update.html Update a service instance>
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
-- -   If the current UpdateServiceInstance action succeeds before the
--     cancellation attempt starts, the resulting deployment state is
--     @SUCCEEDED@ and the cancellation attempt has no effect.
module Amazonka.Proton.CancelServiceInstanceDeployment
  ( -- * Creating a Request
    CancelServiceInstanceDeployment (..),
    newCancelServiceInstanceDeployment,

    -- * Request Lenses
    cancelServiceInstanceDeployment_serviceInstanceName,
    cancelServiceInstanceDeployment_serviceName,

    -- * Destructuring the Response
    CancelServiceInstanceDeploymentResponse (..),
    newCancelServiceInstanceDeploymentResponse,

    -- * Response Lenses
    cancelServiceInstanceDeploymentResponse_httpStatus,
    cancelServiceInstanceDeploymentResponse_serviceInstance,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelServiceInstanceDeployment' smart constructor.
data CancelServiceInstanceDeployment = CancelServiceInstanceDeployment'
  { -- | The name of the service instance with the deployment to cancel.
    serviceInstanceName :: Prelude.Text,
    -- | The name of the service with the service instance deployment to cancel.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelServiceInstanceDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceInstanceName', 'cancelServiceInstanceDeployment_serviceInstanceName' - The name of the service instance with the deployment to cancel.
--
-- 'serviceName', 'cancelServiceInstanceDeployment_serviceName' - The name of the service with the service instance deployment to cancel.
newCancelServiceInstanceDeployment ::
  -- | 'serviceInstanceName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  CancelServiceInstanceDeployment
newCancelServiceInstanceDeployment
  pServiceInstanceName_
  pServiceName_ =
    CancelServiceInstanceDeployment'
      { serviceInstanceName =
          pServiceInstanceName_,
        serviceName = pServiceName_
      }

-- | The name of the service instance with the deployment to cancel.
cancelServiceInstanceDeployment_serviceInstanceName :: Lens.Lens' CancelServiceInstanceDeployment Prelude.Text
cancelServiceInstanceDeployment_serviceInstanceName = Lens.lens (\CancelServiceInstanceDeployment' {serviceInstanceName} -> serviceInstanceName) (\s@CancelServiceInstanceDeployment' {} a -> s {serviceInstanceName = a} :: CancelServiceInstanceDeployment)

-- | The name of the service with the service instance deployment to cancel.
cancelServiceInstanceDeployment_serviceName :: Lens.Lens' CancelServiceInstanceDeployment Prelude.Text
cancelServiceInstanceDeployment_serviceName = Lens.lens (\CancelServiceInstanceDeployment' {serviceName} -> serviceName) (\s@CancelServiceInstanceDeployment' {} a -> s {serviceName = a} :: CancelServiceInstanceDeployment)

instance
  Core.AWSRequest
    CancelServiceInstanceDeployment
  where
  type
    AWSResponse CancelServiceInstanceDeployment =
      CancelServiceInstanceDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelServiceInstanceDeploymentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "serviceInstance")
      )

instance
  Prelude.Hashable
    CancelServiceInstanceDeployment
  where
  hashWithSalt
    _salt
    CancelServiceInstanceDeployment' {..} =
      _salt
        `Prelude.hashWithSalt` serviceInstanceName
        `Prelude.hashWithSalt` serviceName

instance
  Prelude.NFData
    CancelServiceInstanceDeployment
  where
  rnf CancelServiceInstanceDeployment' {..} =
    Prelude.rnf serviceInstanceName
      `Prelude.seq` Prelude.rnf serviceName

instance
  Data.ToHeaders
    CancelServiceInstanceDeployment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CancelServiceInstanceDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelServiceInstanceDeployment where
  toJSON CancelServiceInstanceDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("serviceInstanceName" Data..= serviceInstanceName),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath CancelServiceInstanceDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelServiceInstanceDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelServiceInstanceDeploymentResponse' smart constructor.
data CancelServiceInstanceDeploymentResponse = CancelServiceInstanceDeploymentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service instance summary data that\'s returned by Proton.
    serviceInstance :: ServiceInstance
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelServiceInstanceDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelServiceInstanceDeploymentResponse_httpStatus' - The response's http status code.
--
-- 'serviceInstance', 'cancelServiceInstanceDeploymentResponse_serviceInstance' - The service instance summary data that\'s returned by Proton.
newCancelServiceInstanceDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceInstance'
  ServiceInstance ->
  CancelServiceInstanceDeploymentResponse
newCancelServiceInstanceDeploymentResponse
  pHttpStatus_
  pServiceInstance_ =
    CancelServiceInstanceDeploymentResponse'
      { httpStatus =
          pHttpStatus_,
        serviceInstance =
          pServiceInstance_
      }

-- | The response's http status code.
cancelServiceInstanceDeploymentResponse_httpStatus :: Lens.Lens' CancelServiceInstanceDeploymentResponse Prelude.Int
cancelServiceInstanceDeploymentResponse_httpStatus = Lens.lens (\CancelServiceInstanceDeploymentResponse' {httpStatus} -> httpStatus) (\s@CancelServiceInstanceDeploymentResponse' {} a -> s {httpStatus = a} :: CancelServiceInstanceDeploymentResponse)

-- | The service instance summary data that\'s returned by Proton.
cancelServiceInstanceDeploymentResponse_serviceInstance :: Lens.Lens' CancelServiceInstanceDeploymentResponse ServiceInstance
cancelServiceInstanceDeploymentResponse_serviceInstance = Lens.lens (\CancelServiceInstanceDeploymentResponse' {serviceInstance} -> serviceInstance) (\s@CancelServiceInstanceDeploymentResponse' {} a -> s {serviceInstance = a} :: CancelServiceInstanceDeploymentResponse)

instance
  Prelude.NFData
    CancelServiceInstanceDeploymentResponse
  where
  rnf CancelServiceInstanceDeploymentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceInstance
