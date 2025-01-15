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
-- Module      : Amazonka.SageMakerEdge.GetDeployments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use to get the active deployments from a device.
module Amazonka.SageMakerEdge.GetDeployments
  ( -- * Creating a Request
    GetDeployments (..),
    newGetDeployments,

    -- * Request Lenses
    getDeployments_deviceName,
    getDeployments_deviceFleetName,

    -- * Destructuring the Response
    GetDeploymentsResponse (..),
    newGetDeploymentsResponse,

    -- * Response Lenses
    getDeploymentsResponse_deployments,
    getDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerEdge.Types

-- | /See:/ 'newGetDeployments' smart constructor.
data GetDeployments = GetDeployments'
  { -- | The unique name of the device you want to get the configuration of
    -- active deployments from.
    deviceName :: Prelude.Text,
    -- | The name of the fleet that the device belongs to.
    deviceFleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'getDeployments_deviceName' - The unique name of the device you want to get the configuration of
-- active deployments from.
--
-- 'deviceFleetName', 'getDeployments_deviceFleetName' - The name of the fleet that the device belongs to.
newGetDeployments ::
  -- | 'deviceName'
  Prelude.Text ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  GetDeployments
newGetDeployments pDeviceName_ pDeviceFleetName_ =
  GetDeployments'
    { deviceName = pDeviceName_,
      deviceFleetName = pDeviceFleetName_
    }

-- | The unique name of the device you want to get the configuration of
-- active deployments from.
getDeployments_deviceName :: Lens.Lens' GetDeployments Prelude.Text
getDeployments_deviceName = Lens.lens (\GetDeployments' {deviceName} -> deviceName) (\s@GetDeployments' {} a -> s {deviceName = a} :: GetDeployments)

-- | The name of the fleet that the device belongs to.
getDeployments_deviceFleetName :: Lens.Lens' GetDeployments Prelude.Text
getDeployments_deviceFleetName = Lens.lens (\GetDeployments' {deviceFleetName} -> deviceFleetName) (\s@GetDeployments' {} a -> s {deviceFleetName = a} :: GetDeployments)

instance Core.AWSRequest GetDeployments where
  type
    AWSResponse GetDeployments =
      GetDeploymentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentsResponse'
            Prelude.<$> (x Data..?> "Deployments" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployments where
  hashWithSalt _salt GetDeployments' {..} =
    _salt
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` deviceFleetName

instance Prelude.NFData GetDeployments where
  rnf GetDeployments' {..} =
    Prelude.rnf deviceName `Prelude.seq`
      Prelude.rnf deviceFleetName

instance Data.ToHeaders GetDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDeployments where
  toJSON GetDeployments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DeviceName" Data..= deviceName),
            Prelude.Just
              ("DeviceFleetName" Data..= deviceFleetName)
          ]
      )

instance Data.ToPath GetDeployments where
  toPath = Prelude.const "/GetDeployments"

instance Data.ToQuery GetDeployments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeploymentsResponse' smart constructor.
data GetDeploymentsResponse = GetDeploymentsResponse'
  { -- | Returns a list of the configurations of the active deployments on the
    -- device.
    deployments :: Prelude.Maybe [EdgeDeployment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployments', 'getDeploymentsResponse_deployments' - Returns a list of the configurations of the active deployments on the
-- device.
--
-- 'httpStatus', 'getDeploymentsResponse_httpStatus' - The response's http status code.
newGetDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeploymentsResponse
newGetDeploymentsResponse pHttpStatus_ =
  GetDeploymentsResponse'
    { deployments =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of the configurations of the active deployments on the
-- device.
getDeploymentsResponse_deployments :: Lens.Lens' GetDeploymentsResponse (Prelude.Maybe [EdgeDeployment])
getDeploymentsResponse_deployments = Lens.lens (\GetDeploymentsResponse' {deployments} -> deployments) (\s@GetDeploymentsResponse' {} a -> s {deployments = a} :: GetDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDeploymentsResponse_httpStatus :: Lens.Lens' GetDeploymentsResponse Prelude.Int
getDeploymentsResponse_httpStatus = Lens.lens (\GetDeploymentsResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentsResponse' {} a -> s {httpStatus = a} :: GetDeploymentsResponse)

instance Prelude.NFData GetDeploymentsResponse where
  rnf GetDeploymentsResponse' {..} =
    Prelude.rnf deployments `Prelude.seq`
      Prelude.rnf httpStatus
