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
-- Module      : Amazonka.WorkLink.UpdateDevicePolicyConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device policy configuration for the fleet.
module Amazonka.WorkLink.UpdateDevicePolicyConfiguration
  ( -- * Creating a Request
    UpdateDevicePolicyConfiguration (..),
    newUpdateDevicePolicyConfiguration,

    -- * Request Lenses
    updateDevicePolicyConfiguration_deviceCaCertificate,
    updateDevicePolicyConfiguration_fleetArn,

    -- * Destructuring the Response
    UpdateDevicePolicyConfigurationResponse (..),
    newUpdateDevicePolicyConfigurationResponse,

    -- * Response Lenses
    updateDevicePolicyConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newUpdateDevicePolicyConfiguration' smart constructor.
data UpdateDevicePolicyConfiguration = UpdateDevicePolicyConfiguration'
  { -- | The certificate chain, including intermediate certificates and the root
    -- certificate authority certificate used to issue device certificates.
    deviceCaCertificate :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevicePolicyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceCaCertificate', 'updateDevicePolicyConfiguration_deviceCaCertificate' - The certificate chain, including intermediate certificates and the root
-- certificate authority certificate used to issue device certificates.
--
-- 'fleetArn', 'updateDevicePolicyConfiguration_fleetArn' - The ARN of the fleet.
newUpdateDevicePolicyConfiguration ::
  -- | 'fleetArn'
  Prelude.Text ->
  UpdateDevicePolicyConfiguration
newUpdateDevicePolicyConfiguration pFleetArn_ =
  UpdateDevicePolicyConfiguration'
    { deviceCaCertificate =
        Prelude.Nothing,
      fleetArn = pFleetArn_
    }

-- | The certificate chain, including intermediate certificates and the root
-- certificate authority certificate used to issue device certificates.
updateDevicePolicyConfiguration_deviceCaCertificate :: Lens.Lens' UpdateDevicePolicyConfiguration (Prelude.Maybe Prelude.Text)
updateDevicePolicyConfiguration_deviceCaCertificate = Lens.lens (\UpdateDevicePolicyConfiguration' {deviceCaCertificate} -> deviceCaCertificate) (\s@UpdateDevicePolicyConfiguration' {} a -> s {deviceCaCertificate = a} :: UpdateDevicePolicyConfiguration)

-- | The ARN of the fleet.
updateDevicePolicyConfiguration_fleetArn :: Lens.Lens' UpdateDevicePolicyConfiguration Prelude.Text
updateDevicePolicyConfiguration_fleetArn = Lens.lens (\UpdateDevicePolicyConfiguration' {fleetArn} -> fleetArn) (\s@UpdateDevicePolicyConfiguration' {} a -> s {fleetArn = a} :: UpdateDevicePolicyConfiguration)

instance
  Core.AWSRequest
    UpdateDevicePolicyConfiguration
  where
  type
    AWSResponse UpdateDevicePolicyConfiguration =
      UpdateDevicePolicyConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDevicePolicyConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDevicePolicyConfiguration
  where
  hashWithSalt
    salt'
    UpdateDevicePolicyConfiguration' {..} =
      salt' `Prelude.hashWithSalt` fleetArn
        `Prelude.hashWithSalt` deviceCaCertificate

instance
  Prelude.NFData
    UpdateDevicePolicyConfiguration
  where
  rnf UpdateDevicePolicyConfiguration' {..} =
    Prelude.rnf deviceCaCertificate
      `Prelude.seq` Prelude.rnf fleetArn

instance
  Core.ToHeaders
    UpdateDevicePolicyConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDevicePolicyConfiguration where
  toJSON UpdateDevicePolicyConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeviceCaCertificate" Core..=)
              Prelude.<$> deviceCaCertificate,
            Prelude.Just ("FleetArn" Core..= fleetArn)
          ]
      )

instance Core.ToPath UpdateDevicePolicyConfiguration where
  toPath =
    Prelude.const "/updateDevicePolicyConfiguration"

instance Core.ToQuery UpdateDevicePolicyConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDevicePolicyConfigurationResponse' smart constructor.
data UpdateDevicePolicyConfigurationResponse = UpdateDevicePolicyConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDevicePolicyConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDevicePolicyConfigurationResponse_httpStatus' - The response's http status code.
newUpdateDevicePolicyConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDevicePolicyConfigurationResponse
newUpdateDevicePolicyConfigurationResponse
  pHttpStatus_ =
    UpdateDevicePolicyConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateDevicePolicyConfigurationResponse_httpStatus :: Lens.Lens' UpdateDevicePolicyConfigurationResponse Prelude.Int
updateDevicePolicyConfigurationResponse_httpStatus = Lens.lens (\UpdateDevicePolicyConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateDevicePolicyConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateDevicePolicyConfigurationResponse)

instance
  Prelude.NFData
    UpdateDevicePolicyConfigurationResponse
  where
  rnf UpdateDevicePolicyConfigurationResponse' {..} =
    Prelude.rnf httpStatus
