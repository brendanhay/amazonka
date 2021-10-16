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
-- Module      : Network.AWS.EC2.GetVpnConnectionDeviceSampleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Download an AWS-provided sample configuration file to be used with the
-- customer gateway device specified for your Site-to-Site VPN connection.
module Network.AWS.EC2.GetVpnConnectionDeviceSampleConfiguration
  ( -- * Creating a Request
    GetVpnConnectionDeviceSampleConfiguration (..),
    newGetVpnConnectionDeviceSampleConfiguration,

    -- * Request Lenses
    getVpnConnectionDeviceSampleConfiguration_dryRun,
    getVpnConnectionDeviceSampleConfiguration_internetKeyExchangeVersion,
    getVpnConnectionDeviceSampleConfiguration_vpnConnectionId,
    getVpnConnectionDeviceSampleConfiguration_vpnConnectionDeviceTypeId,

    -- * Destructuring the Response
    GetVpnConnectionDeviceSampleConfigurationResponse (..),
    newGetVpnConnectionDeviceSampleConfigurationResponse,

    -- * Response Lenses
    getVpnConnectionDeviceSampleConfigurationResponse_vpnConnectionDeviceSampleConfiguration,
    getVpnConnectionDeviceSampleConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetVpnConnectionDeviceSampleConfiguration' smart constructor.
data GetVpnConnectionDeviceSampleConfiguration = GetVpnConnectionDeviceSampleConfiguration'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IKE version to be used in the sample configuration file for your
    -- customer gateway device. You can specify one of the following versions:
    -- @ikev1@ or @ikev2@.
    internetKeyExchangeVersion :: Prelude.Maybe Prelude.Text,
    -- | The @VpnConnectionId@ specifies the Site-to-Site VPN connection used for
    -- the sample configuration.
    vpnConnectionId :: Prelude.Text,
    -- | Device identifier provided by the @GetVpnConnectionDeviceTypes@ API.
    vpnConnectionDeviceTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpnConnectionDeviceSampleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getVpnConnectionDeviceSampleConfiguration_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'internetKeyExchangeVersion', 'getVpnConnectionDeviceSampleConfiguration_internetKeyExchangeVersion' - The IKE version to be used in the sample configuration file for your
-- customer gateway device. You can specify one of the following versions:
-- @ikev1@ or @ikev2@.
--
-- 'vpnConnectionId', 'getVpnConnectionDeviceSampleConfiguration_vpnConnectionId' - The @VpnConnectionId@ specifies the Site-to-Site VPN connection used for
-- the sample configuration.
--
-- 'vpnConnectionDeviceTypeId', 'getVpnConnectionDeviceSampleConfiguration_vpnConnectionDeviceTypeId' - Device identifier provided by the @GetVpnConnectionDeviceTypes@ API.
newGetVpnConnectionDeviceSampleConfiguration ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  -- | 'vpnConnectionDeviceTypeId'
  Prelude.Text ->
  GetVpnConnectionDeviceSampleConfiguration
newGetVpnConnectionDeviceSampleConfiguration
  pVpnConnectionId_
  pVpnConnectionDeviceTypeId_ =
    GetVpnConnectionDeviceSampleConfiguration'
      { dryRun =
          Prelude.Nothing,
        internetKeyExchangeVersion =
          Prelude.Nothing,
        vpnConnectionId =
          pVpnConnectionId_,
        vpnConnectionDeviceTypeId =
          pVpnConnectionDeviceTypeId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getVpnConnectionDeviceSampleConfiguration_dryRun :: Lens.Lens' GetVpnConnectionDeviceSampleConfiguration (Prelude.Maybe Prelude.Bool)
getVpnConnectionDeviceSampleConfiguration_dryRun = Lens.lens (\GetVpnConnectionDeviceSampleConfiguration' {dryRun} -> dryRun) (\s@GetVpnConnectionDeviceSampleConfiguration' {} a -> s {dryRun = a} :: GetVpnConnectionDeviceSampleConfiguration)

-- | The IKE version to be used in the sample configuration file for your
-- customer gateway device. You can specify one of the following versions:
-- @ikev1@ or @ikev2@.
getVpnConnectionDeviceSampleConfiguration_internetKeyExchangeVersion :: Lens.Lens' GetVpnConnectionDeviceSampleConfiguration (Prelude.Maybe Prelude.Text)
getVpnConnectionDeviceSampleConfiguration_internetKeyExchangeVersion = Lens.lens (\GetVpnConnectionDeviceSampleConfiguration' {internetKeyExchangeVersion} -> internetKeyExchangeVersion) (\s@GetVpnConnectionDeviceSampleConfiguration' {} a -> s {internetKeyExchangeVersion = a} :: GetVpnConnectionDeviceSampleConfiguration)

-- | The @VpnConnectionId@ specifies the Site-to-Site VPN connection used for
-- the sample configuration.
getVpnConnectionDeviceSampleConfiguration_vpnConnectionId :: Lens.Lens' GetVpnConnectionDeviceSampleConfiguration Prelude.Text
getVpnConnectionDeviceSampleConfiguration_vpnConnectionId = Lens.lens (\GetVpnConnectionDeviceSampleConfiguration' {vpnConnectionId} -> vpnConnectionId) (\s@GetVpnConnectionDeviceSampleConfiguration' {} a -> s {vpnConnectionId = a} :: GetVpnConnectionDeviceSampleConfiguration)

-- | Device identifier provided by the @GetVpnConnectionDeviceTypes@ API.
getVpnConnectionDeviceSampleConfiguration_vpnConnectionDeviceTypeId :: Lens.Lens' GetVpnConnectionDeviceSampleConfiguration Prelude.Text
getVpnConnectionDeviceSampleConfiguration_vpnConnectionDeviceTypeId = Lens.lens (\GetVpnConnectionDeviceSampleConfiguration' {vpnConnectionDeviceTypeId} -> vpnConnectionDeviceTypeId) (\s@GetVpnConnectionDeviceSampleConfiguration' {} a -> s {vpnConnectionDeviceTypeId = a} :: GetVpnConnectionDeviceSampleConfiguration)

instance
  Core.AWSRequest
    GetVpnConnectionDeviceSampleConfiguration
  where
  type
    AWSResponse
      GetVpnConnectionDeviceSampleConfiguration =
      GetVpnConnectionDeviceSampleConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetVpnConnectionDeviceSampleConfigurationResponse'
            Prelude.<$> (x Core..@? "vpnConnectionDeviceSampleConfiguration")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVpnConnectionDeviceSampleConfiguration

instance
  Prelude.NFData
    GetVpnConnectionDeviceSampleConfiguration

instance
  Core.ToHeaders
    GetVpnConnectionDeviceSampleConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetVpnConnectionDeviceSampleConfiguration
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetVpnConnectionDeviceSampleConfiguration
  where
  toQuery
    GetVpnConnectionDeviceSampleConfiguration' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "GetVpnConnectionDeviceSampleConfiguration" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Core.=: dryRun,
          "InternetKeyExchangeVersion"
            Core.=: internetKeyExchangeVersion,
          "VpnConnectionId" Core.=: vpnConnectionId,
          "VpnConnectionDeviceTypeId"
            Core.=: vpnConnectionDeviceTypeId
        ]

-- | /See:/ 'newGetVpnConnectionDeviceSampleConfigurationResponse' smart constructor.
data GetVpnConnectionDeviceSampleConfigurationResponse = GetVpnConnectionDeviceSampleConfigurationResponse'
  { -- | Sample configuration file for the specified customer gateway device.
    vpnConnectionDeviceSampleConfiguration :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpnConnectionDeviceSampleConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnConnectionDeviceSampleConfiguration', 'getVpnConnectionDeviceSampleConfigurationResponse_vpnConnectionDeviceSampleConfiguration' - Sample configuration file for the specified customer gateway device.
--
-- 'httpStatus', 'getVpnConnectionDeviceSampleConfigurationResponse_httpStatus' - The response's http status code.
newGetVpnConnectionDeviceSampleConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVpnConnectionDeviceSampleConfigurationResponse
newGetVpnConnectionDeviceSampleConfigurationResponse
  pHttpStatus_ =
    GetVpnConnectionDeviceSampleConfigurationResponse'
      { vpnConnectionDeviceSampleConfiguration =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Sample configuration file for the specified customer gateway device.
getVpnConnectionDeviceSampleConfigurationResponse_vpnConnectionDeviceSampleConfiguration :: Lens.Lens' GetVpnConnectionDeviceSampleConfigurationResponse (Prelude.Maybe Prelude.Text)
getVpnConnectionDeviceSampleConfigurationResponse_vpnConnectionDeviceSampleConfiguration = Lens.lens (\GetVpnConnectionDeviceSampleConfigurationResponse' {vpnConnectionDeviceSampleConfiguration} -> vpnConnectionDeviceSampleConfiguration) (\s@GetVpnConnectionDeviceSampleConfigurationResponse' {} a -> s {vpnConnectionDeviceSampleConfiguration = a} :: GetVpnConnectionDeviceSampleConfigurationResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
getVpnConnectionDeviceSampleConfigurationResponse_httpStatus :: Lens.Lens' GetVpnConnectionDeviceSampleConfigurationResponse Prelude.Int
getVpnConnectionDeviceSampleConfigurationResponse_httpStatus = Lens.lens (\GetVpnConnectionDeviceSampleConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetVpnConnectionDeviceSampleConfigurationResponse' {} a -> s {httpStatus = a} :: GetVpnConnectionDeviceSampleConfigurationResponse)

instance
  Prelude.NFData
    GetVpnConnectionDeviceSampleConfigurationResponse
