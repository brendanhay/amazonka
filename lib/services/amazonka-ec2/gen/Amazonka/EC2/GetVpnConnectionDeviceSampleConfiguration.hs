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
-- Module      : Amazonka.EC2.GetVpnConnectionDeviceSampleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Download an Amazon Web Services-provided sample configuration file to be
-- used with the customer gateway device specified for your Site-to-Site
-- VPN connection.
module Amazonka.EC2.GetVpnConnectionDeviceSampleConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetVpnConnectionDeviceSampleConfigurationResponse'
            Prelude.<$> (x Data..@? "vpnConnectionDeviceSampleConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVpnConnectionDeviceSampleConfiguration
  where
  hashWithSalt
    _salt
    GetVpnConnectionDeviceSampleConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` internetKeyExchangeVersion
        `Prelude.hashWithSalt` vpnConnectionId
        `Prelude.hashWithSalt` vpnConnectionDeviceTypeId

instance
  Prelude.NFData
    GetVpnConnectionDeviceSampleConfiguration
  where
  rnf GetVpnConnectionDeviceSampleConfiguration' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf internetKeyExchangeVersion
      `Prelude.seq` Prelude.rnf vpnConnectionId
      `Prelude.seq` Prelude.rnf vpnConnectionDeviceTypeId

instance
  Data.ToHeaders
    GetVpnConnectionDeviceSampleConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetVpnConnectionDeviceSampleConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetVpnConnectionDeviceSampleConfiguration
  where
  toQuery
    GetVpnConnectionDeviceSampleConfiguration' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "GetVpnConnectionDeviceSampleConfiguration" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "InternetKeyExchangeVersion"
            Data.=: internetKeyExchangeVersion,
          "VpnConnectionId" Data.=: vpnConnectionId,
          "VpnConnectionDeviceTypeId"
            Data.=: vpnConnectionDeviceTypeId
        ]

-- | /See:/ 'newGetVpnConnectionDeviceSampleConfigurationResponse' smart constructor.
data GetVpnConnectionDeviceSampleConfigurationResponse = GetVpnConnectionDeviceSampleConfigurationResponse'
  { -- | Sample configuration file for the specified customer gateway device.
    vpnConnectionDeviceSampleConfiguration :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
getVpnConnectionDeviceSampleConfigurationResponse_vpnConnectionDeviceSampleConfiguration = Lens.lens (\GetVpnConnectionDeviceSampleConfigurationResponse' {vpnConnectionDeviceSampleConfiguration} -> vpnConnectionDeviceSampleConfiguration) (\s@GetVpnConnectionDeviceSampleConfigurationResponse' {} a -> s {vpnConnectionDeviceSampleConfiguration = a} :: GetVpnConnectionDeviceSampleConfigurationResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
getVpnConnectionDeviceSampleConfigurationResponse_httpStatus :: Lens.Lens' GetVpnConnectionDeviceSampleConfigurationResponse Prelude.Int
getVpnConnectionDeviceSampleConfigurationResponse_httpStatus = Lens.lens (\GetVpnConnectionDeviceSampleConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetVpnConnectionDeviceSampleConfigurationResponse' {} a -> s {httpStatus = a} :: GetVpnConnectionDeviceSampleConfigurationResponse)

instance
  Prelude.NFData
    GetVpnConnectionDeviceSampleConfigurationResponse
  where
  rnf
    GetVpnConnectionDeviceSampleConfigurationResponse' {..} =
      Prelude.rnf vpnConnectionDeviceSampleConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
