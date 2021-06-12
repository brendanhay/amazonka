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
-- Module      : Network.AWS.EC2.ExportClientVpnClientConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the contents of the Client VPN endpoint configuration file for
-- the specified Client VPN endpoint. The Client VPN endpoint configuration
-- file includes the Client VPN endpoint and certificate information
-- clients need to establish a connection with the Client VPN endpoint.
module Network.AWS.EC2.ExportClientVpnClientConfiguration
  ( -- * Creating a Request
    ExportClientVpnClientConfiguration (..),
    newExportClientVpnClientConfiguration,

    -- * Request Lenses
    exportClientVpnClientConfiguration_dryRun,
    exportClientVpnClientConfiguration_clientVpnEndpointId,

    -- * Destructuring the Response
    ExportClientVpnClientConfigurationResponse (..),
    newExportClientVpnClientConfigurationResponse,

    -- * Response Lenses
    exportClientVpnClientConfigurationResponse_clientConfiguration,
    exportClientVpnClientConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportClientVpnClientConfiguration' smart constructor.
data ExportClientVpnClientConfiguration = ExportClientVpnClientConfiguration'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportClientVpnClientConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'exportClientVpnClientConfiguration_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientVpnEndpointId', 'exportClientVpnClientConfiguration_clientVpnEndpointId' - The ID of the Client VPN endpoint.
newExportClientVpnClientConfiguration ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  ExportClientVpnClientConfiguration
newExportClientVpnClientConfiguration
  pClientVpnEndpointId_ =
    ExportClientVpnClientConfiguration'
      { dryRun =
          Core.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
exportClientVpnClientConfiguration_dryRun :: Lens.Lens' ExportClientVpnClientConfiguration (Core.Maybe Core.Bool)
exportClientVpnClientConfiguration_dryRun = Lens.lens (\ExportClientVpnClientConfiguration' {dryRun} -> dryRun) (\s@ExportClientVpnClientConfiguration' {} a -> s {dryRun = a} :: ExportClientVpnClientConfiguration)

-- | The ID of the Client VPN endpoint.
exportClientVpnClientConfiguration_clientVpnEndpointId :: Lens.Lens' ExportClientVpnClientConfiguration Core.Text
exportClientVpnClientConfiguration_clientVpnEndpointId = Lens.lens (\ExportClientVpnClientConfiguration' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ExportClientVpnClientConfiguration' {} a -> s {clientVpnEndpointId = a} :: ExportClientVpnClientConfiguration)

instance
  Core.AWSRequest
    ExportClientVpnClientConfiguration
  where
  type
    AWSResponse ExportClientVpnClientConfiguration =
      ExportClientVpnClientConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ExportClientVpnClientConfigurationResponse'
            Core.<$> (x Core..@? "clientConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ExportClientVpnClientConfiguration

instance
  Core.NFData
    ExportClientVpnClientConfiguration

instance
  Core.ToHeaders
    ExportClientVpnClientConfiguration
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ExportClientVpnClientConfiguration
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ExportClientVpnClientConfiguration
  where
  toQuery ExportClientVpnClientConfiguration' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ExportClientVpnClientConfiguration" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
      ]

-- | /See:/ 'newExportClientVpnClientConfigurationResponse' smart constructor.
data ExportClientVpnClientConfigurationResponse = ExportClientVpnClientConfigurationResponse'
  { -- | The contents of the Client VPN endpoint configuration file.
    clientConfiguration :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportClientVpnClientConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientConfiguration', 'exportClientVpnClientConfigurationResponse_clientConfiguration' - The contents of the Client VPN endpoint configuration file.
--
-- 'httpStatus', 'exportClientVpnClientConfigurationResponse_httpStatus' - The response's http status code.
newExportClientVpnClientConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ExportClientVpnClientConfigurationResponse
newExportClientVpnClientConfigurationResponse
  pHttpStatus_ =
    ExportClientVpnClientConfigurationResponse'
      { clientConfiguration =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The contents of the Client VPN endpoint configuration file.
exportClientVpnClientConfigurationResponse_clientConfiguration :: Lens.Lens' ExportClientVpnClientConfigurationResponse (Core.Maybe Core.Text)
exportClientVpnClientConfigurationResponse_clientConfiguration = Lens.lens (\ExportClientVpnClientConfigurationResponse' {clientConfiguration} -> clientConfiguration) (\s@ExportClientVpnClientConfigurationResponse' {} a -> s {clientConfiguration = a} :: ExportClientVpnClientConfigurationResponse)

-- | The response's http status code.
exportClientVpnClientConfigurationResponse_httpStatus :: Lens.Lens' ExportClientVpnClientConfigurationResponse Core.Int
exportClientVpnClientConfigurationResponse_httpStatus = Lens.lens (\ExportClientVpnClientConfigurationResponse' {httpStatus} -> httpStatus) (\s@ExportClientVpnClientConfigurationResponse' {} a -> s {httpStatus = a} :: ExportClientVpnClientConfigurationResponse)

instance
  Core.NFData
    ExportClientVpnClientConfigurationResponse
