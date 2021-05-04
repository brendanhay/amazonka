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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportClientVpnClientConfiguration' smart constructor.
data ExportClientVpnClientConfiguration = ExportClientVpnClientConfiguration'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ExportClientVpnClientConfiguration
newExportClientVpnClientConfiguration
  pClientVpnEndpointId_ =
    ExportClientVpnClientConfiguration'
      { dryRun =
          Prelude.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
exportClientVpnClientConfiguration_dryRun :: Lens.Lens' ExportClientVpnClientConfiguration (Prelude.Maybe Prelude.Bool)
exportClientVpnClientConfiguration_dryRun = Lens.lens (\ExportClientVpnClientConfiguration' {dryRun} -> dryRun) (\s@ExportClientVpnClientConfiguration' {} a -> s {dryRun = a} :: ExportClientVpnClientConfiguration)

-- | The ID of the Client VPN endpoint.
exportClientVpnClientConfiguration_clientVpnEndpointId :: Lens.Lens' ExportClientVpnClientConfiguration Prelude.Text
exportClientVpnClientConfiguration_clientVpnEndpointId = Lens.lens (\ExportClientVpnClientConfiguration' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ExportClientVpnClientConfiguration' {} a -> s {clientVpnEndpointId = a} :: ExportClientVpnClientConfiguration)

instance
  Prelude.AWSRequest
    ExportClientVpnClientConfiguration
  where
  type
    Rs ExportClientVpnClientConfiguration =
      ExportClientVpnClientConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ExportClientVpnClientConfigurationResponse'
            Prelude.<$> (x Prelude..@? "clientConfiguration")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExportClientVpnClientConfiguration

instance
  Prelude.NFData
    ExportClientVpnClientConfiguration

instance
  Prelude.ToHeaders
    ExportClientVpnClientConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ExportClientVpnClientConfiguration
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ExportClientVpnClientConfiguration
  where
  toQuery ExportClientVpnClientConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ExportClientVpnClientConfiguration" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "ClientVpnEndpointId" Prelude.=: clientVpnEndpointId
      ]

-- | /See:/ 'newExportClientVpnClientConfigurationResponse' smart constructor.
data ExportClientVpnClientConfigurationResponse = ExportClientVpnClientConfigurationResponse'
  { -- | The contents of the Client VPN endpoint configuration file.
    clientConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ExportClientVpnClientConfigurationResponse
newExportClientVpnClientConfigurationResponse
  pHttpStatus_ =
    ExportClientVpnClientConfigurationResponse'
      { clientConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The contents of the Client VPN endpoint configuration file.
exportClientVpnClientConfigurationResponse_clientConfiguration :: Lens.Lens' ExportClientVpnClientConfigurationResponse (Prelude.Maybe Prelude.Text)
exportClientVpnClientConfigurationResponse_clientConfiguration = Lens.lens (\ExportClientVpnClientConfigurationResponse' {clientConfiguration} -> clientConfiguration) (\s@ExportClientVpnClientConfigurationResponse' {} a -> s {clientConfiguration = a} :: ExportClientVpnClientConfigurationResponse)

-- | The response's http status code.
exportClientVpnClientConfigurationResponse_httpStatus :: Lens.Lens' ExportClientVpnClientConfigurationResponse Prelude.Int
exportClientVpnClientConfigurationResponse_httpStatus = Lens.lens (\ExportClientVpnClientConfigurationResponse' {httpStatus} -> httpStatus) (\s@ExportClientVpnClientConfigurationResponse' {} a -> s {httpStatus = a} :: ExportClientVpnClientConfigurationResponse)

instance
  Prelude.NFData
    ExportClientVpnClientConfigurationResponse
