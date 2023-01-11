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
-- Module      : Amazonka.EC2.ExportClientVpnClientConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the contents of the Client VPN endpoint configuration file for
-- the specified Client VPN endpoint. The Client VPN endpoint configuration
-- file includes the Client VPN endpoint and certificate information
-- clients need to establish a connection with the Client VPN endpoint.
module Amazonka.EC2.ExportClientVpnClientConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    ExportClientVpnClientConfiguration
  where
  type
    AWSResponse ExportClientVpnClientConfiguration =
      ExportClientVpnClientConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ExportClientVpnClientConfigurationResponse'
            Prelude.<$> (x Data..@? "clientConfiguration")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExportClientVpnClientConfiguration
  where
  hashWithSalt
    _salt
    ExportClientVpnClientConfiguration' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` clientVpnEndpointId

instance
  Prelude.NFData
    ExportClientVpnClientConfiguration
  where
  rnf ExportClientVpnClientConfiguration' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf clientVpnEndpointId

instance
  Data.ToHeaders
    ExportClientVpnClientConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ExportClientVpnClientConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ExportClientVpnClientConfiguration
  where
  toQuery ExportClientVpnClientConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ExportClientVpnClientConfiguration" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ClientVpnEndpointId" Data.=: clientVpnEndpointId
      ]

-- | /See:/ 'newExportClientVpnClientConfigurationResponse' smart constructor.
data ExportClientVpnClientConfigurationResponse = ExportClientVpnClientConfigurationResponse'
  { -- | The contents of the Client VPN endpoint configuration file.
    clientConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf ExportClientVpnClientConfigurationResponse' {..} =
    Prelude.rnf clientConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
