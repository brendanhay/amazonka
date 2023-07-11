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
-- Module      : Amazonka.EC2.ExportClientVpnClientCertificateRevocationList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the client certificate revocation list for the specified
-- Client VPN endpoint.
module Amazonka.EC2.ExportClientVpnClientCertificateRevocationList
  ( -- * Creating a Request
    ExportClientVpnClientCertificateRevocationList (..),
    newExportClientVpnClientCertificateRevocationList,

    -- * Request Lenses
    exportClientVpnClientCertificateRevocationList_dryRun,
    exportClientVpnClientCertificateRevocationList_clientVpnEndpointId,

    -- * Destructuring the Response
    ExportClientVpnClientCertificateRevocationListResponse (..),
    newExportClientVpnClientCertificateRevocationListResponse,

    -- * Response Lenses
    exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList,
    exportClientVpnClientCertificateRevocationListResponse_status,
    exportClientVpnClientCertificateRevocationListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportClientVpnClientCertificateRevocationList' smart constructor.
data ExportClientVpnClientCertificateRevocationList = ExportClientVpnClientCertificateRevocationList'
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
-- Create a value of 'ExportClientVpnClientCertificateRevocationList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'exportClientVpnClientCertificateRevocationList_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientVpnEndpointId', 'exportClientVpnClientCertificateRevocationList_clientVpnEndpointId' - The ID of the Client VPN endpoint.
newExportClientVpnClientCertificateRevocationList ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  ExportClientVpnClientCertificateRevocationList
newExportClientVpnClientCertificateRevocationList
  pClientVpnEndpointId_ =
    ExportClientVpnClientCertificateRevocationList'
      { dryRun =
          Prelude.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
exportClientVpnClientCertificateRevocationList_dryRun :: Lens.Lens' ExportClientVpnClientCertificateRevocationList (Prelude.Maybe Prelude.Bool)
exportClientVpnClientCertificateRevocationList_dryRun = Lens.lens (\ExportClientVpnClientCertificateRevocationList' {dryRun} -> dryRun) (\s@ExportClientVpnClientCertificateRevocationList' {} a -> s {dryRun = a} :: ExportClientVpnClientCertificateRevocationList)

-- | The ID of the Client VPN endpoint.
exportClientVpnClientCertificateRevocationList_clientVpnEndpointId :: Lens.Lens' ExportClientVpnClientCertificateRevocationList Prelude.Text
exportClientVpnClientCertificateRevocationList_clientVpnEndpointId = Lens.lens (\ExportClientVpnClientCertificateRevocationList' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ExportClientVpnClientCertificateRevocationList' {} a -> s {clientVpnEndpointId = a} :: ExportClientVpnClientCertificateRevocationList)

instance
  Core.AWSRequest
    ExportClientVpnClientCertificateRevocationList
  where
  type
    AWSResponse
      ExportClientVpnClientCertificateRevocationList =
      ExportClientVpnClientCertificateRevocationListResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ExportClientVpnClientCertificateRevocationListResponse'
            Prelude.<$> (x Data..@? "certificateRevocationList")
            Prelude.<*> (x Data..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExportClientVpnClientCertificateRevocationList
  where
  hashWithSalt
    _salt
    ExportClientVpnClientCertificateRevocationList' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` clientVpnEndpointId

instance
  Prelude.NFData
    ExportClientVpnClientCertificateRevocationList
  where
  rnf
    ExportClientVpnClientCertificateRevocationList' {..} =
      Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf clientVpnEndpointId

instance
  Data.ToHeaders
    ExportClientVpnClientCertificateRevocationList
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ExportClientVpnClientCertificateRevocationList
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ExportClientVpnClientCertificateRevocationList
  where
  toQuery
    ExportClientVpnClientCertificateRevocationList' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "ExportClientVpnClientCertificateRevocationList" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "ClientVpnEndpointId" Data.=: clientVpnEndpointId
        ]

-- | /See:/ 'newExportClientVpnClientCertificateRevocationListResponse' smart constructor.
data ExportClientVpnClientCertificateRevocationListResponse = ExportClientVpnClientCertificateRevocationListResponse'
  { -- | Information about the client certificate revocation list.
    certificateRevocationList :: Prelude.Maybe Prelude.Text,
    -- | The current state of the client certificate revocation list.
    status :: Prelude.Maybe ClientCertificateRevocationListStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportClientVpnClientCertificateRevocationListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateRevocationList', 'exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList' - Information about the client certificate revocation list.
--
-- 'status', 'exportClientVpnClientCertificateRevocationListResponse_status' - The current state of the client certificate revocation list.
--
-- 'httpStatus', 'exportClientVpnClientCertificateRevocationListResponse_httpStatus' - The response's http status code.
newExportClientVpnClientCertificateRevocationListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportClientVpnClientCertificateRevocationListResponse
newExportClientVpnClientCertificateRevocationListResponse
  pHttpStatus_ =
    ExportClientVpnClientCertificateRevocationListResponse'
      { certificateRevocationList =
          Prelude.Nothing,
        status =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the client certificate revocation list.
exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse (Prelude.Maybe Prelude.Text)
exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList = Lens.lens (\ExportClientVpnClientCertificateRevocationListResponse' {certificateRevocationList} -> certificateRevocationList) (\s@ExportClientVpnClientCertificateRevocationListResponse' {} a -> s {certificateRevocationList = a} :: ExportClientVpnClientCertificateRevocationListResponse)

-- | The current state of the client certificate revocation list.
exportClientVpnClientCertificateRevocationListResponse_status :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse (Prelude.Maybe ClientCertificateRevocationListStatus)
exportClientVpnClientCertificateRevocationListResponse_status = Lens.lens (\ExportClientVpnClientCertificateRevocationListResponse' {status} -> status) (\s@ExportClientVpnClientCertificateRevocationListResponse' {} a -> s {status = a} :: ExportClientVpnClientCertificateRevocationListResponse)

-- | The response's http status code.
exportClientVpnClientCertificateRevocationListResponse_httpStatus :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse Prelude.Int
exportClientVpnClientCertificateRevocationListResponse_httpStatus = Lens.lens (\ExportClientVpnClientCertificateRevocationListResponse' {httpStatus} -> httpStatus) (\s@ExportClientVpnClientCertificateRevocationListResponse' {} a -> s {httpStatus = a} :: ExportClientVpnClientCertificateRevocationListResponse)

instance
  Prelude.NFData
    ExportClientVpnClientCertificateRevocationListResponse
  where
  rnf
    ExportClientVpnClientCertificateRevocationListResponse' {..} =
      Prelude.rnf certificateRevocationList
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf httpStatus
