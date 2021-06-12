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
-- Module      : Network.AWS.EC2.ExportClientVpnClientCertificateRevocationList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the client certificate revocation list for the specified
-- Client VPN endpoint.
module Network.AWS.EC2.ExportClientVpnClientCertificateRevocationList
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportClientVpnClientCertificateRevocationList' smart constructor.
data ExportClientVpnClientCertificateRevocationList = ExportClientVpnClientCertificateRevocationList'
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
  Core.Text ->
  ExportClientVpnClientCertificateRevocationList
newExportClientVpnClientCertificateRevocationList
  pClientVpnEndpointId_ =
    ExportClientVpnClientCertificateRevocationList'
      { dryRun =
          Core.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
exportClientVpnClientCertificateRevocationList_dryRun :: Lens.Lens' ExportClientVpnClientCertificateRevocationList (Core.Maybe Core.Bool)
exportClientVpnClientCertificateRevocationList_dryRun = Lens.lens (\ExportClientVpnClientCertificateRevocationList' {dryRun} -> dryRun) (\s@ExportClientVpnClientCertificateRevocationList' {} a -> s {dryRun = a} :: ExportClientVpnClientCertificateRevocationList)

-- | The ID of the Client VPN endpoint.
exportClientVpnClientCertificateRevocationList_clientVpnEndpointId :: Lens.Lens' ExportClientVpnClientCertificateRevocationList Core.Text
exportClientVpnClientCertificateRevocationList_clientVpnEndpointId = Lens.lens (\ExportClientVpnClientCertificateRevocationList' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ExportClientVpnClientCertificateRevocationList' {} a -> s {clientVpnEndpointId = a} :: ExportClientVpnClientCertificateRevocationList)

instance
  Core.AWSRequest
    ExportClientVpnClientCertificateRevocationList
  where
  type
    AWSResponse
      ExportClientVpnClientCertificateRevocationList =
      ExportClientVpnClientCertificateRevocationListResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ExportClientVpnClientCertificateRevocationListResponse'
            Core.<$> (x Core..@? "certificateRevocationList")
              Core.<*> (x Core..@? "status")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ExportClientVpnClientCertificateRevocationList

instance
  Core.NFData
    ExportClientVpnClientCertificateRevocationList

instance
  Core.ToHeaders
    ExportClientVpnClientCertificateRevocationList
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ExportClientVpnClientCertificateRevocationList
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ExportClientVpnClientCertificateRevocationList
  where
  toQuery
    ExportClientVpnClientCertificateRevocationList' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "ExportClientVpnClientCertificateRevocationList" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun,
          "ClientVpnEndpointId" Core.=: clientVpnEndpointId
        ]

-- | /See:/ 'newExportClientVpnClientCertificateRevocationListResponse' smart constructor.
data ExportClientVpnClientCertificateRevocationListResponse = ExportClientVpnClientCertificateRevocationListResponse'
  { -- | Information about the client certificate revocation list.
    certificateRevocationList :: Core.Maybe Core.Text,
    -- | The current state of the client certificate revocation list.
    status :: Core.Maybe ClientCertificateRevocationListStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ExportClientVpnClientCertificateRevocationListResponse
newExportClientVpnClientCertificateRevocationListResponse
  pHttpStatus_ =
    ExportClientVpnClientCertificateRevocationListResponse'
      { certificateRevocationList =
          Core.Nothing,
        status =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the client certificate revocation list.
exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse (Core.Maybe Core.Text)
exportClientVpnClientCertificateRevocationListResponse_certificateRevocationList = Lens.lens (\ExportClientVpnClientCertificateRevocationListResponse' {certificateRevocationList} -> certificateRevocationList) (\s@ExportClientVpnClientCertificateRevocationListResponse' {} a -> s {certificateRevocationList = a} :: ExportClientVpnClientCertificateRevocationListResponse)

-- | The current state of the client certificate revocation list.
exportClientVpnClientCertificateRevocationListResponse_status :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse (Core.Maybe ClientCertificateRevocationListStatus)
exportClientVpnClientCertificateRevocationListResponse_status = Lens.lens (\ExportClientVpnClientCertificateRevocationListResponse' {status} -> status) (\s@ExportClientVpnClientCertificateRevocationListResponse' {} a -> s {status = a} :: ExportClientVpnClientCertificateRevocationListResponse)

-- | The response's http status code.
exportClientVpnClientCertificateRevocationListResponse_httpStatus :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse Core.Int
exportClientVpnClientCertificateRevocationListResponse_httpStatus = Lens.lens (\ExportClientVpnClientCertificateRevocationListResponse' {httpStatus} -> httpStatus) (\s@ExportClientVpnClientCertificateRevocationListResponse' {} a -> s {httpStatus = a} :: ExportClientVpnClientCertificateRevocationListResponse)

instance
  Core.NFData
    ExportClientVpnClientCertificateRevocationListResponse
