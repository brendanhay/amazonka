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
-- Module      : Network.AWS.EC2.ImportClientVpnClientCertificateRevocationList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a client certificate revocation list to the specified Client VPN
-- endpoint. Uploading a client certificate revocation list overwrites the
-- existing client certificate revocation list.
--
-- Uploading a client certificate revocation list resets existing client
-- connections.
module Network.AWS.EC2.ImportClientVpnClientCertificateRevocationList
  ( -- * Creating a Request
    ImportClientVpnClientCertificateRevocationList (..),
    newImportClientVpnClientCertificateRevocationList,

    -- * Request Lenses
    importClientVpnClientCertificateRevocationList_dryRun,
    importClientVpnClientCertificateRevocationList_clientVpnEndpointId,
    importClientVpnClientCertificateRevocationList_certificateRevocationList,

    -- * Destructuring the Response
    ImportClientVpnClientCertificateRevocationListResponse (..),
    newImportClientVpnClientCertificateRevocationListResponse,

    -- * Response Lenses
    importClientVpnClientCertificateRevocationListResponse_return,
    importClientVpnClientCertificateRevocationListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportClientVpnClientCertificateRevocationList' smart constructor.
data ImportClientVpnClientCertificateRevocationList = ImportClientVpnClientCertificateRevocationList'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Client VPN endpoint to which the client certificate
    -- revocation list applies.
    clientVpnEndpointId :: Core.Text,
    -- | The client certificate revocation list file. For more information, see
    -- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List>
    -- in the /AWS Client VPN Administrator Guide/.
    certificateRevocationList :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportClientVpnClientCertificateRevocationList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'importClientVpnClientCertificateRevocationList_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientVpnEndpointId', 'importClientVpnClientCertificateRevocationList_clientVpnEndpointId' - The ID of the Client VPN endpoint to which the client certificate
-- revocation list applies.
--
-- 'certificateRevocationList', 'importClientVpnClientCertificateRevocationList_certificateRevocationList' - The client certificate revocation list file. For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List>
-- in the /AWS Client VPN Administrator Guide/.
newImportClientVpnClientCertificateRevocationList ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  -- | 'certificateRevocationList'
  Core.Text ->
  ImportClientVpnClientCertificateRevocationList
newImportClientVpnClientCertificateRevocationList
  pClientVpnEndpointId_
  pCertificateRevocationList_ =
    ImportClientVpnClientCertificateRevocationList'
      { dryRun =
          Core.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_,
        certificateRevocationList =
          pCertificateRevocationList_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importClientVpnClientCertificateRevocationList_dryRun :: Lens.Lens' ImportClientVpnClientCertificateRevocationList (Core.Maybe Core.Bool)
importClientVpnClientCertificateRevocationList_dryRun = Lens.lens (\ImportClientVpnClientCertificateRevocationList' {dryRun} -> dryRun) (\s@ImportClientVpnClientCertificateRevocationList' {} a -> s {dryRun = a} :: ImportClientVpnClientCertificateRevocationList)

-- | The ID of the Client VPN endpoint to which the client certificate
-- revocation list applies.
importClientVpnClientCertificateRevocationList_clientVpnEndpointId :: Lens.Lens' ImportClientVpnClientCertificateRevocationList Core.Text
importClientVpnClientCertificateRevocationList_clientVpnEndpointId = Lens.lens (\ImportClientVpnClientCertificateRevocationList' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ImportClientVpnClientCertificateRevocationList' {} a -> s {clientVpnEndpointId = a} :: ImportClientVpnClientCertificateRevocationList)

-- | The client certificate revocation list file. For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List>
-- in the /AWS Client VPN Administrator Guide/.
importClientVpnClientCertificateRevocationList_certificateRevocationList :: Lens.Lens' ImportClientVpnClientCertificateRevocationList Core.Text
importClientVpnClientCertificateRevocationList_certificateRevocationList = Lens.lens (\ImportClientVpnClientCertificateRevocationList' {certificateRevocationList} -> certificateRevocationList) (\s@ImportClientVpnClientCertificateRevocationList' {} a -> s {certificateRevocationList = a} :: ImportClientVpnClientCertificateRevocationList)

instance
  Core.AWSRequest
    ImportClientVpnClientCertificateRevocationList
  where
  type
    AWSResponse
      ImportClientVpnClientCertificateRevocationList =
      ImportClientVpnClientCertificateRevocationListResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ImportClientVpnClientCertificateRevocationListResponse'
            Core.<$> (x Core..@? "return")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ImportClientVpnClientCertificateRevocationList

instance
  Core.NFData
    ImportClientVpnClientCertificateRevocationList

instance
  Core.ToHeaders
    ImportClientVpnClientCertificateRevocationList
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ImportClientVpnClientCertificateRevocationList
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ImportClientVpnClientCertificateRevocationList
  where
  toQuery
    ImportClientVpnClientCertificateRevocationList' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "ImportClientVpnClientCertificateRevocationList" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun,
          "ClientVpnEndpointId" Core.=: clientVpnEndpointId,
          "CertificateRevocationList"
            Core.=: certificateRevocationList
        ]

-- | /See:/ 'newImportClientVpnClientCertificateRevocationListResponse' smart constructor.
data ImportClientVpnClientCertificateRevocationListResponse = ImportClientVpnClientCertificateRevocationListResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportClientVpnClientCertificateRevocationListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'importClientVpnClientCertificateRevocationListResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'importClientVpnClientCertificateRevocationListResponse_httpStatus' - The response's http status code.
newImportClientVpnClientCertificateRevocationListResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ImportClientVpnClientCertificateRevocationListResponse
newImportClientVpnClientCertificateRevocationListResponse
  pHttpStatus_ =
    ImportClientVpnClientCertificateRevocationListResponse'
      { return' =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
importClientVpnClientCertificateRevocationListResponse_return :: Lens.Lens' ImportClientVpnClientCertificateRevocationListResponse (Core.Maybe Core.Bool)
importClientVpnClientCertificateRevocationListResponse_return = Lens.lens (\ImportClientVpnClientCertificateRevocationListResponse' {return'} -> return') (\s@ImportClientVpnClientCertificateRevocationListResponse' {} a -> s {return' = a} :: ImportClientVpnClientCertificateRevocationListResponse)

-- | The response's http status code.
importClientVpnClientCertificateRevocationListResponse_httpStatus :: Lens.Lens' ImportClientVpnClientCertificateRevocationListResponse Core.Int
importClientVpnClientCertificateRevocationListResponse_httpStatus = Lens.lens (\ImportClientVpnClientCertificateRevocationListResponse' {httpStatus} -> httpStatus) (\s@ImportClientVpnClientCertificateRevocationListResponse' {} a -> s {httpStatus = a} :: ImportClientVpnClientCertificateRevocationListResponse)

instance
  Core.NFData
    ImportClientVpnClientCertificateRevocationListResponse
