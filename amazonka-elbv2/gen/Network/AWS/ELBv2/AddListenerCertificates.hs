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
-- Module      : Network.AWS.ELBv2.AddListenerCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified SSL server certificate to the certificate list for
-- the specified HTTPS or TLS listener.
--
-- If the certificate in already in the certificate list, the call is
-- successful but the certificate is not added again.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html HTTPS listeners>
-- in the /Application Load Balancers Guide/ or
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html TLS listeners>
-- in the /Network Load Balancers Guide/.
module Network.AWS.ELBv2.AddListenerCertificates
  ( -- * Creating a Request
    AddListenerCertificates (..),
    newAddListenerCertificates,

    -- * Request Lenses
    addListenerCertificates_listenerArn,
    addListenerCertificates_certificates,

    -- * Destructuring the Response
    AddListenerCertificatesResponse (..),
    newAddListenerCertificatesResponse,

    -- * Response Lenses
    addListenerCertificatesResponse_certificates,
    addListenerCertificatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddListenerCertificates' smart constructor.
data AddListenerCertificates = AddListenerCertificates'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Core.Text,
    -- | The certificate to add. You can specify one certificate per call. Set
    -- @CertificateArn@ to the certificate ARN but do not set @IsDefault@.
    certificates :: [Certificate]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddListenerCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArn', 'addListenerCertificates_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'certificates', 'addListenerCertificates_certificates' - The certificate to add. You can specify one certificate per call. Set
-- @CertificateArn@ to the certificate ARN but do not set @IsDefault@.
newAddListenerCertificates ::
  -- | 'listenerArn'
  Core.Text ->
  AddListenerCertificates
newAddListenerCertificates pListenerArn_ =
  AddListenerCertificates'
    { listenerArn =
        pListenerArn_,
      certificates = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the listener.
addListenerCertificates_listenerArn :: Lens.Lens' AddListenerCertificates Core.Text
addListenerCertificates_listenerArn = Lens.lens (\AddListenerCertificates' {listenerArn} -> listenerArn) (\s@AddListenerCertificates' {} a -> s {listenerArn = a} :: AddListenerCertificates)

-- | The certificate to add. You can specify one certificate per call. Set
-- @CertificateArn@ to the certificate ARN but do not set @IsDefault@.
addListenerCertificates_certificates :: Lens.Lens' AddListenerCertificates [Certificate]
addListenerCertificates_certificates = Lens.lens (\AddListenerCertificates' {certificates} -> certificates) (\s@AddListenerCertificates' {} a -> s {certificates = a} :: AddListenerCertificates) Core.. Lens._Coerce

instance Core.AWSRequest AddListenerCertificates where
  type
    AWSResponse AddListenerCertificates =
      AddListenerCertificatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AddListenerCertificatesResult"
      ( \s h x ->
          AddListenerCertificatesResponse'
            Core.<$> ( x Core..@? "Certificates" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddListenerCertificates

instance Core.NFData AddListenerCertificates

instance Core.ToHeaders AddListenerCertificates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AddListenerCertificates where
  toPath = Core.const "/"

instance Core.ToQuery AddListenerCertificates where
  toQuery AddListenerCertificates' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AddListenerCertificates" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "ListenerArn" Core.=: listenerArn,
        "Certificates"
          Core.=: Core.toQueryList "member" certificates
      ]

-- | /See:/ 'newAddListenerCertificatesResponse' smart constructor.
data AddListenerCertificatesResponse = AddListenerCertificatesResponse'
  { -- | Information about the certificates in the certificate list.
    certificates :: Core.Maybe [Certificate],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddListenerCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificates', 'addListenerCertificatesResponse_certificates' - Information about the certificates in the certificate list.
--
-- 'httpStatus', 'addListenerCertificatesResponse_httpStatus' - The response's http status code.
newAddListenerCertificatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddListenerCertificatesResponse
newAddListenerCertificatesResponse pHttpStatus_ =
  AddListenerCertificatesResponse'
    { certificates =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the certificates in the certificate list.
addListenerCertificatesResponse_certificates :: Lens.Lens' AddListenerCertificatesResponse (Core.Maybe [Certificate])
addListenerCertificatesResponse_certificates = Lens.lens (\AddListenerCertificatesResponse' {certificates} -> certificates) (\s@AddListenerCertificatesResponse' {} a -> s {certificates = a} :: AddListenerCertificatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
addListenerCertificatesResponse_httpStatus :: Lens.Lens' AddListenerCertificatesResponse Core.Int
addListenerCertificatesResponse_httpStatus = Lens.lens (\AddListenerCertificatesResponse' {httpStatus} -> httpStatus) (\s@AddListenerCertificatesResponse' {} a -> s {httpStatus = a} :: AddListenerCertificatesResponse)

instance Core.NFData AddListenerCertificatesResponse
