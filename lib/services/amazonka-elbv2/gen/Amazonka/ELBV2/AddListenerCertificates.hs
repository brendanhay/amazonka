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
-- Module      : Amazonka.ELBV2.AddListenerCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ELBV2.AddListenerCertificates
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddListenerCertificates' smart constructor.
data AddListenerCertificates = AddListenerCertificates'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Text,
    -- | The certificate to add. You can specify one certificate per call. Set
    -- @CertificateArn@ to the certificate ARN but do not set @IsDefault@.
    certificates :: [Certificate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  AddListenerCertificates
newAddListenerCertificates pListenerArn_ =
  AddListenerCertificates'
    { listenerArn =
        pListenerArn_,
      certificates = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the listener.
addListenerCertificates_listenerArn :: Lens.Lens' AddListenerCertificates Prelude.Text
addListenerCertificates_listenerArn = Lens.lens (\AddListenerCertificates' {listenerArn} -> listenerArn) (\s@AddListenerCertificates' {} a -> s {listenerArn = a} :: AddListenerCertificates)

-- | The certificate to add. You can specify one certificate per call. Set
-- @CertificateArn@ to the certificate ARN but do not set @IsDefault@.
addListenerCertificates_certificates :: Lens.Lens' AddListenerCertificates [Certificate]
addListenerCertificates_certificates = Lens.lens (\AddListenerCertificates' {certificates} -> certificates) (\s@AddListenerCertificates' {} a -> s {certificates = a} :: AddListenerCertificates) Prelude.. Lens.coerced

instance Core.AWSRequest AddListenerCertificates where
  type
    AWSResponse AddListenerCertificates =
      AddListenerCertificatesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AddListenerCertificatesResult"
      ( \s h x ->
          AddListenerCertificatesResponse'
            Prelude.<$> ( x Core..@? "Certificates" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddListenerCertificates where
  hashWithSalt _salt AddListenerCertificates' {..} =
    _salt `Prelude.hashWithSalt` listenerArn
      `Prelude.hashWithSalt` certificates

instance Prelude.NFData AddListenerCertificates where
  rnf AddListenerCertificates' {..} =
    Prelude.rnf listenerArn
      `Prelude.seq` Prelude.rnf certificates

instance Core.ToHeaders AddListenerCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AddListenerCertificates where
  toPath = Prelude.const "/"

instance Core.ToQuery AddListenerCertificates where
  toQuery AddListenerCertificates' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AddListenerCertificates" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "ListenerArn" Core.=: listenerArn,
        "Certificates"
          Core.=: Core.toQueryList "member" certificates
      ]

-- | /See:/ 'newAddListenerCertificatesResponse' smart constructor.
data AddListenerCertificatesResponse = AddListenerCertificatesResponse'
  { -- | Information about the certificates in the certificate list.
    certificates :: Prelude.Maybe [Certificate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AddListenerCertificatesResponse
newAddListenerCertificatesResponse pHttpStatus_ =
  AddListenerCertificatesResponse'
    { certificates =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the certificates in the certificate list.
addListenerCertificatesResponse_certificates :: Lens.Lens' AddListenerCertificatesResponse (Prelude.Maybe [Certificate])
addListenerCertificatesResponse_certificates = Lens.lens (\AddListenerCertificatesResponse' {certificates} -> certificates) (\s@AddListenerCertificatesResponse' {} a -> s {certificates = a} :: AddListenerCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addListenerCertificatesResponse_httpStatus :: Lens.Lens' AddListenerCertificatesResponse Prelude.Int
addListenerCertificatesResponse_httpStatus = Lens.lens (\AddListenerCertificatesResponse' {httpStatus} -> httpStatus) (\s@AddListenerCertificatesResponse' {} a -> s {httpStatus = a} :: AddListenerCertificatesResponse)

instance
  Prelude.NFData
    AddListenerCertificatesResponse
  where
  rnf AddListenerCertificatesResponse' {..} =
    Prelude.rnf certificates
      `Prelude.seq` Prelude.rnf httpStatus
