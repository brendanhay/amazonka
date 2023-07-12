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
-- Module      : Amazonka.WorkSpacesWeb.GetTrustStoreCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the trust store certificate.
module Amazonka.WorkSpacesWeb.GetTrustStoreCertificate
  ( -- * Creating a Request
    GetTrustStoreCertificate (..),
    newGetTrustStoreCertificate,

    -- * Request Lenses
    getTrustStoreCertificate_thumbprint,
    getTrustStoreCertificate_trustStoreArn,

    -- * Destructuring the Response
    GetTrustStoreCertificateResponse (..),
    newGetTrustStoreCertificateResponse,

    -- * Response Lenses
    getTrustStoreCertificateResponse_certificate,
    getTrustStoreCertificateResponse_trustStoreArn,
    getTrustStoreCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetTrustStoreCertificate' smart constructor.
data GetTrustStoreCertificate = GetTrustStoreCertificate'
  { -- | The thumbprint of the trust store certificate.
    thumbprint :: Prelude.Text,
    -- | The ARN of the trust store certificate.
    trustStoreArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrustStoreCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thumbprint', 'getTrustStoreCertificate_thumbprint' - The thumbprint of the trust store certificate.
--
-- 'trustStoreArn', 'getTrustStoreCertificate_trustStoreArn' - The ARN of the trust store certificate.
newGetTrustStoreCertificate ::
  -- | 'thumbprint'
  Prelude.Text ->
  -- | 'trustStoreArn'
  Prelude.Text ->
  GetTrustStoreCertificate
newGetTrustStoreCertificate
  pThumbprint_
  pTrustStoreArn_ =
    GetTrustStoreCertificate'
      { thumbprint =
          pThumbprint_,
        trustStoreArn = pTrustStoreArn_
      }

-- | The thumbprint of the trust store certificate.
getTrustStoreCertificate_thumbprint :: Lens.Lens' GetTrustStoreCertificate Prelude.Text
getTrustStoreCertificate_thumbprint = Lens.lens (\GetTrustStoreCertificate' {thumbprint} -> thumbprint) (\s@GetTrustStoreCertificate' {} a -> s {thumbprint = a} :: GetTrustStoreCertificate)

-- | The ARN of the trust store certificate.
getTrustStoreCertificate_trustStoreArn :: Lens.Lens' GetTrustStoreCertificate Prelude.Text
getTrustStoreCertificate_trustStoreArn = Lens.lens (\GetTrustStoreCertificate' {trustStoreArn} -> trustStoreArn) (\s@GetTrustStoreCertificate' {} a -> s {trustStoreArn = a} :: GetTrustStoreCertificate)

instance Core.AWSRequest GetTrustStoreCertificate where
  type
    AWSResponse GetTrustStoreCertificate =
      GetTrustStoreCertificateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTrustStoreCertificateResponse'
            Prelude.<$> (x Data..?> "certificate")
            Prelude.<*> (x Data..?> "trustStoreArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTrustStoreCertificate where
  hashWithSalt _salt GetTrustStoreCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` thumbprint
      `Prelude.hashWithSalt` trustStoreArn

instance Prelude.NFData GetTrustStoreCertificate where
  rnf GetTrustStoreCertificate' {..} =
    Prelude.rnf thumbprint
      `Prelude.seq` Prelude.rnf trustStoreArn

instance Data.ToHeaders GetTrustStoreCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTrustStoreCertificate where
  toPath GetTrustStoreCertificate' {..} =
    Prelude.mconcat
      [ "/trustStores/",
        Data.toBS trustStoreArn,
        "/certificate"
      ]

instance Data.ToQuery GetTrustStoreCertificate where
  toQuery GetTrustStoreCertificate' {..} =
    Prelude.mconcat ["thumbprint" Data.=: thumbprint]

-- | /See:/ 'newGetTrustStoreCertificateResponse' smart constructor.
data GetTrustStoreCertificateResponse = GetTrustStoreCertificateResponse'
  { -- | The certificate of the trust store certificate.
    certificate :: Prelude.Maybe Certificate,
    -- | The ARN of the trust store certificate.
    trustStoreArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrustStoreCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'getTrustStoreCertificateResponse_certificate' - The certificate of the trust store certificate.
--
-- 'trustStoreArn', 'getTrustStoreCertificateResponse_trustStoreArn' - The ARN of the trust store certificate.
--
-- 'httpStatus', 'getTrustStoreCertificateResponse_httpStatus' - The response's http status code.
newGetTrustStoreCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTrustStoreCertificateResponse
newGetTrustStoreCertificateResponse pHttpStatus_ =
  GetTrustStoreCertificateResponse'
    { certificate =
        Prelude.Nothing,
      trustStoreArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The certificate of the trust store certificate.
getTrustStoreCertificateResponse_certificate :: Lens.Lens' GetTrustStoreCertificateResponse (Prelude.Maybe Certificate)
getTrustStoreCertificateResponse_certificate = Lens.lens (\GetTrustStoreCertificateResponse' {certificate} -> certificate) (\s@GetTrustStoreCertificateResponse' {} a -> s {certificate = a} :: GetTrustStoreCertificateResponse)

-- | The ARN of the trust store certificate.
getTrustStoreCertificateResponse_trustStoreArn :: Lens.Lens' GetTrustStoreCertificateResponse (Prelude.Maybe Prelude.Text)
getTrustStoreCertificateResponse_trustStoreArn = Lens.lens (\GetTrustStoreCertificateResponse' {trustStoreArn} -> trustStoreArn) (\s@GetTrustStoreCertificateResponse' {} a -> s {trustStoreArn = a} :: GetTrustStoreCertificateResponse)

-- | The response's http status code.
getTrustStoreCertificateResponse_httpStatus :: Lens.Lens' GetTrustStoreCertificateResponse Prelude.Int
getTrustStoreCertificateResponse_httpStatus = Lens.lens (\GetTrustStoreCertificateResponse' {httpStatus} -> httpStatus) (\s@GetTrustStoreCertificateResponse' {} a -> s {httpStatus = a} :: GetTrustStoreCertificateResponse)

instance
  Prelude.NFData
    GetTrustStoreCertificateResponse
  where
  rnf GetTrustStoreCertificateResponse' {..} =
    Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf trustStoreArn
      `Prelude.seq` Prelude.rnf httpStatus
