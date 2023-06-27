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
-- Module      : Amazonka.PaymentCryptography.GetPublicKeyCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the public key certificate of the asymmetric key pair that exists
-- within Amazon Web Services Payment Cryptography.
--
-- Unlike the private key of an asymmetric key, which never leaves Amazon
-- Web Services Payment Cryptography unencrypted, callers with
-- @GetPublicKeyCertificate@ permission can download the public key
-- certificate of the asymmetric key. You can share the public key
-- certificate to allow others to encrypt messages and verify signatures
-- outside of Amazon Web Services Payment Cryptography
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
module Amazonka.PaymentCryptography.GetPublicKeyCertificate
  ( -- * Creating a Request
    GetPublicKeyCertificate (..),
    newGetPublicKeyCertificate,

    -- * Request Lenses
    getPublicKeyCertificate_keyIdentifier,

    -- * Destructuring the Response
    GetPublicKeyCertificateResponse (..),
    newGetPublicKeyCertificateResponse,

    -- * Response Lenses
    getPublicKeyCertificateResponse_httpStatus,
    getPublicKeyCertificateResponse_keyCertificate,
    getPublicKeyCertificateResponse_keyCertificateChain,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPublicKeyCertificate' smart constructor.
data GetPublicKeyCertificate = GetPublicKeyCertificate'
  { -- | The @KeyARN@ of the asymmetric key pair.
    keyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPublicKeyCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyIdentifier', 'getPublicKeyCertificate_keyIdentifier' - The @KeyARN@ of the asymmetric key pair.
newGetPublicKeyCertificate ::
  -- | 'keyIdentifier'
  Prelude.Text ->
  GetPublicKeyCertificate
newGetPublicKeyCertificate pKeyIdentifier_ =
  GetPublicKeyCertificate'
    { keyIdentifier =
        pKeyIdentifier_
    }

-- | The @KeyARN@ of the asymmetric key pair.
getPublicKeyCertificate_keyIdentifier :: Lens.Lens' GetPublicKeyCertificate Prelude.Text
getPublicKeyCertificate_keyIdentifier = Lens.lens (\GetPublicKeyCertificate' {keyIdentifier} -> keyIdentifier) (\s@GetPublicKeyCertificate' {} a -> s {keyIdentifier = a} :: GetPublicKeyCertificate)

instance Core.AWSRequest GetPublicKeyCertificate where
  type
    AWSResponse GetPublicKeyCertificate =
      GetPublicKeyCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPublicKeyCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyCertificate")
            Prelude.<*> (x Data..:> "KeyCertificateChain")
      )

instance Prelude.Hashable GetPublicKeyCertificate where
  hashWithSalt _salt GetPublicKeyCertificate' {..} =
    _salt `Prelude.hashWithSalt` keyIdentifier

instance Prelude.NFData GetPublicKeyCertificate where
  rnf GetPublicKeyCertificate' {..} =
    Prelude.rnf keyIdentifier

instance Data.ToHeaders GetPublicKeyCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.GetPublicKeyCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPublicKeyCertificate where
  toJSON GetPublicKeyCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyIdentifier" Data..= keyIdentifier)
          ]
      )

instance Data.ToPath GetPublicKeyCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPublicKeyCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPublicKeyCertificateResponse' smart constructor.
data GetPublicKeyCertificateResponse = GetPublicKeyCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The public key component of the asymmetric key pair in a certificate
    -- (PEM) format. It is signed by the root certificate authority (CA) within
    -- your service account. The certificate expires in 90 days.
    keyCertificate :: Data.Sensitive Prelude.Text,
    -- | The certificate chain that signed the public key certificate of the
    -- asymmetric key pair. This is the root certificate authority (CA) within
    -- your service account.
    keyCertificateChain :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPublicKeyCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getPublicKeyCertificateResponse_httpStatus' - The response's http status code.
--
-- 'keyCertificate', 'getPublicKeyCertificateResponse_keyCertificate' - The public key component of the asymmetric key pair in a certificate
-- (PEM) format. It is signed by the root certificate authority (CA) within
-- your service account. The certificate expires in 90 days.
--
-- 'keyCertificateChain', 'getPublicKeyCertificateResponse_keyCertificateChain' - The certificate chain that signed the public key certificate of the
-- asymmetric key pair. This is the root certificate authority (CA) within
-- your service account.
newGetPublicKeyCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyCertificate'
  Prelude.Text ->
  -- | 'keyCertificateChain'
  Prelude.Text ->
  GetPublicKeyCertificateResponse
newGetPublicKeyCertificateResponse
  pHttpStatus_
  pKeyCertificate_
  pKeyCertificateChain_ =
    GetPublicKeyCertificateResponse'
      { httpStatus =
          pHttpStatus_,
        keyCertificate =
          Data._Sensitive Lens.# pKeyCertificate_,
        keyCertificateChain =
          Data._Sensitive
            Lens.# pKeyCertificateChain_
      }

-- | The response's http status code.
getPublicKeyCertificateResponse_httpStatus :: Lens.Lens' GetPublicKeyCertificateResponse Prelude.Int
getPublicKeyCertificateResponse_httpStatus = Lens.lens (\GetPublicKeyCertificateResponse' {httpStatus} -> httpStatus) (\s@GetPublicKeyCertificateResponse' {} a -> s {httpStatus = a} :: GetPublicKeyCertificateResponse)

-- | The public key component of the asymmetric key pair in a certificate
-- (PEM) format. It is signed by the root certificate authority (CA) within
-- your service account. The certificate expires in 90 days.
getPublicKeyCertificateResponse_keyCertificate :: Lens.Lens' GetPublicKeyCertificateResponse Prelude.Text
getPublicKeyCertificateResponse_keyCertificate = Lens.lens (\GetPublicKeyCertificateResponse' {keyCertificate} -> keyCertificate) (\s@GetPublicKeyCertificateResponse' {} a -> s {keyCertificate = a} :: GetPublicKeyCertificateResponse) Prelude.. Data._Sensitive

-- | The certificate chain that signed the public key certificate of the
-- asymmetric key pair. This is the root certificate authority (CA) within
-- your service account.
getPublicKeyCertificateResponse_keyCertificateChain :: Lens.Lens' GetPublicKeyCertificateResponse Prelude.Text
getPublicKeyCertificateResponse_keyCertificateChain = Lens.lens (\GetPublicKeyCertificateResponse' {keyCertificateChain} -> keyCertificateChain) (\s@GetPublicKeyCertificateResponse' {} a -> s {keyCertificateChain = a} :: GetPublicKeyCertificateResponse) Prelude.. Data._Sensitive

instance
  Prelude.NFData
    GetPublicKeyCertificateResponse
  where
  rnf GetPublicKeyCertificateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyCertificate
      `Prelude.seq` Prelude.rnf keyCertificateChain
