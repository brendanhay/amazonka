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
-- Module      : Amazonka.PaymentCryptography.GetParametersForExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the export token and the signing key certificate to initiate a
-- TR-34 key export from Amazon Web Services Payment Cryptography.
--
-- The signing key certificate signs the wrapped key under export within
-- the TR-34 key payload. The export token and signing key certificate must
-- be in place and operational before calling ExportKey. The export token
-- expires in 7 days. You can use the same export token to export multiple
-- keys from your service account.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   ExportKey
--
-- -   GetParametersForImport
module Amazonka.PaymentCryptography.GetParametersForExport
  ( -- * Creating a Request
    GetParametersForExport (..),
    newGetParametersForExport,

    -- * Request Lenses
    getParametersForExport_keyMaterialType,
    getParametersForExport_signingKeyAlgorithm,

    -- * Destructuring the Response
    GetParametersForExportResponse (..),
    newGetParametersForExportResponse,

    -- * Response Lenses
    getParametersForExportResponse_httpStatus,
    getParametersForExportResponse_exportToken,
    getParametersForExportResponse_parametersValidUntilTimestamp,
    getParametersForExportResponse_signingKeyAlgorithm,
    getParametersForExportResponse_signingKeyCertificate,
    getParametersForExportResponse_signingKeyCertificateChain,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetParametersForExport' smart constructor.
data GetParametersForExport = GetParametersForExport'
  { -- | The key block format type (for example, TR-34 or TR-31) to use during
    -- key material export. Export token is only required for a TR-34 key
    -- export, @TR34_KEY_BLOCK@. Export token is not required for TR-31 key
    -- export.
    keyMaterialType :: KeyMaterialType,
    -- | The signing key algorithm to generate a signing key certificate. This
    -- certificate signs the wrapped key under export within the TR-34 key
    -- block cryptogram. @RSA_2048@ is the only signing key algorithm allowed.
    signingKeyAlgorithm :: KeyAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParametersForExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyMaterialType', 'getParametersForExport_keyMaterialType' - The key block format type (for example, TR-34 or TR-31) to use during
-- key material export. Export token is only required for a TR-34 key
-- export, @TR34_KEY_BLOCK@. Export token is not required for TR-31 key
-- export.
--
-- 'signingKeyAlgorithm', 'getParametersForExport_signingKeyAlgorithm' - The signing key algorithm to generate a signing key certificate. This
-- certificate signs the wrapped key under export within the TR-34 key
-- block cryptogram. @RSA_2048@ is the only signing key algorithm allowed.
newGetParametersForExport ::
  -- | 'keyMaterialType'
  KeyMaterialType ->
  -- | 'signingKeyAlgorithm'
  KeyAlgorithm ->
  GetParametersForExport
newGetParametersForExport
  pKeyMaterialType_
  pSigningKeyAlgorithm_ =
    GetParametersForExport'
      { keyMaterialType =
          pKeyMaterialType_,
        signingKeyAlgorithm = pSigningKeyAlgorithm_
      }

-- | The key block format type (for example, TR-34 or TR-31) to use during
-- key material export. Export token is only required for a TR-34 key
-- export, @TR34_KEY_BLOCK@. Export token is not required for TR-31 key
-- export.
getParametersForExport_keyMaterialType :: Lens.Lens' GetParametersForExport KeyMaterialType
getParametersForExport_keyMaterialType = Lens.lens (\GetParametersForExport' {keyMaterialType} -> keyMaterialType) (\s@GetParametersForExport' {} a -> s {keyMaterialType = a} :: GetParametersForExport)

-- | The signing key algorithm to generate a signing key certificate. This
-- certificate signs the wrapped key under export within the TR-34 key
-- block cryptogram. @RSA_2048@ is the only signing key algorithm allowed.
getParametersForExport_signingKeyAlgorithm :: Lens.Lens' GetParametersForExport KeyAlgorithm
getParametersForExport_signingKeyAlgorithm = Lens.lens (\GetParametersForExport' {signingKeyAlgorithm} -> signingKeyAlgorithm) (\s@GetParametersForExport' {} a -> s {signingKeyAlgorithm = a} :: GetParametersForExport)

instance Core.AWSRequest GetParametersForExport where
  type
    AWSResponse GetParametersForExport =
      GetParametersForExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParametersForExportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ExportToken")
            Prelude.<*> (x Data..:> "ParametersValidUntilTimestamp")
            Prelude.<*> (x Data..:> "SigningKeyAlgorithm")
            Prelude.<*> (x Data..:> "SigningKeyCertificate")
            Prelude.<*> (x Data..:> "SigningKeyCertificateChain")
      )

instance Prelude.Hashable GetParametersForExport where
  hashWithSalt _salt GetParametersForExport' {..} =
    _salt
      `Prelude.hashWithSalt` keyMaterialType
      `Prelude.hashWithSalt` signingKeyAlgorithm

instance Prelude.NFData GetParametersForExport where
  rnf GetParametersForExport' {..} =
    Prelude.rnf keyMaterialType
      `Prelude.seq` Prelude.rnf signingKeyAlgorithm

instance Data.ToHeaders GetParametersForExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.GetParametersForExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetParametersForExport where
  toJSON GetParametersForExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyMaterialType" Data..= keyMaterialType),
            Prelude.Just
              ("SigningKeyAlgorithm" Data..= signingKeyAlgorithm)
          ]
      )

instance Data.ToPath GetParametersForExport where
  toPath = Prelude.const "/"

instance Data.ToQuery GetParametersForExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParametersForExportResponse' smart constructor.
data GetParametersForExportResponse = GetParametersForExportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The export token to initiate key export from Amazon Web Services Payment
    -- Cryptography. The export token expires after 7 days. You can use the
    -- same export token to export multiple keys from the same service account.
    exportToken :: Prelude.Text,
    -- | The validity period of the export token.
    parametersValidUntilTimestamp :: Data.POSIX,
    -- | The algorithm of the signing key certificate for use in TR-34 key block
    -- generation. @RSA_2048@ is the only signing key algorithm allowed.
    signingKeyAlgorithm :: KeyAlgorithm,
    -- | The signing key certificate of the public key for signature within the
    -- TR-34 key block cryptogram. The certificate expires after 7 days.
    signingKeyCertificate :: Data.Sensitive Prelude.Text,
    -- | The certificate chain that signed the signing key certificate. This is
    -- the root certificate authority (CA) within your service account.
    signingKeyCertificateChain :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParametersForExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getParametersForExportResponse_httpStatus' - The response's http status code.
--
-- 'exportToken', 'getParametersForExportResponse_exportToken' - The export token to initiate key export from Amazon Web Services Payment
-- Cryptography. The export token expires after 7 days. You can use the
-- same export token to export multiple keys from the same service account.
--
-- 'parametersValidUntilTimestamp', 'getParametersForExportResponse_parametersValidUntilTimestamp' - The validity period of the export token.
--
-- 'signingKeyAlgorithm', 'getParametersForExportResponse_signingKeyAlgorithm' - The algorithm of the signing key certificate for use in TR-34 key block
-- generation. @RSA_2048@ is the only signing key algorithm allowed.
--
-- 'signingKeyCertificate', 'getParametersForExportResponse_signingKeyCertificate' - The signing key certificate of the public key for signature within the
-- TR-34 key block cryptogram. The certificate expires after 7 days.
--
-- 'signingKeyCertificateChain', 'getParametersForExportResponse_signingKeyCertificateChain' - The certificate chain that signed the signing key certificate. This is
-- the root certificate authority (CA) within your service account.
newGetParametersForExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'exportToken'
  Prelude.Text ->
  -- | 'parametersValidUntilTimestamp'
  Prelude.UTCTime ->
  -- | 'signingKeyAlgorithm'
  KeyAlgorithm ->
  -- | 'signingKeyCertificate'
  Prelude.Text ->
  -- | 'signingKeyCertificateChain'
  Prelude.Text ->
  GetParametersForExportResponse
newGetParametersForExportResponse
  pHttpStatus_
  pExportToken_
  pParametersValidUntilTimestamp_
  pSigningKeyAlgorithm_
  pSigningKeyCertificate_
  pSigningKeyCertificateChain_ =
    GetParametersForExportResponse'
      { httpStatus =
          pHttpStatus_,
        exportToken = pExportToken_,
        parametersValidUntilTimestamp =
          Data._Time
            Lens.# pParametersValidUntilTimestamp_,
        signingKeyAlgorithm = pSigningKeyAlgorithm_,
        signingKeyCertificate =
          Data._Sensitive
            Lens.# pSigningKeyCertificate_,
        signingKeyCertificateChain =
          Data._Sensitive
            Lens.# pSigningKeyCertificateChain_
      }

-- | The response's http status code.
getParametersForExportResponse_httpStatus :: Lens.Lens' GetParametersForExportResponse Prelude.Int
getParametersForExportResponse_httpStatus = Lens.lens (\GetParametersForExportResponse' {httpStatus} -> httpStatus) (\s@GetParametersForExportResponse' {} a -> s {httpStatus = a} :: GetParametersForExportResponse)

-- | The export token to initiate key export from Amazon Web Services Payment
-- Cryptography. The export token expires after 7 days. You can use the
-- same export token to export multiple keys from the same service account.
getParametersForExportResponse_exportToken :: Lens.Lens' GetParametersForExportResponse Prelude.Text
getParametersForExportResponse_exportToken = Lens.lens (\GetParametersForExportResponse' {exportToken} -> exportToken) (\s@GetParametersForExportResponse' {} a -> s {exportToken = a} :: GetParametersForExportResponse)

-- | The validity period of the export token.
getParametersForExportResponse_parametersValidUntilTimestamp :: Lens.Lens' GetParametersForExportResponse Prelude.UTCTime
getParametersForExportResponse_parametersValidUntilTimestamp = Lens.lens (\GetParametersForExportResponse' {parametersValidUntilTimestamp} -> parametersValidUntilTimestamp) (\s@GetParametersForExportResponse' {} a -> s {parametersValidUntilTimestamp = a} :: GetParametersForExportResponse) Prelude.. Data._Time

-- | The algorithm of the signing key certificate for use in TR-34 key block
-- generation. @RSA_2048@ is the only signing key algorithm allowed.
getParametersForExportResponse_signingKeyAlgorithm :: Lens.Lens' GetParametersForExportResponse KeyAlgorithm
getParametersForExportResponse_signingKeyAlgorithm = Lens.lens (\GetParametersForExportResponse' {signingKeyAlgorithm} -> signingKeyAlgorithm) (\s@GetParametersForExportResponse' {} a -> s {signingKeyAlgorithm = a} :: GetParametersForExportResponse)

-- | The signing key certificate of the public key for signature within the
-- TR-34 key block cryptogram. The certificate expires after 7 days.
getParametersForExportResponse_signingKeyCertificate :: Lens.Lens' GetParametersForExportResponse Prelude.Text
getParametersForExportResponse_signingKeyCertificate = Lens.lens (\GetParametersForExportResponse' {signingKeyCertificate} -> signingKeyCertificate) (\s@GetParametersForExportResponse' {} a -> s {signingKeyCertificate = a} :: GetParametersForExportResponse) Prelude.. Data._Sensitive

-- | The certificate chain that signed the signing key certificate. This is
-- the root certificate authority (CA) within your service account.
getParametersForExportResponse_signingKeyCertificateChain :: Lens.Lens' GetParametersForExportResponse Prelude.Text
getParametersForExportResponse_signingKeyCertificateChain = Lens.lens (\GetParametersForExportResponse' {signingKeyCertificateChain} -> signingKeyCertificateChain) (\s@GetParametersForExportResponse' {} a -> s {signingKeyCertificateChain = a} :: GetParametersForExportResponse) Prelude.. Data._Sensitive

instance
  Prelude.NFData
    GetParametersForExportResponse
  where
  rnf GetParametersForExportResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exportToken
      `Prelude.seq` Prelude.rnf parametersValidUntilTimestamp
      `Prelude.seq` Prelude.rnf signingKeyAlgorithm
      `Prelude.seq` Prelude.rnf signingKeyCertificate
      `Prelude.seq` Prelude.rnf signingKeyCertificateChain
