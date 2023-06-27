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
-- Module      : Amazonka.PaymentCryptography.GetParametersForImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the import token and the wrapping key certificate to initiate a
-- TR-34 key import into Amazon Web Services Payment Cryptography.
--
-- The wrapping key certificate wraps the key under import within the TR-34
-- key payload. The import token and wrapping key certificate must be in
-- place and operational before calling ImportKey. The import token expires
-- in 7 days. The same import token can be used to import multiple keys
-- into your service account.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   GetParametersForExport
--
-- -   ImportKey
module Amazonka.PaymentCryptography.GetParametersForImport
  ( -- * Creating a Request
    GetParametersForImport (..),
    newGetParametersForImport,

    -- * Request Lenses
    getParametersForImport_keyMaterialType,
    getParametersForImport_wrappingKeyAlgorithm,

    -- * Destructuring the Response
    GetParametersForImportResponse (..),
    newGetParametersForImportResponse,

    -- * Response Lenses
    getParametersForImportResponse_httpStatus,
    getParametersForImportResponse_importToken,
    getParametersForImportResponse_parametersValidUntilTimestamp,
    getParametersForImportResponse_wrappingKeyAlgorithm,
    getParametersForImportResponse_wrappingKeyCertificate,
    getParametersForImportResponse_wrappingKeyCertificateChain,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetParametersForImport' smart constructor.
data GetParametersForImport = GetParametersForImport'
  { -- | The key block format type such as TR-34 or TR-31 to use during key
    -- material import. Import token is only required for TR-34 key import
    -- @TR34_KEY_BLOCK@. Import token is not required for TR-31 key import.
    keyMaterialType :: KeyMaterialType,
    -- | The wrapping key algorithm to generate a wrapping key certificate. This
    -- certificate wraps the key under import within the TR-34 key block
    -- cryptogram. @RSA_2048@ is the only wrapping key algorithm allowed.
    wrappingKeyAlgorithm :: KeyAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParametersForImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyMaterialType', 'getParametersForImport_keyMaterialType' - The key block format type such as TR-34 or TR-31 to use during key
-- material import. Import token is only required for TR-34 key import
-- @TR34_KEY_BLOCK@. Import token is not required for TR-31 key import.
--
-- 'wrappingKeyAlgorithm', 'getParametersForImport_wrappingKeyAlgorithm' - The wrapping key algorithm to generate a wrapping key certificate. This
-- certificate wraps the key under import within the TR-34 key block
-- cryptogram. @RSA_2048@ is the only wrapping key algorithm allowed.
newGetParametersForImport ::
  -- | 'keyMaterialType'
  KeyMaterialType ->
  -- | 'wrappingKeyAlgorithm'
  KeyAlgorithm ->
  GetParametersForImport
newGetParametersForImport
  pKeyMaterialType_
  pWrappingKeyAlgorithm_ =
    GetParametersForImport'
      { keyMaterialType =
          pKeyMaterialType_,
        wrappingKeyAlgorithm = pWrappingKeyAlgorithm_
      }

-- | The key block format type such as TR-34 or TR-31 to use during key
-- material import. Import token is only required for TR-34 key import
-- @TR34_KEY_BLOCK@. Import token is not required for TR-31 key import.
getParametersForImport_keyMaterialType :: Lens.Lens' GetParametersForImport KeyMaterialType
getParametersForImport_keyMaterialType = Lens.lens (\GetParametersForImport' {keyMaterialType} -> keyMaterialType) (\s@GetParametersForImport' {} a -> s {keyMaterialType = a} :: GetParametersForImport)

-- | The wrapping key algorithm to generate a wrapping key certificate. This
-- certificate wraps the key under import within the TR-34 key block
-- cryptogram. @RSA_2048@ is the only wrapping key algorithm allowed.
getParametersForImport_wrappingKeyAlgorithm :: Lens.Lens' GetParametersForImport KeyAlgorithm
getParametersForImport_wrappingKeyAlgorithm = Lens.lens (\GetParametersForImport' {wrappingKeyAlgorithm} -> wrappingKeyAlgorithm) (\s@GetParametersForImport' {} a -> s {wrappingKeyAlgorithm = a} :: GetParametersForImport)

instance Core.AWSRequest GetParametersForImport where
  type
    AWSResponse GetParametersForImport =
      GetParametersForImportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParametersForImportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ImportToken")
            Prelude.<*> (x Data..:> "ParametersValidUntilTimestamp")
            Prelude.<*> (x Data..:> "WrappingKeyAlgorithm")
            Prelude.<*> (x Data..:> "WrappingKeyCertificate")
            Prelude.<*> (x Data..:> "WrappingKeyCertificateChain")
      )

instance Prelude.Hashable GetParametersForImport where
  hashWithSalt _salt GetParametersForImport' {..} =
    _salt
      `Prelude.hashWithSalt` keyMaterialType
      `Prelude.hashWithSalt` wrappingKeyAlgorithm

instance Prelude.NFData GetParametersForImport where
  rnf GetParametersForImport' {..} =
    Prelude.rnf keyMaterialType
      `Prelude.seq` Prelude.rnf wrappingKeyAlgorithm

instance Data.ToHeaders GetParametersForImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.GetParametersForImport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetParametersForImport where
  toJSON GetParametersForImport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("KeyMaterialType" Data..= keyMaterialType),
            Prelude.Just
              ( "WrappingKeyAlgorithm"
                  Data..= wrappingKeyAlgorithm
              )
          ]
      )

instance Data.ToPath GetParametersForImport where
  toPath = Prelude.const "/"

instance Data.ToQuery GetParametersForImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParametersForImportResponse' smart constructor.
data GetParametersForImportResponse = GetParametersForImportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The import token to initiate key import into Amazon Web Services Payment
    -- Cryptography. The import token expires after 7 days. You can use the
    -- same import token to import multiple keys to the same service account.
    importToken :: Prelude.Text,
    -- | The validity period of the import token.
    parametersValidUntilTimestamp :: Data.POSIX,
    -- | The algorithm of the wrapping key for use within TR-34 key block.
    -- @RSA_2048@ is the only wrapping key algorithm allowed.
    wrappingKeyAlgorithm :: KeyAlgorithm,
    -- | The wrapping key certificate of the wrapping key for use within the
    -- TR-34 key block. The certificate expires in 7 days.
    wrappingKeyCertificate :: Data.Sensitive Prelude.Text,
    -- | The Amazon Web Services Payment Cryptography certificate chain that
    -- signed the wrapping key certificate. This is the root certificate
    -- authority (CA) within your service account.
    wrappingKeyCertificateChain :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParametersForImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getParametersForImportResponse_httpStatus' - The response's http status code.
--
-- 'importToken', 'getParametersForImportResponse_importToken' - The import token to initiate key import into Amazon Web Services Payment
-- Cryptography. The import token expires after 7 days. You can use the
-- same import token to import multiple keys to the same service account.
--
-- 'parametersValidUntilTimestamp', 'getParametersForImportResponse_parametersValidUntilTimestamp' - The validity period of the import token.
--
-- 'wrappingKeyAlgorithm', 'getParametersForImportResponse_wrappingKeyAlgorithm' - The algorithm of the wrapping key for use within TR-34 key block.
-- @RSA_2048@ is the only wrapping key algorithm allowed.
--
-- 'wrappingKeyCertificate', 'getParametersForImportResponse_wrappingKeyCertificate' - The wrapping key certificate of the wrapping key for use within the
-- TR-34 key block. The certificate expires in 7 days.
--
-- 'wrappingKeyCertificateChain', 'getParametersForImportResponse_wrappingKeyCertificateChain' - The Amazon Web Services Payment Cryptography certificate chain that
-- signed the wrapping key certificate. This is the root certificate
-- authority (CA) within your service account.
newGetParametersForImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'importToken'
  Prelude.Text ->
  -- | 'parametersValidUntilTimestamp'
  Prelude.UTCTime ->
  -- | 'wrappingKeyAlgorithm'
  KeyAlgorithm ->
  -- | 'wrappingKeyCertificate'
  Prelude.Text ->
  -- | 'wrappingKeyCertificateChain'
  Prelude.Text ->
  GetParametersForImportResponse
newGetParametersForImportResponse
  pHttpStatus_
  pImportToken_
  pParametersValidUntilTimestamp_
  pWrappingKeyAlgorithm_
  pWrappingKeyCertificate_
  pWrappingKeyCertificateChain_ =
    GetParametersForImportResponse'
      { httpStatus =
          pHttpStatus_,
        importToken = pImportToken_,
        parametersValidUntilTimestamp =
          Data._Time
            Lens.# pParametersValidUntilTimestamp_,
        wrappingKeyAlgorithm =
          pWrappingKeyAlgorithm_,
        wrappingKeyCertificate =
          Data._Sensitive
            Lens.# pWrappingKeyCertificate_,
        wrappingKeyCertificateChain =
          Data._Sensitive
            Lens.# pWrappingKeyCertificateChain_
      }

-- | The response's http status code.
getParametersForImportResponse_httpStatus :: Lens.Lens' GetParametersForImportResponse Prelude.Int
getParametersForImportResponse_httpStatus = Lens.lens (\GetParametersForImportResponse' {httpStatus} -> httpStatus) (\s@GetParametersForImportResponse' {} a -> s {httpStatus = a} :: GetParametersForImportResponse)

-- | The import token to initiate key import into Amazon Web Services Payment
-- Cryptography. The import token expires after 7 days. You can use the
-- same import token to import multiple keys to the same service account.
getParametersForImportResponse_importToken :: Lens.Lens' GetParametersForImportResponse Prelude.Text
getParametersForImportResponse_importToken = Lens.lens (\GetParametersForImportResponse' {importToken} -> importToken) (\s@GetParametersForImportResponse' {} a -> s {importToken = a} :: GetParametersForImportResponse)

-- | The validity period of the import token.
getParametersForImportResponse_parametersValidUntilTimestamp :: Lens.Lens' GetParametersForImportResponse Prelude.UTCTime
getParametersForImportResponse_parametersValidUntilTimestamp = Lens.lens (\GetParametersForImportResponse' {parametersValidUntilTimestamp} -> parametersValidUntilTimestamp) (\s@GetParametersForImportResponse' {} a -> s {parametersValidUntilTimestamp = a} :: GetParametersForImportResponse) Prelude.. Data._Time

-- | The algorithm of the wrapping key for use within TR-34 key block.
-- @RSA_2048@ is the only wrapping key algorithm allowed.
getParametersForImportResponse_wrappingKeyAlgorithm :: Lens.Lens' GetParametersForImportResponse KeyAlgorithm
getParametersForImportResponse_wrappingKeyAlgorithm = Lens.lens (\GetParametersForImportResponse' {wrappingKeyAlgorithm} -> wrappingKeyAlgorithm) (\s@GetParametersForImportResponse' {} a -> s {wrappingKeyAlgorithm = a} :: GetParametersForImportResponse)

-- | The wrapping key certificate of the wrapping key for use within the
-- TR-34 key block. The certificate expires in 7 days.
getParametersForImportResponse_wrappingKeyCertificate :: Lens.Lens' GetParametersForImportResponse Prelude.Text
getParametersForImportResponse_wrappingKeyCertificate = Lens.lens (\GetParametersForImportResponse' {wrappingKeyCertificate} -> wrappingKeyCertificate) (\s@GetParametersForImportResponse' {} a -> s {wrappingKeyCertificate = a} :: GetParametersForImportResponse) Prelude.. Data._Sensitive

-- | The Amazon Web Services Payment Cryptography certificate chain that
-- signed the wrapping key certificate. This is the root certificate
-- authority (CA) within your service account.
getParametersForImportResponse_wrappingKeyCertificateChain :: Lens.Lens' GetParametersForImportResponse Prelude.Text
getParametersForImportResponse_wrappingKeyCertificateChain = Lens.lens (\GetParametersForImportResponse' {wrappingKeyCertificateChain} -> wrappingKeyCertificateChain) (\s@GetParametersForImportResponse' {} a -> s {wrappingKeyCertificateChain = a} :: GetParametersForImportResponse) Prelude.. Data._Sensitive

instance
  Prelude.NFData
    GetParametersForImportResponse
  where
  rnf GetParametersForImportResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importToken
      `Prelude.seq` Prelude.rnf parametersValidUntilTimestamp
      `Prelude.seq` Prelude.rnf wrappingKeyAlgorithm
      `Prelude.seq` Prelude.rnf wrappingKeyCertificate
      `Prelude.seq` Prelude.rnf wrappingKeyCertificateChain
