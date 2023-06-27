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
-- Module      : Amazonka.KMS.GetParametersForImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the public key and an import token you need to import or
-- reimport key material for a KMS key.
--
-- By default, KMS keys are created with key material that KMS generates.
-- This operation supports
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing key material>,
-- an advanced feature that lets you generate and import the cryptographic
-- key material for a KMS key. For more information about importing key
-- material into KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing key material>
-- in the /Key Management Service Developer Guide/.
--
-- Before calling @GetParametersForImport@, use the CreateKey operation
-- with an @Origin@ value of @EXTERNAL@ to create a KMS key with no key
-- material. You can import key material for a symmetric encryption KMS
-- key, HMAC KMS key, asymmetric encryption KMS key, or asymmetric signing
-- KMS key. You can also import key material into a
-- <kms/latest/developerguide/multi-region-keys-overview.html multi-Region key>
-- of any supported type. However, you can\'t import key material into a
-- KMS key in a
-- <kms/latest/developerguide/custom-key-store-overview.html custom key store>.
-- You can also use @GetParametersForImport@ to get a public key and import
-- token to
-- <kms/latest/developerguide/importing-keys.html#reimport-key-material reimport the original key material>
-- into a KMS key whose key material expired or was deleted.
--
-- @GetParametersForImport@ returns the items that you need to import your
-- key material.
--
-- -   The public key (or \"wrapping key\") of an RSA key pair that KMS
--     generates.
--
--     You will use this public key to encrypt (\"wrap\") your key material
--     while it\'s in transit to KMS.
--
-- -   A import token that ensures that KMS can decrypt your key material
--     and associate it with the correct KMS key.
--
-- The public key and its import token are permanently linked and must be
-- used together. Each public key and import token set is valid for 24
-- hours. The expiration date and time appear in the @ParametersValidTo@
-- field in the @GetParametersForImport@ response. You cannot use an
-- expired public key or import token in an ImportKeyMaterial request. If
-- your key and token expire, send another @GetParametersForImport@
-- request.
--
-- @GetParametersForImport@ requires the following information:
--
-- -   The key ID of the KMS key for which you are importing the key
--     material.
--
-- -   The key spec of the public key (\"wrapping key\") that you will use
--     to encrypt your key material during import.
--
-- -   The wrapping algorithm that you will use with the public key to
--     encrypt your key material.
--
-- You can use the same or a different public key spec and wrapping
-- algorithm each time you import or reimport the same key material.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GetParametersForImport>
-- (key policy)
--
-- __Related operations:__
--
-- -   ImportKeyMaterial
--
-- -   DeleteImportedKeyMaterial
module Amazonka.KMS.GetParametersForImport
  ( -- * Creating a Request
    GetParametersForImport (..),
    newGetParametersForImport,

    -- * Request Lenses
    getParametersForImport_keyId,
    getParametersForImport_wrappingAlgorithm,
    getParametersForImport_wrappingKeySpec,

    -- * Destructuring the Response
    GetParametersForImportResponse (..),
    newGetParametersForImportResponse,

    -- * Response Lenses
    getParametersForImportResponse_importToken,
    getParametersForImportResponse_keyId,
    getParametersForImportResponse_parametersValidTo,
    getParametersForImportResponse_publicKey,
    getParametersForImportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetParametersForImport' smart constructor.
data GetParametersForImport = GetParametersForImport'
  { -- | The identifier of the KMS key that will be associated with the imported
    -- key material. The @Origin@ of the KMS key must be @EXTERNAL@.
    --
    -- All KMS key types are supported, including multi-Region keys. However,
    -- you cannot import key material into a KMS key in a custom key store.
    --
    -- Specify the key ID or key ARN of the KMS key.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text,
    -- | The algorithm you will use with the RSA public key (@PublicKey@) in the
    -- response to protect your key material during import. For more
    -- information, see
    -- <kms/latest/developerguide/importing-keys-get-public-key-and-token.html#select-wrapping-algorithm Select a wrapping algorithm>
    -- in the /Key Management Service Developer Guide/.
    --
    -- For RSA_AES wrapping algorithms, you encrypt your key material with an
    -- AES key that you generate, then encrypt your AES key with the RSA public
    -- key from KMS. For RSAES wrapping algorithms, you encrypt your key
    -- material directly with the RSA public key from KMS.
    --
    -- The wrapping algorithms that you can use depend on the type of key
    -- material that you are importing. To import an RSA private key, you must
    -- use an RSA_AES wrapping algorithm.
    --
    -- -   __RSA_AES_KEY_WRAP_SHA_256__ — Supported for wrapping RSA and ECC
    --     key material.
    --
    -- -   __RSA_AES_KEY_WRAP_SHA_1__ — Supported for wrapping RSA and ECC key
    --     material.
    --
    -- -   __RSAES_OAEP_SHA_256__ — Supported for all types of key material,
    --     except RSA key material (private key).
    --
    --     You cannot use the RSAES_OAEP_SHA_256 wrapping algorithm with the
    --     RSA_2048 wrapping key spec to wrap ECC_NIST_P521 key material.
    --
    -- -   __RSAES_OAEP_SHA_1__ — Supported for all types of key material,
    --     except RSA key material (private key).
    --
    --     You cannot use the RSAES_OAEP_SHA_1 wrapping algorithm with the
    --     RSA_2048 wrapping key spec to wrap ECC_NIST_P521 key material.
    --
    -- -   __RSAES_PKCS1_V1_5__ (Deprecated) — Supported only for symmetric
    --     encryption key material (and only in legacy mode).
    wrappingAlgorithm :: AlgorithmSpec,
    -- | The type of RSA public key to return in the response. You will use this
    -- wrapping key with the specified wrapping algorithm to protect your key
    -- material during import.
    --
    -- Use the longest RSA wrapping key that is practical.
    --
    -- You cannot use an RSA_2048 public key to directly wrap an ECC_NIST_P521
    -- private key. Instead, use an RSA_AES wrapping algorithm or choose a
    -- longer RSA public key.
    wrappingKeySpec :: WrappingKeySpec
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
-- 'keyId', 'getParametersForImport_keyId' - The identifier of the KMS key that will be associated with the imported
-- key material. The @Origin@ of the KMS key must be @EXTERNAL@.
--
-- All KMS key types are supported, including multi-Region keys. However,
-- you cannot import key material into a KMS key in a custom key store.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
--
-- 'wrappingAlgorithm', 'getParametersForImport_wrappingAlgorithm' - The algorithm you will use with the RSA public key (@PublicKey@) in the
-- response to protect your key material during import. For more
-- information, see
-- <kms/latest/developerguide/importing-keys-get-public-key-and-token.html#select-wrapping-algorithm Select a wrapping algorithm>
-- in the /Key Management Service Developer Guide/.
--
-- For RSA_AES wrapping algorithms, you encrypt your key material with an
-- AES key that you generate, then encrypt your AES key with the RSA public
-- key from KMS. For RSAES wrapping algorithms, you encrypt your key
-- material directly with the RSA public key from KMS.
--
-- The wrapping algorithms that you can use depend on the type of key
-- material that you are importing. To import an RSA private key, you must
-- use an RSA_AES wrapping algorithm.
--
-- -   __RSA_AES_KEY_WRAP_SHA_256__ — Supported for wrapping RSA and ECC
--     key material.
--
-- -   __RSA_AES_KEY_WRAP_SHA_1__ — Supported for wrapping RSA and ECC key
--     material.
--
-- -   __RSAES_OAEP_SHA_256__ — Supported for all types of key material,
--     except RSA key material (private key).
--
--     You cannot use the RSAES_OAEP_SHA_256 wrapping algorithm with the
--     RSA_2048 wrapping key spec to wrap ECC_NIST_P521 key material.
--
-- -   __RSAES_OAEP_SHA_1__ — Supported for all types of key material,
--     except RSA key material (private key).
--
--     You cannot use the RSAES_OAEP_SHA_1 wrapping algorithm with the
--     RSA_2048 wrapping key spec to wrap ECC_NIST_P521 key material.
--
-- -   __RSAES_PKCS1_V1_5__ (Deprecated) — Supported only for symmetric
--     encryption key material (and only in legacy mode).
--
-- 'wrappingKeySpec', 'getParametersForImport_wrappingKeySpec' - The type of RSA public key to return in the response. You will use this
-- wrapping key with the specified wrapping algorithm to protect your key
-- material during import.
--
-- Use the longest RSA wrapping key that is practical.
--
-- You cannot use an RSA_2048 public key to directly wrap an ECC_NIST_P521
-- private key. Instead, use an RSA_AES wrapping algorithm or choose a
-- longer RSA public key.
newGetParametersForImport ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'wrappingAlgorithm'
  AlgorithmSpec ->
  -- | 'wrappingKeySpec'
  WrappingKeySpec ->
  GetParametersForImport
newGetParametersForImport
  pKeyId_
  pWrappingAlgorithm_
  pWrappingKeySpec_ =
    GetParametersForImport'
      { keyId = pKeyId_,
        wrappingAlgorithm = pWrappingAlgorithm_,
        wrappingKeySpec = pWrappingKeySpec_
      }

-- | The identifier of the KMS key that will be associated with the imported
-- key material. The @Origin@ of the KMS key must be @EXTERNAL@.
--
-- All KMS key types are supported, including multi-Region keys. However,
-- you cannot import key material into a KMS key in a custom key store.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
getParametersForImport_keyId :: Lens.Lens' GetParametersForImport Prelude.Text
getParametersForImport_keyId = Lens.lens (\GetParametersForImport' {keyId} -> keyId) (\s@GetParametersForImport' {} a -> s {keyId = a} :: GetParametersForImport)

-- | The algorithm you will use with the RSA public key (@PublicKey@) in the
-- response to protect your key material during import. For more
-- information, see
-- <kms/latest/developerguide/importing-keys-get-public-key-and-token.html#select-wrapping-algorithm Select a wrapping algorithm>
-- in the /Key Management Service Developer Guide/.
--
-- For RSA_AES wrapping algorithms, you encrypt your key material with an
-- AES key that you generate, then encrypt your AES key with the RSA public
-- key from KMS. For RSAES wrapping algorithms, you encrypt your key
-- material directly with the RSA public key from KMS.
--
-- The wrapping algorithms that you can use depend on the type of key
-- material that you are importing. To import an RSA private key, you must
-- use an RSA_AES wrapping algorithm.
--
-- -   __RSA_AES_KEY_WRAP_SHA_256__ — Supported for wrapping RSA and ECC
--     key material.
--
-- -   __RSA_AES_KEY_WRAP_SHA_1__ — Supported for wrapping RSA and ECC key
--     material.
--
-- -   __RSAES_OAEP_SHA_256__ — Supported for all types of key material,
--     except RSA key material (private key).
--
--     You cannot use the RSAES_OAEP_SHA_256 wrapping algorithm with the
--     RSA_2048 wrapping key spec to wrap ECC_NIST_P521 key material.
--
-- -   __RSAES_OAEP_SHA_1__ — Supported for all types of key material,
--     except RSA key material (private key).
--
--     You cannot use the RSAES_OAEP_SHA_1 wrapping algorithm with the
--     RSA_2048 wrapping key spec to wrap ECC_NIST_P521 key material.
--
-- -   __RSAES_PKCS1_V1_5__ (Deprecated) — Supported only for symmetric
--     encryption key material (and only in legacy mode).
getParametersForImport_wrappingAlgorithm :: Lens.Lens' GetParametersForImport AlgorithmSpec
getParametersForImport_wrappingAlgorithm = Lens.lens (\GetParametersForImport' {wrappingAlgorithm} -> wrappingAlgorithm) (\s@GetParametersForImport' {} a -> s {wrappingAlgorithm = a} :: GetParametersForImport)

-- | The type of RSA public key to return in the response. You will use this
-- wrapping key with the specified wrapping algorithm to protect your key
-- material during import.
--
-- Use the longest RSA wrapping key that is practical.
--
-- You cannot use an RSA_2048 public key to directly wrap an ECC_NIST_P521
-- private key. Instead, use an RSA_AES wrapping algorithm or choose a
-- longer RSA public key.
getParametersForImport_wrappingKeySpec :: Lens.Lens' GetParametersForImport WrappingKeySpec
getParametersForImport_wrappingKeySpec = Lens.lens (\GetParametersForImport' {wrappingKeySpec} -> wrappingKeySpec) (\s@GetParametersForImport' {} a -> s {wrappingKeySpec = a} :: GetParametersForImport)

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
            Prelude.<$> (x Data..?> "ImportToken")
            Prelude.<*> (x Data..?> "KeyId")
            Prelude.<*> (x Data..?> "ParametersValidTo")
            Prelude.<*> (x Data..?> "PublicKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetParametersForImport where
  hashWithSalt _salt GetParametersForImport' {..} =
    _salt
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` wrappingAlgorithm
      `Prelude.hashWithSalt` wrappingKeySpec

instance Prelude.NFData GetParametersForImport where
  rnf GetParametersForImport' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf wrappingAlgorithm
      `Prelude.seq` Prelude.rnf wrappingKeySpec

instance Data.ToHeaders GetParametersForImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.GetParametersForImport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetParametersForImport where
  toJSON GetParametersForImport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just
              ("WrappingAlgorithm" Data..= wrappingAlgorithm),
            Prelude.Just
              ("WrappingKeySpec" Data..= wrappingKeySpec)
          ]
      )

instance Data.ToPath GetParametersForImport where
  toPath = Prelude.const "/"

instance Data.ToQuery GetParametersForImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParametersForImportResponse' smart constructor.
data GetParametersForImportResponse = GetParametersForImportResponse'
  { -- | The import token to send in a subsequent ImportKeyMaterial request.
    importToken :: Prelude.Maybe Data.Base64,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the KMS key to use in a subsequent ImportKeyMaterial request. This is
    -- the same KMS key specified in the @GetParametersForImport@ request.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the import token and public key are no longer valid.
    -- After this time, you cannot use them to make an ImportKeyMaterial
    -- request and you must send another @GetParametersForImport@ request to
    -- get new ones.
    parametersValidTo :: Prelude.Maybe Data.POSIX,
    -- | The public key to use to encrypt the key material before importing it
    -- with ImportKeyMaterial.
    publicKey :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'importToken', 'getParametersForImportResponse_importToken' - The import token to send in a subsequent ImportKeyMaterial request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyId', 'getParametersForImportResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key to use in a subsequent ImportKeyMaterial request. This is
-- the same KMS key specified in the @GetParametersForImport@ request.
--
-- 'parametersValidTo', 'getParametersForImportResponse_parametersValidTo' - The time at which the import token and public key are no longer valid.
-- After this time, you cannot use them to make an ImportKeyMaterial
-- request and you must send another @GetParametersForImport@ request to
-- get new ones.
--
-- 'publicKey', 'getParametersForImportResponse_publicKey' - The public key to use to encrypt the key material before importing it
-- with ImportKeyMaterial.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'httpStatus', 'getParametersForImportResponse_httpStatus' - The response's http status code.
newGetParametersForImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetParametersForImportResponse
newGetParametersForImportResponse pHttpStatus_ =
  GetParametersForImportResponse'
    { importToken =
        Prelude.Nothing,
      keyId = Prelude.Nothing,
      parametersValidTo = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The import token to send in a subsequent ImportKeyMaterial request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getParametersForImportResponse_importToken :: Lens.Lens' GetParametersForImportResponse (Prelude.Maybe Prelude.ByteString)
getParametersForImportResponse_importToken = Lens.lens (\GetParametersForImportResponse' {importToken} -> importToken) (\s@GetParametersForImportResponse' {} a -> s {importToken = a} :: GetParametersForImportResponse) Prelude.. Lens.mapping Data._Base64

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key to use in a subsequent ImportKeyMaterial request. This is
-- the same KMS key specified in the @GetParametersForImport@ request.
getParametersForImportResponse_keyId :: Lens.Lens' GetParametersForImportResponse (Prelude.Maybe Prelude.Text)
getParametersForImportResponse_keyId = Lens.lens (\GetParametersForImportResponse' {keyId} -> keyId) (\s@GetParametersForImportResponse' {} a -> s {keyId = a} :: GetParametersForImportResponse)

-- | The time at which the import token and public key are no longer valid.
-- After this time, you cannot use them to make an ImportKeyMaterial
-- request and you must send another @GetParametersForImport@ request to
-- get new ones.
getParametersForImportResponse_parametersValidTo :: Lens.Lens' GetParametersForImportResponse (Prelude.Maybe Prelude.UTCTime)
getParametersForImportResponse_parametersValidTo = Lens.lens (\GetParametersForImportResponse' {parametersValidTo} -> parametersValidTo) (\s@GetParametersForImportResponse' {} a -> s {parametersValidTo = a} :: GetParametersForImportResponse) Prelude.. Lens.mapping Data._Time

-- | The public key to use to encrypt the key material before importing it
-- with ImportKeyMaterial.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getParametersForImportResponse_publicKey :: Lens.Lens' GetParametersForImportResponse (Prelude.Maybe Prelude.ByteString)
getParametersForImportResponse_publicKey = Lens.lens (\GetParametersForImportResponse' {publicKey} -> publicKey) (\s@GetParametersForImportResponse' {} a -> s {publicKey = a} :: GetParametersForImportResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | The response's http status code.
getParametersForImportResponse_httpStatus :: Lens.Lens' GetParametersForImportResponse Prelude.Int
getParametersForImportResponse_httpStatus = Lens.lens (\GetParametersForImportResponse' {httpStatus} -> httpStatus) (\s@GetParametersForImportResponse' {} a -> s {httpStatus = a} :: GetParametersForImportResponse)

instance
  Prelude.NFData
    GetParametersForImportResponse
  where
  rnf GetParametersForImportResponse' {..} =
    Prelude.rnf importToken
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf parametersValidTo
      `Prelude.seq` Prelude.rnf publicKey
      `Prelude.seq` Prelude.rnf httpStatus
