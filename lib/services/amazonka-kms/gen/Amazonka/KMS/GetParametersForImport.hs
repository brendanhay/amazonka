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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the items you need to import key material into a symmetric
-- encryption KMS key. For more information about importing key material
-- into KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing key material>
-- in the /Key Management Service Developer Guide/.
--
-- This operation returns a public key and an import token. Use the public
-- key to encrypt the symmetric key material. Store the import token to
-- send with a subsequent ImportKeyMaterial request.
--
-- You must specify the key ID of the symmetric encryption KMS key into
-- which you will import key material. This KMS key\'s @Origin@ must be
-- @EXTERNAL@. You must also specify the wrapping algorithm and type of
-- wrapping key (public key) that you will use to encrypt the key material.
-- You cannot perform this operation on an asymmetric KMS key, an HMAC KMS
-- key, or on any KMS key in a different Amazon Web Services account.
--
-- To import key material, you must use the public key and import token
-- from the same response. These items are valid for 24 hours. The
-- expiration date and time appear in the @GetParametersForImport@
-- response. You cannot use an expired token in an ImportKeyMaterial
-- request. If your key and token expire, send another
-- @GetParametersForImport@ request.
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
    getParametersForImportResponse_publicKey,
    getParametersForImportResponse_keyId,
    getParametersForImportResponse_importToken,
    getParametersForImportResponse_parametersValidTo,
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
  { -- | The identifier of the symmetric encryption KMS key into which you will
    -- import key material. The @Origin@ of the KMS key must be @EXTERNAL@.
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
    -- | The algorithm you will use to encrypt the key material before importing
    -- it with ImportKeyMaterial. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material>
    -- in the /Key Management Service Developer Guide/.
    wrappingAlgorithm :: AlgorithmSpec,
    -- | The type of wrapping key (public key) to return in the response. Only
    -- 2048-bit RSA public keys are supported.
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
-- 'keyId', 'getParametersForImport_keyId' - The identifier of the symmetric encryption KMS key into which you will
-- import key material. The @Origin@ of the KMS key must be @EXTERNAL@.
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
-- 'wrappingAlgorithm', 'getParametersForImport_wrappingAlgorithm' - The algorithm you will use to encrypt the key material before importing
-- it with ImportKeyMaterial. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material>
-- in the /Key Management Service Developer Guide/.
--
-- 'wrappingKeySpec', 'getParametersForImport_wrappingKeySpec' - The type of wrapping key (public key) to return in the response. Only
-- 2048-bit RSA public keys are supported.
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

-- | The identifier of the symmetric encryption KMS key into which you will
-- import key material. The @Origin@ of the KMS key must be @EXTERNAL@.
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

-- | The algorithm you will use to encrypt the key material before importing
-- it with ImportKeyMaterial. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material>
-- in the /Key Management Service Developer Guide/.
getParametersForImport_wrappingAlgorithm :: Lens.Lens' GetParametersForImport AlgorithmSpec
getParametersForImport_wrappingAlgorithm = Lens.lens (\GetParametersForImport' {wrappingAlgorithm} -> wrappingAlgorithm) (\s@GetParametersForImport' {} a -> s {wrappingAlgorithm = a} :: GetParametersForImport)

-- | The type of wrapping key (public key) to return in the response. Only
-- 2048-bit RSA public keys are supported.
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
            Prelude.<$> (x Data..?> "PublicKey")
            Prelude.<*> (x Data..?> "KeyId")
            Prelude.<*> (x Data..?> "ImportToken")
            Prelude.<*> (x Data..?> "ParametersValidTo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetParametersForImport where
  hashWithSalt _salt GetParametersForImport' {..} =
    _salt `Prelude.hashWithSalt` keyId
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
  { -- | The public key to use to encrypt the key material before importing it
    -- with ImportKeyMaterial.
    publicKey :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the KMS key to use in a subsequent ImportKeyMaterial request. This is
    -- the same KMS key specified in the @GetParametersForImport@ request.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The import token to send in a subsequent ImportKeyMaterial request.
    importToken :: Prelude.Maybe Data.Base64,
    -- | The time at which the import token and public key are no longer valid.
    -- After this time, you cannot use them to make an ImportKeyMaterial
    -- request and you must send another @GetParametersForImport@ request to
    -- get new ones.
    parametersValidTo :: Prelude.Maybe Data.POSIX,
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
-- 'publicKey', 'getParametersForImportResponse_publicKey' - The public key to use to encrypt the key material before importing it
-- with ImportKeyMaterial.--
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
-- 'importToken', 'getParametersForImportResponse_importToken' - The import token to send in a subsequent ImportKeyMaterial request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'parametersValidTo', 'getParametersForImportResponse_parametersValidTo' - The time at which the import token and public key are no longer valid.
-- After this time, you cannot use them to make an ImportKeyMaterial
-- request and you must send another @GetParametersForImport@ request to
-- get new ones.
--
-- 'httpStatus', 'getParametersForImportResponse_httpStatus' - The response's http status code.
newGetParametersForImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetParametersForImportResponse
newGetParametersForImportResponse pHttpStatus_ =
  GetParametersForImportResponse'
    { publicKey =
        Prelude.Nothing,
      keyId = Prelude.Nothing,
      importToken = Prelude.Nothing,
      parametersValidTo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The public key to use to encrypt the key material before importing it
-- with ImportKeyMaterial.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getParametersForImportResponse_publicKey :: Lens.Lens' GetParametersForImportResponse (Prelude.Maybe Prelude.ByteString)
getParametersForImportResponse_publicKey = Lens.lens (\GetParametersForImportResponse' {publicKey} -> publicKey) (\s@GetParametersForImportResponse' {} a -> s {publicKey = a} :: GetParametersForImportResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key to use in a subsequent ImportKeyMaterial request. This is
-- the same KMS key specified in the @GetParametersForImport@ request.
getParametersForImportResponse_keyId :: Lens.Lens' GetParametersForImportResponse (Prelude.Maybe Prelude.Text)
getParametersForImportResponse_keyId = Lens.lens (\GetParametersForImportResponse' {keyId} -> keyId) (\s@GetParametersForImportResponse' {} a -> s {keyId = a} :: GetParametersForImportResponse)

-- | The import token to send in a subsequent ImportKeyMaterial request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getParametersForImportResponse_importToken :: Lens.Lens' GetParametersForImportResponse (Prelude.Maybe Prelude.ByteString)
getParametersForImportResponse_importToken = Lens.lens (\GetParametersForImportResponse' {importToken} -> importToken) (\s@GetParametersForImportResponse' {} a -> s {importToken = a} :: GetParametersForImportResponse) Prelude.. Lens.mapping Data._Base64

-- | The time at which the import token and public key are no longer valid.
-- After this time, you cannot use them to make an ImportKeyMaterial
-- request and you must send another @GetParametersForImport@ request to
-- get new ones.
getParametersForImportResponse_parametersValidTo :: Lens.Lens' GetParametersForImportResponse (Prelude.Maybe Prelude.UTCTime)
getParametersForImportResponse_parametersValidTo = Lens.lens (\GetParametersForImportResponse' {parametersValidTo} -> parametersValidTo) (\s@GetParametersForImportResponse' {} a -> s {parametersValidTo = a} :: GetParametersForImportResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getParametersForImportResponse_httpStatus :: Lens.Lens' GetParametersForImportResponse Prelude.Int
getParametersForImportResponse_httpStatus = Lens.lens (\GetParametersForImportResponse' {httpStatus} -> httpStatus) (\s@GetParametersForImportResponse' {} a -> s {httpStatus = a} :: GetParametersForImportResponse)

instance
  Prelude.NFData
    GetParametersForImportResponse
  where
  rnf GetParametersForImportResponse' {..} =
    Prelude.rnf publicKey
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf importToken
      `Prelude.seq` Prelude.rnf parametersValidTo
      `Prelude.seq` Prelude.rnf httpStatus
