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
-- Module      : Amazonka.KMS.ImportKeyMaterial
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports key material into an existing symmetric encryption KMS key that
-- was created without key material. After you successfully import key
-- material into a KMS key, you can
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html#reimport-key-material reimport the same key material>
-- into that KMS key, but you cannot import different key material.
--
-- You cannot perform this operation on an asymmetric KMS key, an HMAC KMS
-- key, or on any KMS key in a different Amazon Web Services account. For
-- more information about creating KMS keys with no key material and then
-- importing key material, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material>
-- in the /Key Management Service Developer Guide/.
--
-- Before using this operation, call GetParametersForImport. Its response
-- includes a public key and an import token. Use the public key to encrypt
-- the key material. Then, submit the import token from the same
-- @GetParametersForImport@ response.
--
-- When calling this operation, you must specify the following values:
--
-- -   The key ID or key ARN of a KMS key with no key material. Its
--     @Origin@ must be @EXTERNAL@.
--
--     To create a KMS key with no key material, call CreateKey and set the
--     value of its @Origin@ parameter to @EXTERNAL@. To get the @Origin@
--     of a KMS key, call DescribeKey.)
--
-- -   The encrypted key material. To get the public key to encrypt the key
--     material, call GetParametersForImport.
--
-- -   The import token that GetParametersForImport returned. You must use
--     a public key and token from the same @GetParametersForImport@
--     response.
--
-- -   Whether the key material expires and if so, when. If you set an
--     expiration date, KMS deletes the key material from the KMS key on
--     the specified date, and the KMS key becomes unusable. To use the KMS
--     key again, you must reimport the same key material. The only way to
--     change an expiration date is by reimporting the same key material
--     and specifying a new expiration date.
--
-- When this operation is successful, the key state of the KMS key changes
-- from @PendingImport@ to @Enabled@, and you can use the KMS key.
--
-- If this operation fails, use the exception to help determine the
-- problem. If the error is related to the key material, the import token,
-- or wrapping key, use GetParametersForImport to get a new public key and
-- import token for the KMS key and repeat the import procedure. For help,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html#importing-keys-overview How To Import Key Material>
-- in the /Key Management Service Developer Guide/.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ImportKeyMaterial>
-- (key policy)
--
-- __Related operations:__
--
-- -   DeleteImportedKeyMaterial
--
-- -   GetParametersForImport
module Amazonka.KMS.ImportKeyMaterial
  ( -- * Creating a Request
    ImportKeyMaterial (..),
    newImportKeyMaterial,

    -- * Request Lenses
    importKeyMaterial_expirationModel,
    importKeyMaterial_validTo,
    importKeyMaterial_keyId,
    importKeyMaterial_importToken,
    importKeyMaterial_encryptedKeyMaterial,

    -- * Destructuring the Response
    ImportKeyMaterialResponse (..),
    newImportKeyMaterialResponse,

    -- * Response Lenses
    importKeyMaterialResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportKeyMaterial' smart constructor.
data ImportKeyMaterial = ImportKeyMaterial'
  { -- | Specifies whether the key material expires. The default is
    -- @KEY_MATERIAL_EXPIRES@, in which case you must include the @ValidTo@
    -- parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@,
    -- you must omit the @ValidTo@ parameter.
    expirationModel :: Prelude.Maybe ExpirationModelType,
    -- | The time at which the imported key material expires. When the key
    -- material expires, KMS deletes the key material and the KMS key becomes
    -- unusable. You must omit this parameter when the @ExpirationModel@
    -- parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@. Otherwise it is
    -- required.
    validTo :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the symmetric encryption KMS key that receives the
    -- imported key material. This must be the same KMS key specified in the
    -- @KeyID@ parameter of the corresponding GetParametersForImport request.
    -- The @Origin@ of the KMS key must be @EXTERNAL@. You cannot perform this
    -- operation on an asymmetric KMS key, an HMAC KMS key, a KMS key in a
    -- custom key store, or on a KMS key in a different Amazon Web Services
    -- account
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
    -- | The import token that you received in the response to a previous
    -- GetParametersForImport request. It must be from the same response that
    -- contained the public key that you used to encrypt the key material.
    importToken :: Core.Base64,
    -- | The encrypted key material to import. The key material must be encrypted
    -- with the public wrapping key that GetParametersForImport returned, using
    -- the wrapping algorithm that you specified in the same
    -- @GetParametersForImport@ request.
    encryptedKeyMaterial :: Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportKeyMaterial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationModel', 'importKeyMaterial_expirationModel' - Specifies whether the key material expires. The default is
-- @KEY_MATERIAL_EXPIRES@, in which case you must include the @ValidTo@
-- parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@,
-- you must omit the @ValidTo@ parameter.
--
-- 'validTo', 'importKeyMaterial_validTo' - The time at which the imported key material expires. When the key
-- material expires, KMS deletes the key material and the KMS key becomes
-- unusable. You must omit this parameter when the @ExpirationModel@
-- parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@. Otherwise it is
-- required.
--
-- 'keyId', 'importKeyMaterial_keyId' - The identifier of the symmetric encryption KMS key that receives the
-- imported key material. This must be the same KMS key specified in the
-- @KeyID@ parameter of the corresponding GetParametersForImport request.
-- The @Origin@ of the KMS key must be @EXTERNAL@. You cannot perform this
-- operation on an asymmetric KMS key, an HMAC KMS key, a KMS key in a
-- custom key store, or on a KMS key in a different Amazon Web Services
-- account
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
-- 'importToken', 'importKeyMaterial_importToken' - The import token that you received in the response to a previous
-- GetParametersForImport request. It must be from the same response that
-- contained the public key that you used to encrypt the key material.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'encryptedKeyMaterial', 'importKeyMaterial_encryptedKeyMaterial' - The encrypted key material to import. The key material must be encrypted
-- with the public wrapping key that GetParametersForImport returned, using
-- the wrapping algorithm that you specified in the same
-- @GetParametersForImport@ request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newImportKeyMaterial ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'importToken'
  Prelude.ByteString ->
  -- | 'encryptedKeyMaterial'
  Prelude.ByteString ->
  ImportKeyMaterial
newImportKeyMaterial
  pKeyId_
  pImportToken_
  pEncryptedKeyMaterial_ =
    ImportKeyMaterial'
      { expirationModel =
          Prelude.Nothing,
        validTo = Prelude.Nothing,
        keyId = pKeyId_,
        importToken = Core._Base64 Lens.# pImportToken_,
        encryptedKeyMaterial =
          Core._Base64 Lens.# pEncryptedKeyMaterial_
      }

-- | Specifies whether the key material expires. The default is
-- @KEY_MATERIAL_EXPIRES@, in which case you must include the @ValidTo@
-- parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@,
-- you must omit the @ValidTo@ parameter.
importKeyMaterial_expirationModel :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe ExpirationModelType)
importKeyMaterial_expirationModel = Lens.lens (\ImportKeyMaterial' {expirationModel} -> expirationModel) (\s@ImportKeyMaterial' {} a -> s {expirationModel = a} :: ImportKeyMaterial)

-- | The time at which the imported key material expires. When the key
-- material expires, KMS deletes the key material and the KMS key becomes
-- unusable. You must omit this parameter when the @ExpirationModel@
-- parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@. Otherwise it is
-- required.
importKeyMaterial_validTo :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe Prelude.UTCTime)
importKeyMaterial_validTo = Lens.lens (\ImportKeyMaterial' {validTo} -> validTo) (\s@ImportKeyMaterial' {} a -> s {validTo = a} :: ImportKeyMaterial) Prelude.. Lens.mapping Core._Time

-- | The identifier of the symmetric encryption KMS key that receives the
-- imported key material. This must be the same KMS key specified in the
-- @KeyID@ parameter of the corresponding GetParametersForImport request.
-- The @Origin@ of the KMS key must be @EXTERNAL@. You cannot perform this
-- operation on an asymmetric KMS key, an HMAC KMS key, a KMS key in a
-- custom key store, or on a KMS key in a different Amazon Web Services
-- account
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
importKeyMaterial_keyId :: Lens.Lens' ImportKeyMaterial Prelude.Text
importKeyMaterial_keyId = Lens.lens (\ImportKeyMaterial' {keyId} -> keyId) (\s@ImportKeyMaterial' {} a -> s {keyId = a} :: ImportKeyMaterial)

-- | The import token that you received in the response to a previous
-- GetParametersForImport request. It must be from the same response that
-- contained the public key that you used to encrypt the key material.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importKeyMaterial_importToken :: Lens.Lens' ImportKeyMaterial Prelude.ByteString
importKeyMaterial_importToken = Lens.lens (\ImportKeyMaterial' {importToken} -> importToken) (\s@ImportKeyMaterial' {} a -> s {importToken = a} :: ImportKeyMaterial) Prelude.. Core._Base64

-- | The encrypted key material to import. The key material must be encrypted
-- with the public wrapping key that GetParametersForImport returned, using
-- the wrapping algorithm that you specified in the same
-- @GetParametersForImport@ request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importKeyMaterial_encryptedKeyMaterial :: Lens.Lens' ImportKeyMaterial Prelude.ByteString
importKeyMaterial_encryptedKeyMaterial = Lens.lens (\ImportKeyMaterial' {encryptedKeyMaterial} -> encryptedKeyMaterial) (\s@ImportKeyMaterial' {} a -> s {encryptedKeyMaterial = a} :: ImportKeyMaterial) Prelude.. Core._Base64

instance Core.AWSRequest ImportKeyMaterial where
  type
    AWSResponse ImportKeyMaterial =
      ImportKeyMaterialResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportKeyMaterialResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportKeyMaterial where
  hashWithSalt _salt ImportKeyMaterial' {..} =
    _salt `Prelude.hashWithSalt` expirationModel
      `Prelude.hashWithSalt` validTo
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` importToken
      `Prelude.hashWithSalt` encryptedKeyMaterial

instance Prelude.NFData ImportKeyMaterial where
  rnf ImportKeyMaterial' {..} =
    Prelude.rnf expirationModel
      `Prelude.seq` Prelude.rnf validTo
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf importToken
      `Prelude.seq` Prelude.rnf encryptedKeyMaterial

instance Core.ToHeaders ImportKeyMaterial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.ImportKeyMaterial" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ImportKeyMaterial where
  toJSON ImportKeyMaterial' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExpirationModel" Core..=)
              Prelude.<$> expirationModel,
            ("ValidTo" Core..=) Prelude.<$> validTo,
            Prelude.Just ("KeyId" Core..= keyId),
            Prelude.Just ("ImportToken" Core..= importToken),
            Prelude.Just
              ( "EncryptedKeyMaterial"
                  Core..= encryptedKeyMaterial
              )
          ]
      )

instance Core.ToPath ImportKeyMaterial where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportKeyMaterial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportKeyMaterialResponse' smart constructor.
data ImportKeyMaterialResponse = ImportKeyMaterialResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportKeyMaterialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'importKeyMaterialResponse_httpStatus' - The response's http status code.
newImportKeyMaterialResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportKeyMaterialResponse
newImportKeyMaterialResponse pHttpStatus_ =
  ImportKeyMaterialResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
importKeyMaterialResponse_httpStatus :: Lens.Lens' ImportKeyMaterialResponse Prelude.Int
importKeyMaterialResponse_httpStatus = Lens.lens (\ImportKeyMaterialResponse' {httpStatus} -> httpStatus) (\s@ImportKeyMaterialResponse' {} a -> s {httpStatus = a} :: ImportKeyMaterialResponse)

instance Prelude.NFData ImportKeyMaterialResponse where
  rnf ImportKeyMaterialResponse' {..} =
    Prelude.rnf httpStatus
