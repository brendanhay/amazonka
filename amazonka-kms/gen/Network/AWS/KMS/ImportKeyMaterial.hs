{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KMS.ImportKeyMaterial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports key material into an existing symmetric AWS KMS customer master
-- key (CMK) that was created without key material. After you successfully
-- import key material into a CMK, you can
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html#reimport-key-material reimport the same key material>
-- into that CMK, but you cannot import different key material.
--
-- You cannot perform this operation on an asymmetric CMK or on any CMK in
-- a different AWS account. For more information about creating CMKs with
-- no key material and then importing key material, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material>
-- in the /AWS Key Management Service Developer Guide/.
--
-- Before using this operation, call GetParametersForImport. Its response
-- includes a public key and an import token. Use the public key to encrypt
-- the key material. Then, submit the import token from the same
-- @GetParametersForImport@ response.
--
-- When calling this operation, you must specify the following values:
--
-- -   The key ID or key ARN of a CMK with no key material. Its @Origin@
--     must be @EXTERNAL@.
--
--     To create a CMK with no key material, call CreateKey and set the
--     value of its @Origin@ parameter to @EXTERNAL@. To get the @Origin@
--     of a CMK, call DescribeKey.)
--
-- -   The encrypted key material. To get the public key to encrypt the key
--     material, call GetParametersForImport.
--
-- -   The import token that GetParametersForImport returned. You must use
--     a public key and token from the same @GetParametersForImport@
--     response.
--
-- -   Whether the key material expires and if so, when. If you set an
--     expiration date, AWS KMS deletes the key material from the CMK on
--     the specified date, and the CMK becomes unusable. To use the CMK
--     again, you must reimport the same key material. The only way to
--     change an expiration date is by reimporting the same key material
--     and specifying a new expiration date.
--
-- When this operation is successful, the key state of the CMK changes from
-- @PendingImport@ to @Enabled@, and you can use the CMK.
--
-- If this operation fails, use the exception to help determine the
-- problem. If the error is related to the key material, the import token,
-- or wrapping key, use GetParametersForImport to get a new public key and
-- import token for the CMK and repeat the import procedure. For help, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html#importing-keys-overview How To Import Key Material>
-- in the /AWS Key Management Service Developer Guide/.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
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
module Network.AWS.KMS.ImportKeyMaterial
  ( -- * Creating a Request
    ImportKeyMaterial (..),
    newImportKeyMaterial,

    -- * Request Lenses
    importKeyMaterial_validTo,
    importKeyMaterial_expirationModel,
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportKeyMaterial' smart constructor.
data ImportKeyMaterial = ImportKeyMaterial'
  { -- | The time at which the imported key material expires. When the key
    -- material expires, AWS KMS deletes the key material and the CMK becomes
    -- unusable. You must omit this parameter when the @ExpirationModel@
    -- parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@. Otherwise it is
    -- required.
    validTo :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies whether the key material expires. The default is
    -- @KEY_MATERIAL_EXPIRES@, in which case you must include the @ValidTo@
    -- parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@,
    -- you must omit the @ValidTo@ parameter.
    expirationModel :: Prelude.Maybe ExpirationModelType,
    -- | The identifier of the symmetric CMK that receives the imported key
    -- material. The CMK\'s @Origin@ must be @EXTERNAL@. This must be the same
    -- CMK specified in the @KeyID@ parameter of the corresponding
    -- GetParametersForImport request.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
    keyId :: Prelude.Text,
    -- | The import token that you received in the response to a previous
    -- GetParametersForImport request. It must be from the same response that
    -- contained the public key that you used to encrypt the key material.
    importToken :: Prelude.Base64,
    -- | The encrypted key material to import. The key material must be encrypted
    -- with the public wrapping key that GetParametersForImport returned, using
    -- the wrapping algorithm that you specified in the same
    -- @GetParametersForImport@ request.
    encryptedKeyMaterial :: Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportKeyMaterial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validTo', 'importKeyMaterial_validTo' - The time at which the imported key material expires. When the key
-- material expires, AWS KMS deletes the key material and the CMK becomes
-- unusable. You must omit this parameter when the @ExpirationModel@
-- parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@. Otherwise it is
-- required.
--
-- 'expirationModel', 'importKeyMaterial_expirationModel' - Specifies whether the key material expires. The default is
-- @KEY_MATERIAL_EXPIRES@, in which case you must include the @ValidTo@
-- parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@,
-- you must omit the @ValidTo@ parameter.
--
-- 'keyId', 'importKeyMaterial_keyId' - The identifier of the symmetric CMK that receives the imported key
-- material. The CMK\'s @Origin@ must be @EXTERNAL@. This must be the same
-- CMK specified in the @KeyID@ parameter of the corresponding
-- GetParametersForImport request.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
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
      { validTo = Prelude.Nothing,
        expirationModel = Prelude.Nothing,
        keyId = pKeyId_,
        importToken = Prelude._Base64 Lens.# pImportToken_,
        encryptedKeyMaterial =
          Prelude._Base64 Lens.# pEncryptedKeyMaterial_
      }

-- | The time at which the imported key material expires. When the key
-- material expires, AWS KMS deletes the key material and the CMK becomes
-- unusable. You must omit this parameter when the @ExpirationModel@
-- parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@. Otherwise it is
-- required.
importKeyMaterial_validTo :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe Prelude.UTCTime)
importKeyMaterial_validTo = Lens.lens (\ImportKeyMaterial' {validTo} -> validTo) (\s@ImportKeyMaterial' {} a -> s {validTo = a} :: ImportKeyMaterial) Prelude.. Lens.mapping Prelude._Time

-- | Specifies whether the key material expires. The default is
-- @KEY_MATERIAL_EXPIRES@, in which case you must include the @ValidTo@
-- parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@,
-- you must omit the @ValidTo@ parameter.
importKeyMaterial_expirationModel :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe ExpirationModelType)
importKeyMaterial_expirationModel = Lens.lens (\ImportKeyMaterial' {expirationModel} -> expirationModel) (\s@ImportKeyMaterial' {} a -> s {expirationModel = a} :: ImportKeyMaterial)

-- | The identifier of the symmetric CMK that receives the imported key
-- material. The CMK\'s @Origin@ must be @EXTERNAL@. This must be the same
-- CMK specified in the @KeyID@ parameter of the corresponding
-- GetParametersForImport request.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
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
importKeyMaterial_importToken = Lens.lens (\ImportKeyMaterial' {importToken} -> importToken) (\s@ImportKeyMaterial' {} a -> s {importToken = a} :: ImportKeyMaterial) Prelude.. Prelude._Base64

-- | The encrypted key material to import. The key material must be encrypted
-- with the public wrapping key that GetParametersForImport returned, using
-- the wrapping algorithm that you specified in the same
-- @GetParametersForImport@ request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importKeyMaterial_encryptedKeyMaterial :: Lens.Lens' ImportKeyMaterial Prelude.ByteString
importKeyMaterial_encryptedKeyMaterial = Lens.lens (\ImportKeyMaterial' {encryptedKeyMaterial} -> encryptedKeyMaterial) (\s@ImportKeyMaterial' {} a -> s {encryptedKeyMaterial = a} :: ImportKeyMaterial) Prelude.. Prelude._Base64

instance Prelude.AWSRequest ImportKeyMaterial where
  type Rs ImportKeyMaterial = ImportKeyMaterialResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportKeyMaterialResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportKeyMaterial

instance Prelude.NFData ImportKeyMaterial

instance Prelude.ToHeaders ImportKeyMaterial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.ImportKeyMaterial" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ImportKeyMaterial where
  toJSON ImportKeyMaterial' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ValidTo" Prelude..=) Prelude.<$> validTo,
            ("ExpirationModel" Prelude..=)
              Prelude.<$> expirationModel,
            Prelude.Just ("KeyId" Prelude..= keyId),
            Prelude.Just ("ImportToken" Prelude..= importToken),
            Prelude.Just
              ( "EncryptedKeyMaterial"
                  Prelude..= encryptedKeyMaterial
              )
          ]
      )

instance Prelude.ToPath ImportKeyMaterial where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ImportKeyMaterial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportKeyMaterialResponse' smart constructor.
data ImportKeyMaterialResponse = ImportKeyMaterialResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ImportKeyMaterialResponse
