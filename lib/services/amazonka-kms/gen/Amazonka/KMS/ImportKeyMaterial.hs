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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports or reimports key material into an existing KMS key that was
-- created without key material. @ImportKeyMaterial@ also sets the
-- expiration model and expiration date of the imported key material.
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
-- After you successfully import key material into a KMS key, you can
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html#reimport-key-material reimport the same key material>
-- into that KMS key, but you cannot import different key material. You
-- might reimport key material to replace key material that expired or key
-- material that you deleted. You might also reimport key material to
-- change the expiration model or expiration date of the key material.
-- Before reimporting key material, if necessary, call
-- DeleteImportedKeyMaterial to delete the current imported key material.
--
-- Each time you import key material into KMS, you can determine whether
-- (@ExpirationModel@) and when (@ValidTo@) the key material expires. To
-- change the expiration of your key material, you must import it again,
-- either by calling @ImportKeyMaterial@ or using the
-- <kms/latest/developerguide/importing-keys-import-key-material.html#importing-keys-import-key-material-console import features>
-- of the KMS console.
--
-- Before calling @ImportKeyMaterial@:
--
-- -   Create or identify a KMS key with no key material. The KMS key must
--     have an @Origin@ value of @EXTERNAL@, which indicates that the KMS
--     key is designed for imported key material.
--
--     To create an new KMS key for imported key material, call the
--     CreateKey operation with an @Origin@ value of @EXTERNAL@. You can
--     create a symmetric encryption KMS key, HMAC KMS key, asymmetric
--     encryption KMS key, or asymmetric signing KMS key. You can also
--     import key material into a
--     <kms/latest/developerguide/multi-region-keys-overview.html multi-Region key>
--     of any supported type. However, you can\'t import key material into
--     a KMS key in a
--     <kms/latest/developerguide/custom-key-store-overview.html custom key store>.
--
-- -   Use the DescribeKey operation to verify that the @KeyState@ of the
--     KMS key is @PendingImport@, which indicates that the KMS key has no
--     key material.
--
--     If you are reimporting the same key material into an existing KMS
--     key, you might need to call the DeleteImportedKeyMaterial to delete
--     its existing key material.
--
-- -   Call the GetParametersForImport operation to get a public key and
--     import token set for importing key material.
--
-- -   Use the public key in the GetParametersForImport response to encrypt
--     your key material.
--
-- Then, in an @ImportKeyMaterial@ request, you submit your encrypted key
-- material and import token. When calling this operation, you must specify
-- the following values:
--
-- -   The key ID or key ARN of the KMS key to associate with the imported
--     key material. Its @Origin@ must be @EXTERNAL@ and its @KeyState@
--     must be @PendingImport@. You cannot perform this operation on a KMS
--     key in a
--     <kms/latest/developerguide/custom-key-store-overview.html custom key store>,
--     or on a KMS key in a different Amazon Web Services account. To get
--     the @Origin@ and @KeyState@ of a KMS key, call DescribeKey.
--
-- -   The encrypted key material.
--
-- -   The import token that GetParametersForImport returned. You must use
--     a public key and token from the same @GetParametersForImport@
--     response.
--
-- -   Whether the key material expires (@ExpirationModel@) and, if so,
--     when (@ValidTo@). For help with this choice, see
--     <https://docs.aws.amazon.com/en_us/kms/latest/developerguide/importing-keys.html#importing-keys-expiration Setting an expiration time>
--     in the /Key Management Service Developer Guide/.
--
--     If you set an expiration date, KMS deletes the key material from the
--     KMS key on the specified date, making the KMS key unusable. To use
--     the KMS key in cryptographic operations again, you must reimport the
--     same key material. However, you can delete and reimport the key
--     material at any time, including before the key material expires.
--     Each time you reimport, you can eliminate or reset the expiration
--     time.
--
-- When this operation is successful, the key state of the KMS key changes
-- from @PendingImport@ to @Enabled@, and you can use the KMS key in
-- cryptographic operations.
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
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportKeyMaterial' smart constructor.
data ImportKeyMaterial = ImportKeyMaterial'
  { -- | Specifies whether the key material expires. The default is
    -- @KEY_MATERIAL_EXPIRES@. For help with this choice, see
    -- <https://docs.aws.amazon.com/en_us/kms/latest/developerguide/importing-keys.html#importing-keys-expiration Setting an expiration time>
    -- in the /Key Management Service Developer Guide/.
    --
    -- When the value of @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@, you must
    -- specify a value for the @ValidTo@ parameter. When value is
    -- @KEY_MATERIAL_DOES_NOT_EXPIRE@, you must omit the @ValidTo@ parameter.
    --
    -- You cannot change the @ExpirationModel@ or @ValidTo@ values for the
    -- current import after the request completes. To change either value, you
    -- must reimport the key material.
    expirationModel :: Prelude.Maybe ExpirationModelType,
    -- | The date and time when the imported key material expires. This parameter
    -- is required when the value of the @ExpirationModel@ parameter is
    -- @KEY_MATERIAL_EXPIRES@. Otherwise it is not valid.
    --
    -- The value of this parameter must be a future date and time. The maximum
    -- value is 365 days from the request date.
    --
    -- When the key material expires, KMS deletes the key material from the KMS
    -- key. Without its key material, the KMS key is unusable. To use the KMS
    -- key in cryptographic operations, you must reimport the same key
    -- material.
    --
    -- You cannot change the @ExpirationModel@ or @ValidTo@ values for the
    -- current import after the request completes. To change either value, you
    -- must delete (DeleteImportedKeyMaterial) and reimport the key material.
    validTo :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the KMS key that will be associated with the imported
    -- key material. This must be the same KMS key specified in the @KeyID@
    -- parameter of the corresponding GetParametersForImport request. The
    -- @Origin@ of the KMS key must be @EXTERNAL@ and its @KeyState@ must be
    -- @PendingImport@.
    --
    -- The KMS key can be a symmetric encryption KMS key, HMAC KMS key,
    -- asymmetric encryption KMS key, or asymmetric signing KMS key, including
    -- a
    -- <kms/latest/developerguide/multi-region-keys-overview.html multi-Region key>
    -- of any supported type. You cannot perform this operation on a KMS key in
    -- a custom key store, or on a KMS key in a different Amazon Web Services
    -- account.
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
    importToken :: Data.Base64,
    -- | The encrypted key material to import. The key material must be encrypted
    -- under the public wrapping key that GetParametersForImport returned,
    -- using the wrapping algorithm that you specified in the same
    -- @GetParametersForImport@ request.
    encryptedKeyMaterial :: Data.Base64
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
-- @KEY_MATERIAL_EXPIRES@. For help with this choice, see
-- <https://docs.aws.amazon.com/en_us/kms/latest/developerguide/importing-keys.html#importing-keys-expiration Setting an expiration time>
-- in the /Key Management Service Developer Guide/.
--
-- When the value of @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@, you must
-- specify a value for the @ValidTo@ parameter. When value is
-- @KEY_MATERIAL_DOES_NOT_EXPIRE@, you must omit the @ValidTo@ parameter.
--
-- You cannot change the @ExpirationModel@ or @ValidTo@ values for the
-- current import after the request completes. To change either value, you
-- must reimport the key material.
--
-- 'validTo', 'importKeyMaterial_validTo' - The date and time when the imported key material expires. This parameter
-- is required when the value of the @ExpirationModel@ parameter is
-- @KEY_MATERIAL_EXPIRES@. Otherwise it is not valid.
--
-- The value of this parameter must be a future date and time. The maximum
-- value is 365 days from the request date.
--
-- When the key material expires, KMS deletes the key material from the KMS
-- key. Without its key material, the KMS key is unusable. To use the KMS
-- key in cryptographic operations, you must reimport the same key
-- material.
--
-- You cannot change the @ExpirationModel@ or @ValidTo@ values for the
-- current import after the request completes. To change either value, you
-- must delete (DeleteImportedKeyMaterial) and reimport the key material.
--
-- 'keyId', 'importKeyMaterial_keyId' - The identifier of the KMS key that will be associated with the imported
-- key material. This must be the same KMS key specified in the @KeyID@
-- parameter of the corresponding GetParametersForImport request. The
-- @Origin@ of the KMS key must be @EXTERNAL@ and its @KeyState@ must be
-- @PendingImport@.
--
-- The KMS key can be a symmetric encryption KMS key, HMAC KMS key,
-- asymmetric encryption KMS key, or asymmetric signing KMS key, including
-- a
-- <kms/latest/developerguide/multi-region-keys-overview.html multi-Region key>
-- of any supported type. You cannot perform this operation on a KMS key in
-- a custom key store, or on a KMS key in a different Amazon Web Services
-- account.
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
-- under the public wrapping key that GetParametersForImport returned,
-- using the wrapping algorithm that you specified in the same
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
        importToken = Data._Base64 Lens.# pImportToken_,
        encryptedKeyMaterial =
          Data._Base64 Lens.# pEncryptedKeyMaterial_
      }

-- | Specifies whether the key material expires. The default is
-- @KEY_MATERIAL_EXPIRES@. For help with this choice, see
-- <https://docs.aws.amazon.com/en_us/kms/latest/developerguide/importing-keys.html#importing-keys-expiration Setting an expiration time>
-- in the /Key Management Service Developer Guide/.
--
-- When the value of @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@, you must
-- specify a value for the @ValidTo@ parameter. When value is
-- @KEY_MATERIAL_DOES_NOT_EXPIRE@, you must omit the @ValidTo@ parameter.
--
-- You cannot change the @ExpirationModel@ or @ValidTo@ values for the
-- current import after the request completes. To change either value, you
-- must reimport the key material.
importKeyMaterial_expirationModel :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe ExpirationModelType)
importKeyMaterial_expirationModel = Lens.lens (\ImportKeyMaterial' {expirationModel} -> expirationModel) (\s@ImportKeyMaterial' {} a -> s {expirationModel = a} :: ImportKeyMaterial)

-- | The date and time when the imported key material expires. This parameter
-- is required when the value of the @ExpirationModel@ parameter is
-- @KEY_MATERIAL_EXPIRES@. Otherwise it is not valid.
--
-- The value of this parameter must be a future date and time. The maximum
-- value is 365 days from the request date.
--
-- When the key material expires, KMS deletes the key material from the KMS
-- key. Without its key material, the KMS key is unusable. To use the KMS
-- key in cryptographic operations, you must reimport the same key
-- material.
--
-- You cannot change the @ExpirationModel@ or @ValidTo@ values for the
-- current import after the request completes. To change either value, you
-- must delete (DeleteImportedKeyMaterial) and reimport the key material.
importKeyMaterial_validTo :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe Prelude.UTCTime)
importKeyMaterial_validTo = Lens.lens (\ImportKeyMaterial' {validTo} -> validTo) (\s@ImportKeyMaterial' {} a -> s {validTo = a} :: ImportKeyMaterial) Prelude.. Lens.mapping Data._Time

-- | The identifier of the KMS key that will be associated with the imported
-- key material. This must be the same KMS key specified in the @KeyID@
-- parameter of the corresponding GetParametersForImport request. The
-- @Origin@ of the KMS key must be @EXTERNAL@ and its @KeyState@ must be
-- @PendingImport@.
--
-- The KMS key can be a symmetric encryption KMS key, HMAC KMS key,
-- asymmetric encryption KMS key, or asymmetric signing KMS key, including
-- a
-- <kms/latest/developerguide/multi-region-keys-overview.html multi-Region key>
-- of any supported type. You cannot perform this operation on a KMS key in
-- a custom key store, or on a KMS key in a different Amazon Web Services
-- account.
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
importKeyMaterial_importToken = Lens.lens (\ImportKeyMaterial' {importToken} -> importToken) (\s@ImportKeyMaterial' {} a -> s {importToken = a} :: ImportKeyMaterial) Prelude.. Data._Base64

-- | The encrypted key material to import. The key material must be encrypted
-- under the public wrapping key that GetParametersForImport returned,
-- using the wrapping algorithm that you specified in the same
-- @GetParametersForImport@ request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importKeyMaterial_encryptedKeyMaterial :: Lens.Lens' ImportKeyMaterial Prelude.ByteString
importKeyMaterial_encryptedKeyMaterial = Lens.lens (\ImportKeyMaterial' {encryptedKeyMaterial} -> encryptedKeyMaterial) (\s@ImportKeyMaterial' {} a -> s {encryptedKeyMaterial = a} :: ImportKeyMaterial) Prelude.. Data._Base64

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
    _salt
      `Prelude.hashWithSalt` expirationModel
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

instance Data.ToHeaders ImportKeyMaterial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.ImportKeyMaterial" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportKeyMaterial where
  toJSON ImportKeyMaterial' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExpirationModel" Data..=)
              Prelude.<$> expirationModel,
            ("ValidTo" Data..=) Prelude.<$> validTo,
            Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just ("ImportToken" Data..= importToken),
            Prelude.Just
              ( "EncryptedKeyMaterial"
                  Data..= encryptedKeyMaterial
              )
          ]
      )

instance Data.ToPath ImportKeyMaterial where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportKeyMaterial where
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
