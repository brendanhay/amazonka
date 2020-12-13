{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ImportKeyMaterial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports key material into an existing symmetric AWS KMS customer master key (CMK) that was created without key material. After you successfully import key material into a CMK, you can <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html#reimport-key-material reimport the same key material> into that CMK, but you cannot import different key material.
--
-- You cannot perform this operation on an asymmetric CMK or on any CMK in a different AWS account. For more information about creating CMKs with no key material and then importing key material, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ .
-- Before using this operation, call 'GetParametersForImport' . Its response includes a public key and an import token. Use the public key to encrypt the key material. Then, submit the import token from the same @GetParametersForImport@ response.
-- When calling this operation, you must specify the following values:
--
--     * The key ID or key ARN of a CMK with no key material. Its @Origin@ must be @EXTERNAL@ .
-- To create a CMK with no key material, call 'CreateKey' and set the value of its @Origin@ parameter to @EXTERNAL@ . To get the @Origin@ of a CMK, call 'DescribeKey' .)
--
--
--     * The encrypted key material. To get the public key to encrypt the key material, call 'GetParametersForImport' .
--
--
--     * The import token that 'GetParametersForImport' returned. You must use a public key and token from the same @GetParametersForImport@ response.
--
--
--     * Whether the key material expires and if so, when. If you set an expiration date, AWS KMS deletes the key material from the CMK on the specified date, and the CMK becomes unusable. To use the CMK again, you must reimport the same key material. The only way to change an expiration date is by reimporting the same key material and specifying a new expiration date.
--
--
-- When this operation is successful, the key state of the CMK changes from @PendingImport@ to @Enabled@ , and you can use the CMK.
-- If this operation fails, use the exception to help determine the problem. If the error is related to the key material, the import token, or wrapping key, use 'GetParametersForImport' to get a new public key and import token for the CMK and repeat the import procedure. For help, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html#importing-keys-overview How To Import Key Material> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.ImportKeyMaterial
  ( -- * Creating a request
    ImportKeyMaterial (..),
    mkImportKeyMaterial,

    -- ** Request lenses
    ikmExpirationModel,
    ikmKeyId,
    ikmEncryptedKeyMaterial,
    ikmValidTo,
    ikmImportToken,

    -- * Destructuring the response
    ImportKeyMaterialResponse (..),
    mkImportKeyMaterialResponse,

    -- ** Response lenses
    ikmrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportKeyMaterial' smart constructor.
data ImportKeyMaterial = ImportKeyMaterial'
  { -- | Specifies whether the key material expires. The default is @KEY_MATERIAL_EXPIRES@ , in which case you must include the @ValidTo@ parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ , you must omit the @ValidTo@ parameter.
    expirationModel :: Lude.Maybe ExpirationModelType,
    -- | The identifier of the symmetric CMK that receives the imported key material. The CMK's @Origin@ must be @EXTERNAL@ . This must be the same CMK specified in the @KeyID@ parameter of the corresponding 'GetParametersForImport' request.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
    keyId :: Lude.Text,
    -- | The encrypted key material to import. The key material must be encrypted with the public wrapping key that 'GetParametersForImport' returned, using the wrapping algorithm that you specified in the same @GetParametersForImport@ request.
    encryptedKeyMaterial :: Lude.Base64,
    -- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. You must omit this parameter when the @ExpirationModel@ parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ . Otherwise it is required.
    validTo :: Lude.Maybe Lude.Timestamp,
    -- | The import token that you received in the response to a previous 'GetParametersForImport' request. It must be from the same response that contained the public key that you used to encrypt the key material.
    importToken :: Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportKeyMaterial' with the minimum fields required to make a request.
--
-- * 'expirationModel' - Specifies whether the key material expires. The default is @KEY_MATERIAL_EXPIRES@ , in which case you must include the @ValidTo@ parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ , you must omit the @ValidTo@ parameter.
-- * 'keyId' - The identifier of the symmetric CMK that receives the imported key material. The CMK's @Origin@ must be @EXTERNAL@ . This must be the same CMK specified in the @KeyID@ parameter of the corresponding 'GetParametersForImport' request.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
-- * 'encryptedKeyMaterial' - The encrypted key material to import. The key material must be encrypted with the public wrapping key that 'GetParametersForImport' returned, using the wrapping algorithm that you specified in the same @GetParametersForImport@ request.
-- * 'validTo' - The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. You must omit this parameter when the @ExpirationModel@ parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ . Otherwise it is required.
-- * 'importToken' - The import token that you received in the response to a previous 'GetParametersForImport' request. It must be from the same response that contained the public key that you used to encrypt the key material.
mkImportKeyMaterial ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'encryptedKeyMaterial'
  Lude.Base64 ->
  -- | 'importToken'
  Lude.Base64 ->
  ImportKeyMaterial
mkImportKeyMaterial pKeyId_ pEncryptedKeyMaterial_ pImportToken_ =
  ImportKeyMaterial'
    { expirationModel = Lude.Nothing,
      keyId = pKeyId_,
      encryptedKeyMaterial = pEncryptedKeyMaterial_,
      validTo = Lude.Nothing,
      importToken = pImportToken_
    }

-- | Specifies whether the key material expires. The default is @KEY_MATERIAL_EXPIRES@ , in which case you must include the @ValidTo@ parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ , you must omit the @ValidTo@ parameter.
--
-- /Note:/ Consider using 'expirationModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmExpirationModel :: Lens.Lens' ImportKeyMaterial (Lude.Maybe ExpirationModelType)
ikmExpirationModel = Lens.lens (expirationModel :: ImportKeyMaterial -> Lude.Maybe ExpirationModelType) (\s a -> s {expirationModel = a} :: ImportKeyMaterial)
{-# DEPRECATED ikmExpirationModel "Use generic-lens or generic-optics with 'expirationModel' instead." #-}

-- | The identifier of the symmetric CMK that receives the imported key material. The CMK's @Origin@ must be @EXTERNAL@ . This must be the same CMK specified in the @KeyID@ parameter of the corresponding 'GetParametersForImport' request.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmKeyId :: Lens.Lens' ImportKeyMaterial Lude.Text
ikmKeyId = Lens.lens (keyId :: ImportKeyMaterial -> Lude.Text) (\s a -> s {keyId = a} :: ImportKeyMaterial)
{-# DEPRECATED ikmKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The encrypted key material to import. The key material must be encrypted with the public wrapping key that 'GetParametersForImport' returned, using the wrapping algorithm that you specified in the same @GetParametersForImport@ request.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'encryptedKeyMaterial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmEncryptedKeyMaterial :: Lens.Lens' ImportKeyMaterial Lude.Base64
ikmEncryptedKeyMaterial = Lens.lens (encryptedKeyMaterial :: ImportKeyMaterial -> Lude.Base64) (\s a -> s {encryptedKeyMaterial = a} :: ImportKeyMaterial)
{-# DEPRECATED ikmEncryptedKeyMaterial "Use generic-lens or generic-optics with 'encryptedKeyMaterial' instead." #-}

-- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. You must omit this parameter when the @ExpirationModel@ parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ . Otherwise it is required.
--
-- /Note:/ Consider using 'validTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmValidTo :: Lens.Lens' ImportKeyMaterial (Lude.Maybe Lude.Timestamp)
ikmValidTo = Lens.lens (validTo :: ImportKeyMaterial -> Lude.Maybe Lude.Timestamp) (\s a -> s {validTo = a} :: ImportKeyMaterial)
{-# DEPRECATED ikmValidTo "Use generic-lens or generic-optics with 'validTo' instead." #-}

-- | The import token that you received in the response to a previous 'GetParametersForImport' request. It must be from the same response that contained the public key that you used to encrypt the key material.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'importToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmImportToken :: Lens.Lens' ImportKeyMaterial Lude.Base64
ikmImportToken = Lens.lens (importToken :: ImportKeyMaterial -> Lude.Base64) (\s a -> s {importToken = a} :: ImportKeyMaterial)
{-# DEPRECATED ikmImportToken "Use generic-lens or generic-optics with 'importToken' instead." #-}

instance Lude.AWSRequest ImportKeyMaterial where
  type Rs ImportKeyMaterial = ImportKeyMaterialResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ImportKeyMaterialResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportKeyMaterial where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ImportKeyMaterial" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportKeyMaterial where
  toJSON ImportKeyMaterial' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExpirationModel" Lude..=) Lude.<$> expirationModel,
            Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("EncryptedKeyMaterial" Lude..= encryptedKeyMaterial),
            ("ValidTo" Lude..=) Lude.<$> validTo,
            Lude.Just ("ImportToken" Lude..= importToken)
          ]
      )

instance Lude.ToPath ImportKeyMaterial where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportKeyMaterial where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportKeyMaterialResponse' smart constructor.
newtype ImportKeyMaterialResponse = ImportKeyMaterialResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportKeyMaterialResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkImportKeyMaterialResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportKeyMaterialResponse
mkImportKeyMaterialResponse pResponseStatus_ =
  ImportKeyMaterialResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmrsResponseStatus :: Lens.Lens' ImportKeyMaterialResponse Lude.Int
ikmrsResponseStatus = Lens.lens (responseStatus :: ImportKeyMaterialResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportKeyMaterialResponse)
{-# DEPRECATED ikmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
