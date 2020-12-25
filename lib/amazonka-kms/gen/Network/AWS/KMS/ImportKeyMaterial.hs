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
    ikmKeyId,
    ikmImportToken,
    ikmEncryptedKeyMaterial,
    ikmExpirationModel,
    ikmValidTo,

    -- * Destructuring the response
    ImportKeyMaterialResponse (..),
    mkImportKeyMaterialResponse,

    -- ** Response lenses
    ikmrrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportKeyMaterial' smart constructor.
data ImportKeyMaterial = ImportKeyMaterial'
  { -- | The identifier of the symmetric CMK that receives the imported key material. The CMK's @Origin@ must be @EXTERNAL@ . This must be the same CMK specified in the @KeyID@ parameter of the corresponding 'GetParametersForImport' request.
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
    keyId :: Types.KeyIdType,
    -- | The import token that you received in the response to a previous 'GetParametersForImport' request. It must be from the same response that contained the public key that you used to encrypt the key material.
    importToken :: Core.Base64,
    -- | The encrypted key material to import. The key material must be encrypted with the public wrapping key that 'GetParametersForImport' returned, using the wrapping algorithm that you specified in the same @GetParametersForImport@ request.
    encryptedKeyMaterial :: Core.Base64,
    -- | Specifies whether the key material expires. The default is @KEY_MATERIAL_EXPIRES@ , in which case you must include the @ValidTo@ parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ , you must omit the @ValidTo@ parameter.
    expirationModel :: Core.Maybe Types.ExpirationModelType,
    -- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. You must omit this parameter when the @ExpirationModel@ parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ . Otherwise it is required.
    validTo :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ImportKeyMaterial' value with any optional fields omitted.
mkImportKeyMaterial ::
  -- | 'keyId'
  Types.KeyIdType ->
  -- | 'importToken'
  Core.Base64 ->
  -- | 'encryptedKeyMaterial'
  Core.Base64 ->
  ImportKeyMaterial
mkImportKeyMaterial keyId importToken encryptedKeyMaterial =
  ImportKeyMaterial'
    { keyId,
      importToken,
      encryptedKeyMaterial,
      expirationModel = Core.Nothing,
      validTo = Core.Nothing
    }

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
ikmKeyId :: Lens.Lens' ImportKeyMaterial Types.KeyIdType
ikmKeyId = Lens.field @"keyId"
{-# DEPRECATED ikmKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The import token that you received in the response to a previous 'GetParametersForImport' request. It must be from the same response that contained the public key that you used to encrypt the key material.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'importToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmImportToken :: Lens.Lens' ImportKeyMaterial Core.Base64
ikmImportToken = Lens.field @"importToken"
{-# DEPRECATED ikmImportToken "Use generic-lens or generic-optics with 'importToken' instead." #-}

-- | The encrypted key material to import. The key material must be encrypted with the public wrapping key that 'GetParametersForImport' returned, using the wrapping algorithm that you specified in the same @GetParametersForImport@ request.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'encryptedKeyMaterial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmEncryptedKeyMaterial :: Lens.Lens' ImportKeyMaterial Core.Base64
ikmEncryptedKeyMaterial = Lens.field @"encryptedKeyMaterial"
{-# DEPRECATED ikmEncryptedKeyMaterial "Use generic-lens or generic-optics with 'encryptedKeyMaterial' instead." #-}

-- | Specifies whether the key material expires. The default is @KEY_MATERIAL_EXPIRES@ , in which case you must include the @ValidTo@ parameter. When this parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ , you must omit the @ValidTo@ parameter.
--
-- /Note:/ Consider using 'expirationModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmExpirationModel :: Lens.Lens' ImportKeyMaterial (Core.Maybe Types.ExpirationModelType)
ikmExpirationModel = Lens.field @"expirationModel"
{-# DEPRECATED ikmExpirationModel "Use generic-lens or generic-optics with 'expirationModel' instead." #-}

-- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. You must omit this parameter when the @ExpirationModel@ parameter is set to @KEY_MATERIAL_DOES_NOT_EXPIRE@ . Otherwise it is required.
--
-- /Note:/ Consider using 'validTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmValidTo :: Lens.Lens' ImportKeyMaterial (Core.Maybe Core.NominalDiffTime)
ikmValidTo = Lens.field @"validTo"
{-# DEPRECATED ikmValidTo "Use generic-lens or generic-optics with 'validTo' instead." #-}

instance Core.FromJSON ImportKeyMaterial where
  toJSON ImportKeyMaterial {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            Core.Just ("ImportToken" Core..= importToken),
            Core.Just ("EncryptedKeyMaterial" Core..= encryptedKeyMaterial),
            ("ExpirationModel" Core..=) Core.<$> expirationModel,
            ("ValidTo" Core..=) Core.<$> validTo
          ]
      )

instance Core.AWSRequest ImportKeyMaterial where
  type Rs ImportKeyMaterial = ImportKeyMaterialResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.ImportKeyMaterial")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportKeyMaterialResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportKeyMaterialResponse' smart constructor.
newtype ImportKeyMaterialResponse = ImportKeyMaterialResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportKeyMaterialResponse' value with any optional fields omitted.
mkImportKeyMaterialResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportKeyMaterialResponse
mkImportKeyMaterialResponse responseStatus =
  ImportKeyMaterialResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikmrrsResponseStatus :: Lens.Lens' ImportKeyMaterialResponse Core.Int
ikmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ikmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
