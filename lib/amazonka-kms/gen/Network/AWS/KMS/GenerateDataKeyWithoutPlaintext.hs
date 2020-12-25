{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique symmetric data key. This operation returns a data key that is encrypted under a customer master key (CMK) that you specify. To request an asymmetric data key pair, use the 'GenerateDataKeyPair' or 'GenerateDataKeyPairWithoutPlaintext' operations.
--
-- @GenerateDataKeyWithoutPlaintext@ is identical to the 'GenerateDataKey' operation except that returns only the encrypted copy of the data key. This operation is useful for systems that need to encrypt data at some point, but not immediately. When you need to encrypt the data, you call the 'Decrypt' operation on the encrypted copy of the key.
-- It's also useful in distributed systems with different levels of trust. For example, you might store encrypted data in containers. One component of your system creates new containers and stores an encrypted data key with each container. Then, a different component puts the data into the containers. That component first decrypts the data key, uses the plaintext data key to encrypt data, puts the encrypted data into the container, and then destroys the plaintext data key. In this system, the component that creates the containers never sees the plaintext data key.
-- @GenerateDataKeyWithoutPlaintext@ returns a unique data key for each request. The bytes in the keys are not related to the caller or CMK that is used to encrypt the private key.
-- To generate a data key, you must specify the symmetric customer master key (CMK) that is used to encrypt the data key. You cannot use an asymmetric CMK to generate a data key. To get the type of your CMK, use the 'DescribeKey' operation.
-- If the operation succeeds, you will find the encrypted copy of the data key in the @CiphertextBlob@ field.
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
  ( -- * Creating a request
    GenerateDataKeyWithoutPlaintext (..),
    mkGenerateDataKeyWithoutPlaintext,

    -- ** Request lenses
    gdkwpKeyId,
    gdkwpEncryptionContext,
    gdkwpGrantTokens,
    gdkwpKeySpec,
    gdkwpNumberOfBytes,

    -- * Destructuring the response
    GenerateDataKeyWithoutPlaintextResponse (..),
    mkGenerateDataKeyWithoutPlaintextResponse,

    -- ** Response lenses
    gdkwprrsCiphertextBlob,
    gdkwprrsKeyId,
    gdkwprrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGenerateDataKeyWithoutPlaintext' smart constructor.
data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext'
  { -- | The identifier of the symmetric customer master key (CMK) that encrypts the data key.
    --
    -- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN.
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Alias name: @alias/ExampleAlias@
    --
    --
    --     * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
    keyId :: Types.KeyIdType,
    -- | Specifies the encryption context that will be used when encrypting the data key.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
    encryptionContext :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue),
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Core.Maybe [Types.GrantTokenType],
    -- | The length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
    keySpec :: Core.Maybe Types.DataKeySpec,
    -- | The length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
    numberOfBytes :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataKeyWithoutPlaintext' value with any optional fields omitted.
mkGenerateDataKeyWithoutPlaintext ::
  -- | 'keyId'
  Types.KeyIdType ->
  GenerateDataKeyWithoutPlaintext
mkGenerateDataKeyWithoutPlaintext keyId =
  GenerateDataKeyWithoutPlaintext'
    { keyId,
      encryptionContext = Core.Nothing,
      grantTokens = Core.Nothing,
      keySpec = Core.Nothing,
      numberOfBytes = Core.Nothing
    }

-- | The identifier of the symmetric customer master key (CMK) that encrypts the data key.
--
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ . To specify a CMK in a different AWS account, you must use the key ARN or alias ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpKeyId :: Lens.Lens' GenerateDataKeyWithoutPlaintext Types.KeyIdType
gdkwpKeyId = Lens.field @"keyId"
{-# DEPRECATED gdkwpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Specifies the encryption context that will be used when encrypting the data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpEncryptionContext :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
gdkwpEncryptionContext = Lens.field @"encryptionContext"
{-# DEPRECATED gdkwpEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpGrantTokens :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Core.Maybe [Types.GrantTokenType])
gdkwpGrantTokens = Lens.field @"grantTokens"
{-# DEPRECATED gdkwpGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | The length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- /Note:/ Consider using 'keySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpKeySpec :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Core.Maybe Types.DataKeySpec)
gdkwpKeySpec = Lens.field @"keySpec"
{-# DEPRECATED gdkwpKeySpec "Use generic-lens or generic-optics with 'keySpec' instead." #-}

-- | The length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
--
-- /Note:/ Consider using 'numberOfBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwpNumberOfBytes :: Lens.Lens' GenerateDataKeyWithoutPlaintext (Core.Maybe Core.Natural)
gdkwpNumberOfBytes = Lens.field @"numberOfBytes"
{-# DEPRECATED gdkwpNumberOfBytes "Use generic-lens or generic-optics with 'numberOfBytes' instead." #-}

instance Core.FromJSON GenerateDataKeyWithoutPlaintext where
  toJSON GenerateDataKeyWithoutPlaintext {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            ("EncryptionContext" Core..=) Core.<$> encryptionContext,
            ("GrantTokens" Core..=) Core.<$> grantTokens,
            ("KeySpec" Core..=) Core.<$> keySpec,
            ("NumberOfBytes" Core..=) Core.<$> numberOfBytes
          ]
      )

instance Core.AWSRequest GenerateDataKeyWithoutPlaintext where
  type
    Rs GenerateDataKeyWithoutPlaintext =
      GenerateDataKeyWithoutPlaintextResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "TrentService.GenerateDataKeyWithoutPlaintext")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateDataKeyWithoutPlaintextResponse'
            Core.<$> (x Core..:? "CiphertextBlob")
            Core.<*> (x Core..:? "KeyId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGenerateDataKeyWithoutPlaintextResponse' smart constructor.
data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse'
  { -- | The encrypted data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    ciphertextBlob :: Core.Maybe Core.Base64,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
    keyId :: Core.Maybe Types.KeyId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataKeyWithoutPlaintextResponse' value with any optional fields omitted.
mkGenerateDataKeyWithoutPlaintextResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GenerateDataKeyWithoutPlaintextResponse
mkGenerateDataKeyWithoutPlaintextResponse responseStatus =
  GenerateDataKeyWithoutPlaintextResponse'
    { ciphertextBlob =
        Core.Nothing,
      keyId = Core.Nothing,
      responseStatus
    }

-- | The encrypted data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwprrsCiphertextBlob :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse (Core.Maybe Core.Base64)
gdkwprrsCiphertextBlob = Lens.field @"ciphertextBlob"
{-# DEPRECATED gdkwprrsCiphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwprrsKeyId :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse (Core.Maybe Types.KeyId)
gdkwprrsKeyId = Lens.field @"keyId"
{-# DEPRECATED gdkwprrsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkwprrsResponseStatus :: Lens.Lens' GenerateDataKeyWithoutPlaintextResponse Core.Int
gdkwprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdkwprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
