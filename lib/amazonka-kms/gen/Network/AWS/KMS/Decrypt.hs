{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Decrypt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext that was encrypted by a AWS KMS customer master key (CMK) using any of the following operations:
--
--
--     * 'Encrypt'
--
--
--     * 'GenerateDataKey'
--
--
--     * 'GenerateDataKeyPair'
--
--
--     * 'GenerateDataKeyWithoutPlaintext'
--
--
--     * 'GenerateDataKeyPairWithoutPlaintext'
--
--
-- You can use this operation to decrypt ciphertext that was encrypted under a symmetric or asymmetric CMK. When the CMK is asymmetric, you must specify the CMK and the encryption algorithm that was used to encrypt the ciphertext. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
-- The Decrypt operation also decrypts ciphertext that was encrypted outside of AWS KMS by the public key in an AWS KMS asymmetric CMK. However, it cannot decrypt ciphertext produced by other libraries, such as the <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK> or <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption> . These libraries return a ciphertext format that is incompatible with AWS KMS.
-- If the ciphertext was encrypted under a symmetric CMK, you do not need to specify the CMK or the encryption algorithm. AWS KMS can get this information from metadata that it adds to the symmetric ciphertext blob. However, if you prefer, you can specify the @KeyId@ to ensure that a particular CMK is used to decrypt the ciphertext. If you specify a different CMK than the one used to encrypt the ciphertext, the @Decrypt@ operation fails.
-- Whenever possible, use key policies to give users permission to call the Decrypt operation on a particular CMK, instead of using IAM policies. Otherwise, you might create an IAM user policy that gives the user Decrypt permission on all CMKs. This user could decrypt ciphertext that was encrypted by CMKs in other accounts if the key policy for the cross-account CMK permits it. If you must use an IAM policy for @Decrypt@ permissions, limit the user to particular CMKs or particular trusted accounts.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.Decrypt
  ( -- * Creating a request
    Decrypt (..),
    mkDecrypt,

    -- ** Request lenses
    dfCiphertextBlob,
    dfEncryptionAlgorithm,
    dfEncryptionContext,
    dfGrantTokens,
    dfKeyId,

    -- * Destructuring the response
    DecryptResponse (..),
    mkDecryptResponse,

    -- ** Response lenses
    drrsEncryptionAlgorithm,
    drrsKeyId,
    drrsPlaintext,
    drrsResponseStatus,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDecrypt' smart constructor.
data Decrypt = Decrypt'
  { -- | Ciphertext to be decrypted. The blob includes metadata.
    ciphertextBlob :: Core.Base64,
    -- | Specifies the encryption algorithm that will be used to decrypt the ciphertext. Specify the same algorithm that was used to encrypt the data. If you specify a different algorithm, the @Decrypt@ operation fails.
    --
    -- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. The default value, @SYMMETRIC_DEFAULT@ , represents the only supported algorithm that is valid for symmetric CMKs.
    encryptionAlgorithm :: Core.Maybe Types.EncryptionAlgorithmSpec,
    -- | Specifies the encryption context to use when decrypting the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.
    --
    -- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
    encryptionContext :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue),
    -- | A list of grant tokens.
    --
    -- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
    grantTokens :: Core.Maybe [Types.GrantTokenType],
    -- | Specifies the customer master key (CMK) that AWS KMS will use to decrypt the ciphertext. Enter a key ID of the CMK that was used to encrypt the ciphertext.
    --
    -- If you specify a @KeyId@ value, the @Decrypt@ operation succeeds only if the specified CMK was used to encrypt the ciphertext.
    -- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. Otherwise, AWS KMS uses the metadata that it adds to the ciphertext blob to determine which CMK was used to encrypt the ciphertext. However, you can use this parameter to ensure that a particular CMK (of any kind) is used to decrypt the ciphertext.
    -- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ .
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
    keyId :: Core.Maybe Types.KeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Decrypt' value with any optional fields omitted.
mkDecrypt ::
  -- | 'ciphertextBlob'
  Core.Base64 ->
  Decrypt
mkDecrypt ciphertextBlob =
  Decrypt'
    { ciphertextBlob,
      encryptionAlgorithm = Core.Nothing,
      encryptionContext = Core.Nothing,
      grantTokens = Core.Nothing,
      keyId = Core.Nothing
    }

-- | Ciphertext to be decrypted. The blob includes metadata.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCiphertextBlob :: Lens.Lens' Decrypt Core.Base64
dfCiphertextBlob = Lens.field @"ciphertextBlob"
{-# DEPRECATED dfCiphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead." #-}

-- | Specifies the encryption algorithm that will be used to decrypt the ciphertext. Specify the same algorithm that was used to encrypt the data. If you specify a different algorithm, the @Decrypt@ operation fails.
--
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. The default value, @SYMMETRIC_DEFAULT@ , represents the only supported algorithm that is valid for symmetric CMKs.
--
-- /Note:/ Consider using 'encryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfEncryptionAlgorithm :: Lens.Lens' Decrypt (Core.Maybe Types.EncryptionAlgorithmSpec)
dfEncryptionAlgorithm = Lens.field @"encryptionAlgorithm"
{-# DEPRECATED dfEncryptionAlgorithm "Use generic-lens or generic-optics with 'encryptionAlgorithm' instead." #-}

-- | Specifies the encryption context to use when decrypting the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfEncryptionContext :: Lens.Lens' Decrypt (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
dfEncryptionContext = Lens.field @"encryptionContext"
{-# DEPRECATED dfEncryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead." #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfGrantTokens :: Lens.Lens' Decrypt (Core.Maybe [Types.GrantTokenType])
dfGrantTokens = Lens.field @"grantTokens"
{-# DEPRECATED dfGrantTokens "Use generic-lens or generic-optics with 'grantTokens' instead." #-}

-- | Specifies the customer master key (CMK) that AWS KMS will use to decrypt the ciphertext. Enter a key ID of the CMK that was used to encrypt the ciphertext.
--
-- If you specify a @KeyId@ value, the @Decrypt@ operation succeeds only if the specified CMK was used to encrypt the ciphertext.
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. Otherwise, AWS KMS uses the metadata that it adds to the ciphertext blob to determine which CMK was used to encrypt the ciphertext. However, you can use this parameter to ensure that a particular CMK (of any kind) is used to decrypt the ciphertext.
-- To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with @"alias/"@ .
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
dfKeyId :: Lens.Lens' Decrypt (Core.Maybe Types.KeyId)
dfKeyId = Lens.field @"keyId"
{-# DEPRECATED dfKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Core.FromJSON Decrypt where
  toJSON Decrypt {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CiphertextBlob" Core..= ciphertextBlob),
            ("EncryptionAlgorithm" Core..=) Core.<$> encryptionAlgorithm,
            ("EncryptionContext" Core..=) Core.<$> encryptionContext,
            ("GrantTokens" Core..=) Core.<$> grantTokens,
            ("KeyId" Core..=) Core.<$> keyId
          ]
      )

instance Core.AWSRequest Decrypt where
  type Rs Decrypt = DecryptResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.Decrypt")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DecryptResponse'
            Core.<$> (x Core..:? "EncryptionAlgorithm")
            Core.<*> (x Core..:? "KeyId")
            Core.<*> (x Core..:? "Plaintext")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDecryptResponse' smart constructor.
data DecryptResponse = DecryptResponse'
  { -- | The encryption algorithm that was used to decrypt the ciphertext.
    encryptionAlgorithm :: Core.Maybe Types.EncryptionAlgorithmSpec,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to decrypt the ciphertext.
    keyId :: Core.Maybe Types.KeyId,
    -- | Decrypted plaintext data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
    plaintext :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecryptResponse' value with any optional fields omitted.
mkDecryptResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DecryptResponse
mkDecryptResponse responseStatus =
  DecryptResponse'
    { encryptionAlgorithm = Core.Nothing,
      keyId = Core.Nothing,
      plaintext = Core.Nothing,
      responseStatus
    }

-- | The encryption algorithm that was used to decrypt the ciphertext.
--
-- /Note:/ Consider using 'encryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsEncryptionAlgorithm :: Lens.Lens' DecryptResponse (Core.Maybe Types.EncryptionAlgorithmSpec)
drrsEncryptionAlgorithm = Lens.field @"encryptionAlgorithm"
{-# DEPRECATED drrsEncryptionAlgorithm "Use generic-lens or generic-optics with 'encryptionAlgorithm' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to decrypt the ciphertext.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsKeyId :: Lens.Lens' DecryptResponse (Core.Maybe Types.KeyId)
drrsKeyId = Lens.field @"keyId"
{-# DEPRECATED drrsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Decrypted plaintext data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'plaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsPlaintext :: Lens.Lens' DecryptResponse (Core.Maybe (Core.Sensitive Core.Base64))
drrsPlaintext = Lens.field @"plaintext"
{-# DEPRECATED drrsPlaintext "Use generic-lens or generic-optics with 'plaintext' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DecryptResponse Core.Int
drrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
