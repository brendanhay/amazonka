{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Encrypt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Encrypts plaintext into ciphertext by using a customer master key (CMK). The @Encrypt@ operation has two primary use cases:
--
--
--     * You can encrypt small amounts of arbitrary data, such as a personal identifier or database password, or other sensitive information. 
--
--
--     * You can use the @Encrypt@ operation to move encrypted data from one AWS Region to another. For example, in Region A, generate a data key and use the plaintext key to encrypt your data. Then, in Region A, use the @Encrypt@ operation to encrypt the plaintext data key under a CMK in Region B. Now, you can move the encrypted data and the encrypted data key to Region B. When necessary, you can decrypt the encrypted data key and the encrypted data entirely within in Region B.
--
--
-- You don't need to use the @Encrypt@ operation to encrypt a data key. The 'GenerateDataKey' and 'GenerateDataKeyPair' operations return a plaintext data key and an encrypted copy of that data key.
-- When you encrypt data, you must specify a symmetric or asymmetric CMK to use in the encryption operation. The CMK must have a @KeyUsage@ value of @ENCRYPT_DECRYPT.@ To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation. 
-- If you use a symmetric CMK, you can use an encryption context to add additional security to your encryption operation. If you specify an @EncryptionContext@ when encrypting data, you must specify the same encryption context (a case-sensitive exact match) when decrypting the data. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- If you specify an asymmetric CMK, you must also specify the encryption algorithm. The algorithm must be compatible with the CMK type.
-- /Important:/ When you use an asymmetric CMK to encrypt or reencrypt data, be sure to record the CMK and encryption algorithm that you choose. You will be required to provide the same CMK and encryption algorithm when you decrypt the data. If the CMK and algorithm do not match the values used to encrypt the data, the decrypt operation fails.
-- You are not required to supply the CMK ID and encryption algorithm when you decrypt with symmetric CMKs because AWS KMS stores this information in the ciphertext blob. AWS KMS cannot store metadata in ciphertext generated with asymmetric keys. The standard format for asymmetric key ciphertext does not include configurable fields.
-- The maximum size of the data that you can encrypt varies with the type of CMK and the encryption algorithm that you choose.
--
--     * Symmetric CMKs
--
--     * @SYMMETRIC_DEFAULT@ : 4096 bytes
--
--
--
--
--     * @RSA_2048@ 
--
--     * @RSAES_OAEP_SHA_1@ : 214 bytes
--
--
--     * @RSAES_OAEP_SHA_256@ : 190 bytes
--
--
--
--
--     * @RSA_3072@ 
--
--     * @RSAES_OAEP_SHA_1@ : 342 bytes
--
--
--     * @RSAES_OAEP_SHA_256@ : 318 bytes
--
--
--
--
--     * @RSA_4096@ 
--
--     * @RSAES_OAEP_SHA_1@ : 470 bytes
--
--
--     * @RSAES_OAEP_SHA_256@ : 446 bytes
--
--
--
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
-- To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.
module Network.AWS.KMS.Encrypt
    (
    -- * Creating a request
      Encrypt (..)
    , mkEncrypt
    -- ** Request lenses
    , eKeyId
    , ePlaintext
    , eEncryptionAlgorithm
    , eEncryptionContext
    , eGrantTokens

    -- * Destructuring the response
    , EncryptResponse (..)
    , mkEncryptResponse
    -- ** Response lenses
    , errsCiphertextBlob
    , errsEncryptionAlgorithm
    , errsKeyId
    , errsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEncrypt' smart constructor.
data Encrypt = Encrypt'
  { keyId :: Types.KeyIdType
    -- ^ A unique identifier for the customer master key (CMK).
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
  , plaintext :: Core.Sensitive Core.Base64
    -- ^ Data to be encrypted.
  , encryptionAlgorithm :: Core.Maybe Types.EncryptionAlgorithmSpec
    -- ^ Specifies the encryption algorithm that AWS KMS will use to encrypt the plaintext message. The algorithm must be compatible with the CMK that you specify.
--
-- This parameter is required only for asymmetric CMKs. The default value, @SYMMETRIC_DEFAULT@ , is the algorithm used for symmetric CMKs. If you are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
  , encryptionContext :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue)
    -- ^ Specifies the encryption context that will be used to encrypt the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context. 
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
  , grantTokens :: Core.Maybe [Types.GrantTokenType]
    -- ^ A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Encrypt' value with any optional fields omitted.
mkEncrypt
    :: Types.KeyIdType -- ^ 'keyId'
    -> Core.Sensitive Core.Base64 -- ^ 'plaintext'
    -> Encrypt
mkEncrypt keyId plaintext
  = Encrypt'{keyId, plaintext, encryptionAlgorithm = Core.Nothing,
             encryptionContext = Core.Nothing, grantTokens = Core.Nothing}

-- | A unique identifier for the customer master key (CMK).
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
eKeyId :: Lens.Lens' Encrypt Types.KeyIdType
eKeyId = Lens.field @"keyId"
{-# INLINEABLE eKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Data to be encrypted.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'plaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePlaintext :: Lens.Lens' Encrypt (Core.Sensitive Core.Base64)
ePlaintext = Lens.field @"plaintext"
{-# INLINEABLE ePlaintext #-}
{-# DEPRECATED plaintext "Use generic-lens or generic-optics with 'plaintext' instead"  #-}

-- | Specifies the encryption algorithm that AWS KMS will use to encrypt the plaintext message. The algorithm must be compatible with the CMK that you specify.
--
-- This parameter is required only for asymmetric CMKs. The default value, @SYMMETRIC_DEFAULT@ , is the algorithm used for symmetric CMKs. If you are using an asymmetric CMK, we recommend RSAES_OAEP_SHA_256.
--
-- /Note:/ Consider using 'encryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncryptionAlgorithm :: Lens.Lens' Encrypt (Core.Maybe Types.EncryptionAlgorithmSpec)
eEncryptionAlgorithm = Lens.field @"encryptionAlgorithm"
{-# INLINEABLE eEncryptionAlgorithm #-}
{-# DEPRECATED encryptionAlgorithm "Use generic-lens or generic-optics with 'encryptionAlgorithm' instead"  #-}

-- | Specifies the encryption context that will be used to encrypt the data. An encryption context is valid only for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> with a symmetric CMK. The standard asymmetric encryption algorithms that AWS KMS uses do not support an encryption context. 
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncryptionContext :: Lens.Lens' Encrypt (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
eEncryptionContext = Lens.field @"encryptionContext"
{-# INLINEABLE eEncryptionContext #-}
{-# DEPRECATED encryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead"  #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eGrantTokens :: Lens.Lens' Encrypt (Core.Maybe [Types.GrantTokenType])
eGrantTokens = Lens.field @"grantTokens"
{-# INLINEABLE eGrantTokens #-}
{-# DEPRECATED grantTokens "Use generic-lens or generic-optics with 'grantTokens' instead"  #-}

instance Core.ToQuery Encrypt where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders Encrypt where
        toHeaders Encrypt{..}
          = Core.pure ("X-Amz-Target", "TrentService.Encrypt") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON Encrypt where
        toJSON Encrypt{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  Core.Just ("Plaintext" Core..= plaintext),
                  ("EncryptionAlgorithm" Core..=) Core.<$> encryptionAlgorithm,
                  ("EncryptionContext" Core..=) Core.<$> encryptionContext,
                  ("GrantTokens" Core..=) Core.<$> grantTokens])

instance Core.AWSRequest Encrypt where
        type Rs Encrypt = EncryptResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 EncryptResponse' Core.<$>
                   (x Core..:? "CiphertextBlob") Core.<*>
                     x Core..:? "EncryptionAlgorithm"
                     Core.<*> x Core..:? "KeyId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEncryptResponse' smart constructor.
data EncryptResponse = EncryptResponse'
  { ciphertextBlob :: Core.Maybe Core.Base64
    -- ^ The encrypted plaintext. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
  , encryptionAlgorithm :: Core.Maybe Types.EncryptionAlgorithmSpec
    -- ^ The encryption algorithm that was used to encrypt the plaintext.
  , keyId :: Core.Maybe Types.KeyId
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to encrypt the plaintext.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptResponse' value with any optional fields omitted.
mkEncryptResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EncryptResponse
mkEncryptResponse responseStatus
  = EncryptResponse'{ciphertextBlob = Core.Nothing,
                     encryptionAlgorithm = Core.Nothing, keyId = Core.Nothing,
                     responseStatus}

-- | The encrypted plaintext. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errsCiphertextBlob :: Lens.Lens' EncryptResponse (Core.Maybe Core.Base64)
errsCiphertextBlob = Lens.field @"ciphertextBlob"
{-# INLINEABLE errsCiphertextBlob #-}
{-# DEPRECATED ciphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead"  #-}

-- | The encryption algorithm that was used to encrypt the plaintext.
--
-- /Note:/ Consider using 'encryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errsEncryptionAlgorithm :: Lens.Lens' EncryptResponse (Core.Maybe Types.EncryptionAlgorithmSpec)
errsEncryptionAlgorithm = Lens.field @"encryptionAlgorithm"
{-# INLINEABLE errsEncryptionAlgorithm #-}
{-# DEPRECATED encryptionAlgorithm "Use generic-lens or generic-optics with 'encryptionAlgorithm' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to encrypt the plaintext.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errsKeyId :: Lens.Lens' EncryptResponse (Core.Maybe Types.KeyId)
errsKeyId = Lens.field @"keyId"
{-# INLINEABLE errsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
errsResponseStatus :: Lens.Lens' EncryptResponse Core.Int
errsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE errsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
