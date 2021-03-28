{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ReEncrypt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decrypts ciphertext and then reencrypts it entirely within AWS KMS. You can use this operation to change the customer master key (CMK) under which data is encrypted, such as when you <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html#rotate-keys-manually manually rotate> a CMK or change the CMK that protects a ciphertext. You can also use it to reencrypt ciphertext under the same CMK, such as to change the <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context encryption context> of a ciphertext.
--
-- The @ReEncrypt@ operation can decrypt ciphertext that was encrypted by using an AWS KMS CMK in an AWS KMS operation, such as 'Encrypt' or 'GenerateDataKey' . It can also decrypt ciphertext that was encrypted by using the public key of an <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html#asymmetric-cmks asymmetric CMK> outside of AWS KMS. However, it cannot decrypt ciphertext produced by other libraries, such as the <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK> or <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption> . These libraries return a ciphertext format that is incompatible with AWS KMS.
-- When you use the @ReEncrypt@ operation, you need to provide information for the decrypt operation and the subsequent encrypt operation.
--
--     * If your ciphertext was encrypted under an asymmetric CMK, you must identify the /source CMK/ , that is, the CMK that encrypted the ciphertext. You must also supply the encryption algorithm that was used. This information is required to decrypt the data.
--
--
--     * It is optional, but you can specify a source CMK even when the ciphertext was encrypted under a symmetric CMK. This ensures that the ciphertext is decrypted only by using a particular CMK. If the CMK that you specify cannot decrypt the ciphertext, the @ReEncrypt@ operation fails.
--
--
--     * To reencrypt the data, you must specify the /destination CMK/ , that is, the CMK that re-encrypts the data after it is decrypted. You can select a symmetric or asymmetric CMK. If the destination CMK is an asymmetric CMK, you must also provide the encryption algorithm. The algorithm that you choose must be compatible with the CMK.
-- /Important:/ When you use an asymmetric CMK to encrypt or reencrypt data, be sure to record the CMK and encryption algorithm that you choose. You will be required to provide the same CMK and encryption algorithm when you decrypt the data. If the CMK and algorithm do not match the values used to encrypt the data, the decrypt operation fails.
-- You are not required to supply the CMK ID and encryption algorithm when you decrypt with symmetric CMKs because AWS KMS stores this information in the ciphertext blob. AWS KMS cannot store metadata in ciphertext generated with asymmetric keys. The standard format for asymmetric key ciphertext does not include configurable fields.
--
--
-- Unlike other AWS KMS API operations, @ReEncrypt@ callers must have two permissions:
--
--     * @kms:ReEncryptFrom@ permission on the source CMK
--
--
--     * @kms:ReEncryptTo@ permission on the destination CMK
--
--
-- To permit reencryption from or to a CMK, include the @"kms:ReEncrypt*"@ permission in your <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html key policy> . This permission is automatically included in the key policy when you use the console to create a CMK. But you must include it manually when you create a CMK programmatically or when you use the 'PutKeyPolicy' operation to set a key policy.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.ReEncrypt
    (
    -- * Creating a request
      ReEncrypt (..)
    , mkReEncrypt
    -- ** Request lenses
    , reCiphertextBlob
    , reDestinationKeyId
    , reDestinationEncryptionAlgorithm
    , reDestinationEncryptionContext
    , reGrantTokens
    , reSourceEncryptionAlgorithm
    , reSourceEncryptionContext
    , reSourceKeyId

    -- * Destructuring the response
    , ReEncryptResponse (..)
    , mkReEncryptResponse
    -- ** Response lenses
    , rerrsCiphertextBlob
    , rerrsDestinationEncryptionAlgorithm
    , rerrsKeyId
    , rerrsSourceEncryptionAlgorithm
    , rerrsSourceKeyId
    , rerrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReEncrypt' smart constructor.
data ReEncrypt = ReEncrypt'
  { ciphertextBlob :: Core.Base64
    -- ^ Ciphertext of the data to reencrypt.
  , destinationKeyId :: Types.DestinationKeyId
    -- ^ A unique identifier for the CMK that is used to reencrypt the data. Specify a symmetric or asymmetric CMK with a @KeyUsage@ value of @ENCRYPT_DECRYPT@ . To find the @KeyUsage@ value of a CMK, use the 'DescribeKey' operation.
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
  , destinationEncryptionAlgorithm :: Core.Maybe Types.EncryptionAlgorithmSpec
    -- ^ Specifies the encryption algorithm that AWS KMS will use to reecrypt the data after it has decrypted it. The default value, @SYMMETRIC_DEFAULT@ , represents the encryption algorithm used for symmetric CMKs.
--
-- This parameter is required only when the destination CMK is an asymmetric CMK.
  , destinationEncryptionContext :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue)
    -- ^ Specifies that encryption context to use when the reencrypting the data.
--
-- A destination encryption context is valid only when the destination CMK is a symmetric CMK. The standard ciphertext format for asymmetric CMKs does not include fields for metadata.
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
  , grantTokens :: Core.Maybe [Types.GrantTokenType]
    -- ^ A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
  , sourceEncryptionAlgorithm :: Core.Maybe Types.EncryptionAlgorithmSpec
    -- ^ Specifies the encryption algorithm that AWS KMS will use to decrypt the ciphertext before it is reencrypted. The default value, @SYMMETRIC_DEFAULT@ , represents the algorithm used for symmetric CMKs.
--
-- Specify the same algorithm that was used to encrypt the ciphertext. If you specify a different algorithm, the decrypt attempt fails.
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK.
  , sourceEncryptionContext :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue)
    -- ^ Specifies the encryption context to use to decrypt the ciphertext. Enter the same encryption context that was used to encrypt the ciphertext.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
  , sourceKeyId :: Core.Maybe Types.SourceKeyId
    -- ^ A unique identifier for the CMK that is used to decrypt the ciphertext before it reencrypts it using the destination CMK.
--
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. Otherwise, AWS KMS uses the metadata that it adds to the ciphertext blob to determine which CMK was used to encrypt the ciphertext. However, you can use this parameter to ensure that a particular CMK (of any kind) is used to decrypt the ciphertext before it is reencrypted.
-- If you specify a @KeyId@ value, the decrypt part of the @ReEncrypt@ operation succeeds only if the specified CMK was used to encrypt the ciphertext.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReEncrypt' value with any optional fields omitted.
mkReEncrypt
    :: Core.Base64 -- ^ 'ciphertextBlob'
    -> Types.DestinationKeyId -- ^ 'destinationKeyId'
    -> ReEncrypt
mkReEncrypt ciphertextBlob destinationKeyId
  = ReEncrypt'{ciphertextBlob, destinationKeyId,
               destinationEncryptionAlgorithm = Core.Nothing,
               destinationEncryptionContext = Core.Nothing,
               grantTokens = Core.Nothing,
               sourceEncryptionAlgorithm = Core.Nothing,
               sourceEncryptionContext = Core.Nothing, sourceKeyId = Core.Nothing}

-- | Ciphertext of the data to reencrypt.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reCiphertextBlob :: Lens.Lens' ReEncrypt Core.Base64
reCiphertextBlob = Lens.field @"ciphertextBlob"
{-# INLINEABLE reCiphertextBlob #-}
{-# DEPRECATED ciphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead"  #-}

-- | A unique identifier for the CMK that is used to reencrypt the data. Specify a symmetric or asymmetric CMK with a @KeyUsage@ value of @ENCRYPT_DECRYPT@ . To find the @KeyUsage@ value of a CMK, use the 'DescribeKey' operation.
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
-- /Note:/ Consider using 'destinationKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reDestinationKeyId :: Lens.Lens' ReEncrypt Types.DestinationKeyId
reDestinationKeyId = Lens.field @"destinationKeyId"
{-# INLINEABLE reDestinationKeyId #-}
{-# DEPRECATED destinationKeyId "Use generic-lens or generic-optics with 'destinationKeyId' instead"  #-}

-- | Specifies the encryption algorithm that AWS KMS will use to reecrypt the data after it has decrypted it. The default value, @SYMMETRIC_DEFAULT@ , represents the encryption algorithm used for symmetric CMKs.
--
-- This parameter is required only when the destination CMK is an asymmetric CMK.
--
-- /Note:/ Consider using 'destinationEncryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reDestinationEncryptionAlgorithm :: Lens.Lens' ReEncrypt (Core.Maybe Types.EncryptionAlgorithmSpec)
reDestinationEncryptionAlgorithm = Lens.field @"destinationEncryptionAlgorithm"
{-# INLINEABLE reDestinationEncryptionAlgorithm #-}
{-# DEPRECATED destinationEncryptionAlgorithm "Use generic-lens or generic-optics with 'destinationEncryptionAlgorithm' instead"  #-}

-- | Specifies that encryption context to use when the reencrypting the data.
--
-- A destination encryption context is valid only when the destination CMK is a symmetric CMK. The standard ciphertext format for asymmetric CMKs does not include fields for metadata.
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'destinationEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reDestinationEncryptionContext :: Lens.Lens' ReEncrypt (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
reDestinationEncryptionContext = Lens.field @"destinationEncryptionContext"
{-# INLINEABLE reDestinationEncryptionContext #-}
{-# DEPRECATED destinationEncryptionContext "Use generic-lens or generic-optics with 'destinationEncryptionContext' instead"  #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reGrantTokens :: Lens.Lens' ReEncrypt (Core.Maybe [Types.GrantTokenType])
reGrantTokens = Lens.field @"grantTokens"
{-# INLINEABLE reGrantTokens #-}
{-# DEPRECATED grantTokens "Use generic-lens or generic-optics with 'grantTokens' instead"  #-}

-- | Specifies the encryption algorithm that AWS KMS will use to decrypt the ciphertext before it is reencrypted. The default value, @SYMMETRIC_DEFAULT@ , represents the algorithm used for symmetric CMKs.
--
-- Specify the same algorithm that was used to encrypt the ciphertext. If you specify a different algorithm, the decrypt attempt fails.
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK.
--
-- /Note:/ Consider using 'sourceEncryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reSourceEncryptionAlgorithm :: Lens.Lens' ReEncrypt (Core.Maybe Types.EncryptionAlgorithmSpec)
reSourceEncryptionAlgorithm = Lens.field @"sourceEncryptionAlgorithm"
{-# INLINEABLE reSourceEncryptionAlgorithm #-}
{-# DEPRECATED sourceEncryptionAlgorithm "Use generic-lens or generic-optics with 'sourceEncryptionAlgorithm' instead"  #-}

-- | Specifies the encryption context to use to decrypt the ciphertext. Enter the same encryption context that was used to encrypt the ciphertext.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'sourceEncryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reSourceEncryptionContext :: Lens.Lens' ReEncrypt (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
reSourceEncryptionContext = Lens.field @"sourceEncryptionContext"
{-# INLINEABLE reSourceEncryptionContext #-}
{-# DEPRECATED sourceEncryptionContext "Use generic-lens or generic-optics with 'sourceEncryptionContext' instead"  #-}

-- | A unique identifier for the CMK that is used to decrypt the ciphertext before it reencrypts it using the destination CMK.
--
-- This parameter is required only when the ciphertext was encrypted under an asymmetric CMK. Otherwise, AWS KMS uses the metadata that it adds to the ciphertext blob to determine which CMK was used to encrypt the ciphertext. However, you can use this parameter to ensure that a particular CMK (of any kind) is used to decrypt the ciphertext before it is reencrypted.
-- If you specify a @KeyId@ value, the decrypt part of the @ReEncrypt@ operation succeeds only if the specified CMK was used to encrypt the ciphertext.
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
-- /Note:/ Consider using 'sourceKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reSourceKeyId :: Lens.Lens' ReEncrypt (Core.Maybe Types.SourceKeyId)
reSourceKeyId = Lens.field @"sourceKeyId"
{-# INLINEABLE reSourceKeyId #-}
{-# DEPRECATED sourceKeyId "Use generic-lens or generic-optics with 'sourceKeyId' instead"  #-}

instance Core.ToQuery ReEncrypt where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ReEncrypt where
        toHeaders ReEncrypt{..}
          = Core.pure ("X-Amz-Target", "TrentService.ReEncrypt") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ReEncrypt where
        toJSON ReEncrypt{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CiphertextBlob" Core..= ciphertextBlob),
                  Core.Just ("DestinationKeyId" Core..= destinationKeyId),
                  ("DestinationEncryptionAlgorithm" Core..=) Core.<$>
                    destinationEncryptionAlgorithm,
                  ("DestinationEncryptionContext" Core..=) Core.<$>
                    destinationEncryptionContext,
                  ("GrantTokens" Core..=) Core.<$> grantTokens,
                  ("SourceEncryptionAlgorithm" Core..=) Core.<$>
                    sourceEncryptionAlgorithm,
                  ("SourceEncryptionContext" Core..=) Core.<$>
                    sourceEncryptionContext,
                  ("SourceKeyId" Core..=) Core.<$> sourceKeyId])

instance Core.AWSRequest ReEncrypt where
        type Rs ReEncrypt = ReEncryptResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ReEncryptResponse' Core.<$>
                   (x Core..:? "CiphertextBlob") Core.<*>
                     x Core..:? "DestinationEncryptionAlgorithm"
                     Core.<*> x Core..:? "KeyId"
                     Core.<*> x Core..:? "SourceEncryptionAlgorithm"
                     Core.<*> x Core..:? "SourceKeyId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReEncryptResponse' smart constructor.
data ReEncryptResponse = ReEncryptResponse'
  { ciphertextBlob :: Core.Maybe Core.Base64
    -- ^ The reencrypted data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
  , destinationEncryptionAlgorithm :: Core.Maybe Types.EncryptionAlgorithmSpec
    -- ^ The encryption algorithm that was used to reencrypt the data.
  , keyId :: Core.Maybe Types.KeyIdType
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to reencrypt the data.
  , sourceEncryptionAlgorithm :: Core.Maybe Types.EncryptionAlgorithmSpec
    -- ^ The encryption algorithm that was used to decrypt the ciphertext before it was reencrypted.
  , sourceKeyId :: Core.Maybe Types.KeyIdType
    -- ^ Unique identifier of the CMK used to originally encrypt the data.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReEncryptResponse' value with any optional fields omitted.
mkReEncryptResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReEncryptResponse
mkReEncryptResponse responseStatus
  = ReEncryptResponse'{ciphertextBlob = Core.Nothing,
                       destinationEncryptionAlgorithm = Core.Nothing,
                       keyId = Core.Nothing, sourceEncryptionAlgorithm = Core.Nothing,
                       sourceKeyId = Core.Nothing, responseStatus}

-- | The reencrypted data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerrsCiphertextBlob :: Lens.Lens' ReEncryptResponse (Core.Maybe Core.Base64)
rerrsCiphertextBlob = Lens.field @"ciphertextBlob"
{-# INLINEABLE rerrsCiphertextBlob #-}
{-# DEPRECATED ciphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead"  #-}

-- | The encryption algorithm that was used to reencrypt the data.
--
-- /Note:/ Consider using 'destinationEncryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerrsDestinationEncryptionAlgorithm :: Lens.Lens' ReEncryptResponse (Core.Maybe Types.EncryptionAlgorithmSpec)
rerrsDestinationEncryptionAlgorithm = Lens.field @"destinationEncryptionAlgorithm"
{-# INLINEABLE rerrsDestinationEncryptionAlgorithm #-}
{-# DEPRECATED destinationEncryptionAlgorithm "Use generic-lens or generic-optics with 'destinationEncryptionAlgorithm' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that was used to reencrypt the data.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerrsKeyId :: Lens.Lens' ReEncryptResponse (Core.Maybe Types.KeyIdType)
rerrsKeyId = Lens.field @"keyId"
{-# INLINEABLE rerrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The encryption algorithm that was used to decrypt the ciphertext before it was reencrypted.
--
-- /Note:/ Consider using 'sourceEncryptionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerrsSourceEncryptionAlgorithm :: Lens.Lens' ReEncryptResponse (Core.Maybe Types.EncryptionAlgorithmSpec)
rerrsSourceEncryptionAlgorithm = Lens.field @"sourceEncryptionAlgorithm"
{-# INLINEABLE rerrsSourceEncryptionAlgorithm #-}
{-# DEPRECATED sourceEncryptionAlgorithm "Use generic-lens or generic-optics with 'sourceEncryptionAlgorithm' instead"  #-}

-- | Unique identifier of the CMK used to originally encrypt the data.
--
-- /Note:/ Consider using 'sourceKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerrsSourceKeyId :: Lens.Lens' ReEncryptResponse (Core.Maybe Types.KeyIdType)
rerrsSourceKeyId = Lens.field @"sourceKeyId"
{-# INLINEABLE rerrsSourceKeyId #-}
{-# DEPRECATED sourceKeyId "Use generic-lens or generic-optics with 'sourceKeyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerrsResponseStatus :: Lens.Lens' ReEncryptResponse Core.Int
rerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
