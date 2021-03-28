{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique symmetric data key for client-side encryption. This operation returns a plaintext copy of the data key and a copy that is encrypted under a customer master key (CMK) that you specify. You can use the plaintext key to encrypt your data outside of AWS KMS and store the encrypted data key with the encrypted data.
--
-- @GenerateDataKey@ returns a unique data key for each request. The bytes in the plaintext key are not related to the caller or the CMK.
-- To generate a data key, specify the symmetric CMK that will be used to encrypt the data key. You cannot use an asymmetric CMK to generate data keys. To get the type of your CMK, use the 'DescribeKey' operation. You must also specify the length of the data key. Use either the @KeySpec@ or @NumberOfBytes@ parameters (but not both). For 128-bit and 256-bit data keys, use the @KeySpec@ parameter. 
-- To get only an encrypted copy of the data key, use 'GenerateDataKeyWithoutPlaintext' . To generate an asymmetric data key pair, use the 'GenerateDataKeyPair' or 'GenerateDataKeyPairWithoutPlaintext' operation. To get a cryptographically secure random byte string, use 'GenerateRandom' .
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
-- __How to use your data key__ 
-- We recommend that you use the following pattern to encrypt data locally in your application. You can write your own code or use a client-side encryption library, such as the <https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/ AWS Encryption SDK> , the <https://docs.aws.amazon.com/dynamodb-encryption-client/latest/devguide/ Amazon DynamoDB Encryption Client> , or <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 client-side encryption> to do these tasks for you.
-- To encrypt data outside of AWS KMS:
--
--     * Use the @GenerateDataKey@ operation to get a data key.
--
--
--     * Use the plaintext data key (in the @Plaintext@ field of the response) to encrypt your data outside of AWS KMS. Then erase the plaintext data key from memory.
--
--
--     * Store the encrypted data key (in the @CiphertextBlob@ field of the response) with the encrypted data.
--
--
-- To decrypt data outside of AWS KMS:
--
--     * Use the 'Decrypt' operation to decrypt the encrypted data key. The operation returns a plaintext copy of the data key.
--
--
--     * Use the plaintext data key to decrypt data outside of AWS KMS, then erase the plaintext data key from memory.
--
--
module Network.AWS.KMS.GenerateDataKey
    (
    -- * Creating a request
      GenerateDataKey (..)
    , mkGenerateDataKey
    -- ** Request lenses
    , gdkKeyId
    , gdkEncryptionContext
    , gdkGrantTokens
    , gdkKeySpec
    , gdkNumberOfBytes

    -- * Destructuring the response
    , GenerateDataKeyResponse (..)
    , mkGenerateDataKeyResponse
    -- ** Response lenses
    , gdkrrsCiphertextBlob
    , gdkrrsKeyId
    , gdkrrsPlaintext
    , gdkrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGenerateDataKey' smart constructor.
data GenerateDataKey = GenerateDataKey'
  { keyId :: Types.KeyId
    -- ^ Identifies the symmetric CMK that encrypts the data key.
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
  , encryptionContext :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue)
    -- ^ Specifies the encryption context that will be used when encrypting the data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
  , grantTokens :: Core.Maybe [Types.GrantTokenType]
    -- ^ A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
  , keySpec :: Core.Maybe Types.DataKeySpec
    -- ^ Specifies the length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
  , numberOfBytes :: Core.Maybe Core.Natural
    -- ^ Specifies the length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@ parameter.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataKey' value with any optional fields omitted.
mkGenerateDataKey
    :: Types.KeyId -- ^ 'keyId'
    -> GenerateDataKey
mkGenerateDataKey keyId
  = GenerateDataKey'{keyId, encryptionContext = Core.Nothing,
                     grantTokens = Core.Nothing, keySpec = Core.Nothing,
                     numberOfBytes = Core.Nothing}

-- | Identifies the symmetric CMK that encrypts the data key.
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
gdkKeyId :: Lens.Lens' GenerateDataKey Types.KeyId
gdkKeyId = Lens.field @"keyId"
{-# INLINEABLE gdkKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Specifies the encryption context that will be used when encrypting the data key.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkEncryptionContext :: Lens.Lens' GenerateDataKey (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
gdkEncryptionContext = Lens.field @"encryptionContext"
{-# INLINEABLE gdkEncryptionContext #-}
{-# DEPRECATED encryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead"  #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkGrantTokens :: Lens.Lens' GenerateDataKey (Core.Maybe [Types.GrantTokenType])
gdkGrantTokens = Lens.field @"grantTokens"
{-# INLINEABLE gdkGrantTokens #-}
{-# DEPRECATED grantTokens "Use generic-lens or generic-optics with 'grantTokens' instead"  #-}

-- | Specifies the length of the data key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
--
-- /Note:/ Consider using 'keySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkKeySpec :: Lens.Lens' GenerateDataKey (Core.Maybe Types.DataKeySpec)
gdkKeySpec = Lens.field @"keySpec"
{-# INLINEABLE gdkKeySpec #-}
{-# DEPRECATED keySpec "Use generic-lens or generic-optics with 'keySpec' instead"  #-}

-- | Specifies the length of the data key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For 128-bit (16-byte) and 256-bit (32-byte) data keys, use the @KeySpec@ parameter.
--
-- You must specify either the @KeySpec@ or the @NumberOfBytes@ parameter (but not both) in every @GenerateDataKey@ request.
--
-- /Note:/ Consider using 'numberOfBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkNumberOfBytes :: Lens.Lens' GenerateDataKey (Core.Maybe Core.Natural)
gdkNumberOfBytes = Lens.field @"numberOfBytes"
{-# INLINEABLE gdkNumberOfBytes #-}
{-# DEPRECATED numberOfBytes "Use generic-lens or generic-optics with 'numberOfBytes' instead"  #-}

instance Core.ToQuery GenerateDataKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GenerateDataKey where
        toHeaders GenerateDataKey{..}
          = Core.pure ("X-Amz-Target", "TrentService.GenerateDataKey")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GenerateDataKey where
        toJSON GenerateDataKey{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  ("EncryptionContext" Core..=) Core.<$> encryptionContext,
                  ("GrantTokens" Core..=) Core.<$> grantTokens,
                  ("KeySpec" Core..=) Core.<$> keySpec,
                  ("NumberOfBytes" Core..=) Core.<$> numberOfBytes])

instance Core.AWSRequest GenerateDataKey where
        type Rs GenerateDataKey = GenerateDataKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GenerateDataKeyResponse' Core.<$>
                   (x Core..: "CiphertextBlob") Core.<*> x Core..: "KeyId" Core.<*>
                     x Core..: "Plaintext"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGenerateDataKeyResponse' smart constructor.
data GenerateDataKeyResponse = GenerateDataKeyResponse'
  { ciphertextBlob :: Core.Base64
    -- ^ The encrypted copy of the data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
  , keyId :: Types.KeyId
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
  , plaintext :: Core.Sensitive Core.Base64
    -- ^ The plaintext data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded. Use this data key to encrypt your data outside of KMS. Then, remove it from memory as soon as possible.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataKeyResponse' value with any optional fields omitted.
mkGenerateDataKeyResponse
    :: Core.Base64 -- ^ 'ciphertextBlob'
    -> Types.KeyId -- ^ 'keyId'
    -> Core.Sensitive Core.Base64 -- ^ 'plaintext'
    -> Core.Int -- ^ 'responseStatus'
    -> GenerateDataKeyResponse
mkGenerateDataKeyResponse ciphertextBlob keyId plaintext
  responseStatus
  = GenerateDataKeyResponse'{ciphertextBlob, keyId, plaintext,
                             responseStatus}

-- | The encrypted copy of the data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'ciphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkrrsCiphertextBlob :: Lens.Lens' GenerateDataKeyResponse Core.Base64
gdkrrsCiphertextBlob = Lens.field @"ciphertextBlob"
{-# INLINEABLE gdkrrsCiphertextBlob #-}
{-# DEPRECATED ciphertextBlob "Use generic-lens or generic-optics with 'ciphertextBlob' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the data key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkrrsKeyId :: Lens.Lens' GenerateDataKeyResponse Types.KeyId
gdkrrsKeyId = Lens.field @"keyId"
{-# INLINEABLE gdkrrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The plaintext data key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded. Use this data key to encrypt your data outside of KMS. Then, remove it from memory as soon as possible.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'plaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkrrsPlaintext :: Lens.Lens' GenerateDataKeyResponse (Core.Sensitive Core.Base64)
gdkrrsPlaintext = Lens.field @"plaintext"
{-# INLINEABLE gdkrrsPlaintext #-}
{-# DEPRECATED plaintext "Use generic-lens or generic-optics with 'plaintext' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkrrsResponseStatus :: Lens.Lens' GenerateDataKeyResponse Core.Int
gdkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
