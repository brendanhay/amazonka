{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a unique asymmetric data key pair. The @GenerateDataKeyPair@ operation returns a plaintext public key, a plaintext private key, and a copy of the private key that is encrypted under the symmetric CMK you specify. You can use the data key pair to perform asymmetric cryptography outside of AWS KMS.
--
-- @GenerateDataKeyPair@ returns a unique data key pair for each request. The bytes in the keys are not related to the caller or the CMK that is used to encrypt the private key.
-- You can use the public key that @GenerateDataKeyPair@ returns to encrypt data or verify a signature outside of AWS KMS. Then, store the encrypted private key with the data. When you are ready to decrypt data or sign a message, you can use the 'Decrypt' operation to decrypt the encrypted private key.
-- To generate a data key pair, you must specify a symmetric customer master key (CMK) to encrypt the private key in a data key pair. You cannot use an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation. 
-- If you are using the data key pair to encrypt data, or for any operation where you don't immediately need a private key, consider using the 'GenerateDataKeyPairWithoutPlaintext' operation. @GenerateDataKeyPairWithoutPlaintext@ returns a plaintext public key and an encrypted private key, but omits the plaintext private key that you need only to decrypt ciphertext or sign a message. Later, when you need to decrypt the data or sign a message, use the 'Decrypt' operation to decrypt the encrypted private key in the data key pair.
-- You can use the optional encryption context to add additional security to the encryption operation. If you specify an @EncryptionContext@ , you must specify the same encryption context (a case-sensitive exact match) when decrypting the encrypted data key. Otherwise, the request to decrypt fails with an @InvalidCiphertextException@ . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GenerateDataKeyPair
    (
    -- * Creating a request
      GenerateDataKeyPair (..)
    , mkGenerateDataKeyPair
    -- ** Request lenses
    , gdkpKeyId
    , gdkpKeyPairSpec
    , gdkpEncryptionContext
    , gdkpGrantTokens

    -- * Destructuring the response
    , GenerateDataKeyPairResponse (..)
    , mkGenerateDataKeyPairResponse
    -- ** Response lenses
    , gdkprrsKeyId
    , gdkprrsKeyPairSpec
    , gdkprrsPrivateKeyCiphertextBlob
    , gdkprrsPrivateKeyPlaintext
    , gdkprrsPublicKey
    , gdkprrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGenerateDataKeyPair' smart constructor.
data GenerateDataKeyPair = GenerateDataKeyPair'
  { keyId :: Types.KeyId
    -- ^ Specifies the symmetric CMK that encrypts the private key in the data key pair. You cannot specify an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
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
  , keyPairSpec :: Types.DataKeyPairSpec
    -- ^ Determines the type of data key pair that is generated. 
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
  , encryptionContext :: Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue)
    -- ^ Specifies the encryption context that will be used when encrypting the private key in the data key pair.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
  , grantTokens :: Core.Maybe [Types.GrantTokenType]
    -- ^ A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataKeyPair' value with any optional fields omitted.
mkGenerateDataKeyPair
    :: Types.KeyId -- ^ 'keyId'
    -> Types.DataKeyPairSpec -- ^ 'keyPairSpec'
    -> GenerateDataKeyPair
mkGenerateDataKeyPair keyId keyPairSpec
  = GenerateDataKeyPair'{keyId, keyPairSpec,
                         encryptionContext = Core.Nothing, grantTokens = Core.Nothing}

-- | Specifies the symmetric CMK that encrypts the private key in the data key pair. You cannot specify an asymmetric CMK or a CMK in a custom key store. To get the type and origin of your CMK, use the 'DescribeKey' operation.
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
gdkpKeyId :: Lens.Lens' GenerateDataKeyPair Types.KeyId
gdkpKeyId = Lens.field @"keyId"
{-# INLINEABLE gdkpKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Determines the type of data key pair that is generated. 
--
-- The AWS KMS rule that restricts the use of asymmetric RSA CMKs to encrypt and decrypt or to sign and verify (but not both), and the rule that permits you to use ECC CMKs only to sign and verify, are not effective outside of AWS KMS.
--
-- /Note:/ Consider using 'keyPairSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpKeyPairSpec :: Lens.Lens' GenerateDataKeyPair Types.DataKeyPairSpec
gdkpKeyPairSpec = Lens.field @"keyPairSpec"
{-# INLINEABLE gdkpKeyPairSpec #-}
{-# DEPRECATED keyPairSpec "Use generic-lens or generic-optics with 'keyPairSpec' instead"  #-}

-- | Specifies the encryption context that will be used when encrypting the private key in the data key pair.
--
-- An /encryption context/ is a collection of non-secret key-value pairs that represents additional authenticated data. When you use an encryption context to encrypt data, you must specify the same (an exact case-sensitive match) encryption context to decrypt the data. An encryption context is optional when encrypting with a symmetric CMK, but it is highly recommended.
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#encrypt_context Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'encryptionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpEncryptionContext :: Lens.Lens' GenerateDataKeyPair (Core.Maybe (Core.HashMap Types.EncryptionContextKey Types.EncryptionContextValue))
gdkpEncryptionContext = Lens.field @"encryptionContext"
{-# INLINEABLE gdkpEncryptionContext #-}
{-# DEPRECATED encryptionContext "Use generic-lens or generic-optics with 'encryptionContext' instead"  #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkpGrantTokens :: Lens.Lens' GenerateDataKeyPair (Core.Maybe [Types.GrantTokenType])
gdkpGrantTokens = Lens.field @"grantTokens"
{-# INLINEABLE gdkpGrantTokens #-}
{-# DEPRECATED grantTokens "Use generic-lens or generic-optics with 'grantTokens' instead"  #-}

instance Core.ToQuery GenerateDataKeyPair where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GenerateDataKeyPair where
        toHeaders GenerateDataKeyPair{..}
          = Core.pure ("X-Amz-Target", "TrentService.GenerateDataKeyPair")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GenerateDataKeyPair where
        toJSON GenerateDataKeyPair{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  Core.Just ("KeyPairSpec" Core..= keyPairSpec),
                  ("EncryptionContext" Core..=) Core.<$> encryptionContext,
                  ("GrantTokens" Core..=) Core.<$> grantTokens])

instance Core.AWSRequest GenerateDataKeyPair where
        type Rs GenerateDataKeyPair = GenerateDataKeyPairResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GenerateDataKeyPairResponse' Core.<$>
                   (x Core..:? "KeyId") Core.<*> x Core..:? "KeyPairSpec" Core.<*>
                     x Core..:? "PrivateKeyCiphertextBlob"
                     Core.<*> x Core..:? "PrivateKeyPlaintext"
                     Core.<*> x Core..:? "PublicKey"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGenerateDataKeyPairResponse' smart constructor.
data GenerateDataKeyPairResponse = GenerateDataKeyPairResponse'
  { keyId :: Core.Maybe Types.KeyId
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
  , keyPairSpec :: Core.Maybe Types.DataKeyPairSpec
    -- ^ The type of data key pair that was generated.
  , privateKeyCiphertextBlob :: Core.Maybe Core.Base64
    -- ^ The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
  , privateKeyPlaintext :: Core.Maybe (Core.Sensitive Core.Base64)
    -- ^ The plaintext copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
  , publicKey :: Core.Maybe Core.Base64
    -- ^ The public key (in plaintext).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateDataKeyPairResponse' value with any optional fields omitted.
mkGenerateDataKeyPairResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GenerateDataKeyPairResponse
mkGenerateDataKeyPairResponse responseStatus
  = GenerateDataKeyPairResponse'{keyId = Core.Nothing,
                                 keyPairSpec = Core.Nothing,
                                 privateKeyCiphertextBlob = Core.Nothing,
                                 privateKeyPlaintext = Core.Nothing, publicKey = Core.Nothing,
                                 responseStatus}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK that encrypted the private key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprrsKeyId :: Lens.Lens' GenerateDataKeyPairResponse (Core.Maybe Types.KeyId)
gdkprrsKeyId = Lens.field @"keyId"
{-# INLINEABLE gdkprrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The type of data key pair that was generated.
--
-- /Note:/ Consider using 'keyPairSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprrsKeyPairSpec :: Lens.Lens' GenerateDataKeyPairResponse (Core.Maybe Types.DataKeyPairSpec)
gdkprrsKeyPairSpec = Lens.field @"keyPairSpec"
{-# INLINEABLE gdkprrsKeyPairSpec #-}
{-# DEPRECATED keyPairSpec "Use generic-lens or generic-optics with 'keyPairSpec' instead"  #-}

-- | The encrypted copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'privateKeyCiphertextBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprrsPrivateKeyCiphertextBlob :: Lens.Lens' GenerateDataKeyPairResponse (Core.Maybe Core.Base64)
gdkprrsPrivateKeyCiphertextBlob = Lens.field @"privateKeyCiphertextBlob"
{-# INLINEABLE gdkprrsPrivateKeyCiphertextBlob #-}
{-# DEPRECATED privateKeyCiphertextBlob "Use generic-lens or generic-optics with 'privateKeyCiphertextBlob' instead"  #-}

-- | The plaintext copy of the private key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'privateKeyPlaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprrsPrivateKeyPlaintext :: Lens.Lens' GenerateDataKeyPairResponse (Core.Maybe (Core.Sensitive Core.Base64))
gdkprrsPrivateKeyPlaintext = Lens.field @"privateKeyPlaintext"
{-# INLINEABLE gdkprrsPrivateKeyPlaintext #-}
{-# DEPRECATED privateKeyPlaintext "Use generic-lens or generic-optics with 'privateKeyPlaintext' instead"  #-}

-- | The public key (in plaintext).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprrsPublicKey :: Lens.Lens' GenerateDataKeyPairResponse (Core.Maybe Core.Base64)
gdkprrsPublicKey = Lens.field @"publicKey"
{-# INLINEABLE gdkprrsPublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdkprrsResponseStatus :: Lens.Lens' GenerateDataKeyPairResponse Core.Int
gdkprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdkprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
