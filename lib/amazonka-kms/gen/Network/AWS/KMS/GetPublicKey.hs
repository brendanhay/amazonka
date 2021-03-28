{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the public key of an asymmetric CMK. Unlike the private key of a asymmetric CMK, which never leaves AWS KMS unencrypted, callers with @kms:GetPublicKey@ permission can download the public key of an asymmetric CMK. You can share the public key to allow others to encrypt messages and verify signatures outside of AWS KMS. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
--
-- You do not need to download the public key. Instead, you can use the public key within AWS KMS by calling the 'Encrypt' , 'ReEncrypt' , or 'Verify' operations with the identifier of an asymmetric CMK. When you use the public key within AWS KMS, you benefit from the authentication, authorization, and logging that are part of every AWS KMS operation. You also reduce of risk of encrypting data that cannot be decrypted. These features are not effective outside of AWS KMS. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/download-public-key.html#download-public-key-considerations Special Considerations for Downloading Public Keys> .
-- To help you use the public key safely outside of AWS KMS, @GetPublicKey@ returns important information about the public key in the response, including:
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-CustomerMasterKeySpec CustomerMasterKeySpec> : The type of key material in the public key, such as @RSA_4096@ or @ECC_NIST_P521@ .
--
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-KeyUsage KeyUsage> : Whether the key is used for encryption or signing.
--
--
--     * <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-EncryptionAlgorithms EncryptionAlgorithms> or <https://docs.aws.amazon.com/kms/latest/APIReference/API_GetPublicKey.html#KMS-GetPublicKey-response-SigningAlgorithms SigningAlgorithms> : A list of the encryption algorithms or the signing algorithms for the key.
--
--
-- Although AWS KMS cannot enforce these restrictions on external operations, it is crucial that you use this information to prevent the public key from being used improperly. For example, you can prevent a public signing key from being used encrypt data, or prevent a public key from being used with an encryption algorithm that is not supported by AWS KMS. You can also avoid errors, such as using the wrong signing algorithm in a verification operation.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GetPublicKey
    (
    -- * Creating a request
      GetPublicKey (..)
    , mkGetPublicKey
    -- ** Request lenses
    , gpkKeyId
    , gpkGrantTokens

    -- * Destructuring the response
    , GetPublicKeyResponse (..)
    , mkGetPublicKeyResponse
    -- ** Response lenses
    , gpkrrsCustomerMasterKeySpec
    , gpkrrsEncryptionAlgorithms
    , gpkrrsKeyId
    , gpkrrsKeyUsage
    , gpkrrsPublicKey
    , gpkrrsSigningAlgorithms
    , gpkrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPublicKey' smart constructor.
data GetPublicKey = GetPublicKey'
  { keyId :: Types.KeyIdType
    -- ^ Identifies the asymmetric CMK that includes the public key.
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
  , grantTokens :: Core.Maybe [Types.GrantTokenType]
    -- ^ A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicKey' value with any optional fields omitted.
mkGetPublicKey
    :: Types.KeyIdType -- ^ 'keyId'
    -> GetPublicKey
mkGetPublicKey keyId
  = GetPublicKey'{keyId, grantTokens = Core.Nothing}

-- | Identifies the asymmetric CMK that includes the public key.
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
gpkKeyId :: Lens.Lens' GetPublicKey Types.KeyIdType
gpkKeyId = Lens.field @"keyId"
{-# INLINEABLE gpkKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkGrantTokens :: Lens.Lens' GetPublicKey (Core.Maybe [Types.GrantTokenType])
gpkGrantTokens = Lens.field @"grantTokens"
{-# INLINEABLE gpkGrantTokens #-}
{-# DEPRECATED grantTokens "Use generic-lens or generic-optics with 'grantTokens' instead"  #-}

instance Core.ToQuery GetPublicKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPublicKey where
        toHeaders GetPublicKey{..}
          = Core.pure ("X-Amz-Target", "TrentService.GetPublicKey") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPublicKey where
        toJSON GetPublicKey{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  ("GrantTokens" Core..=) Core.<$> grantTokens])

instance Core.AWSRequest GetPublicKey where
        type Rs GetPublicKey = GetPublicKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPublicKeyResponse' Core.<$>
                   (x Core..:? "CustomerMasterKeySpec") Core.<*>
                     x Core..:? "EncryptionAlgorithms"
                     Core.<*> x Core..:? "KeyId"
                     Core.<*> x Core..:? "KeyUsage"
                     Core.<*> x Core..:? "PublicKey"
                     Core.<*> x Core..:? "SigningAlgorithms"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { customerMasterKeySpec :: Core.Maybe Types.CustomerMasterKeySpec
    -- ^ The type of the of the public key that was downloaded.
  , encryptionAlgorithms :: Core.Maybe [Types.EncryptionAlgorithmSpec]
    -- ^ The encryption algorithms that AWS KMS supports for this key. 
--
-- This information is critical. If a public key encrypts data outside of AWS KMS by using an unsupported encryption algorithm, the ciphertext cannot be decrypted. 
-- This field appears in the response only when the @KeyUsage@ of the public key is @ENCRYPT_DECRYPT@ .
  , keyId :: Core.Maybe Types.KeyId
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK from which the public key was downloaded.
  , keyUsage :: Core.Maybe Types.KeyUsageType
    -- ^ The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ . 
--
-- This information is critical. If a public key with @SIGN_VERIFY@ key usage encrypts data outside of AWS KMS, the ciphertext cannot be decrypted. 
  , publicKey :: Core.Maybe Core.Base64
    -- ^ The exported public key. 
--
-- The value is a DER-encoded X.509 public key, also known as @SubjectPublicKeyInfo@ (SPKI), as defined in <https://tools.ietf.org/html/rfc5280 RFC 5280> . When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
--
  , signingAlgorithms :: Core.Maybe [Types.SigningAlgorithmSpec]
    -- ^ The signing algorithms that AWS KMS supports for this key.
--
-- This field appears in the response only when the @KeyUsage@ of the public key is @SIGN_VERIFY@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicKeyResponse' value with any optional fields omitted.
mkGetPublicKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPublicKeyResponse
mkGetPublicKeyResponse responseStatus
  = GetPublicKeyResponse'{customerMasterKeySpec = Core.Nothing,
                          encryptionAlgorithms = Core.Nothing, keyId = Core.Nothing,
                          keyUsage = Core.Nothing, publicKey = Core.Nothing,
                          signingAlgorithms = Core.Nothing, responseStatus}

-- | The type of the of the public key that was downloaded.
--
-- /Note:/ Consider using 'customerMasterKeySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsCustomerMasterKeySpec :: Lens.Lens' GetPublicKeyResponse (Core.Maybe Types.CustomerMasterKeySpec)
gpkrrsCustomerMasterKeySpec = Lens.field @"customerMasterKeySpec"
{-# INLINEABLE gpkrrsCustomerMasterKeySpec #-}
{-# DEPRECATED customerMasterKeySpec "Use generic-lens or generic-optics with 'customerMasterKeySpec' instead"  #-}

-- | The encryption algorithms that AWS KMS supports for this key. 
--
-- This information is critical. If a public key encrypts data outside of AWS KMS by using an unsupported encryption algorithm, the ciphertext cannot be decrypted. 
-- This field appears in the response only when the @KeyUsage@ of the public key is @ENCRYPT_DECRYPT@ .
--
-- /Note:/ Consider using 'encryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsEncryptionAlgorithms :: Lens.Lens' GetPublicKeyResponse (Core.Maybe [Types.EncryptionAlgorithmSpec])
gpkrrsEncryptionAlgorithms = Lens.field @"encryptionAlgorithms"
{-# INLINEABLE gpkrrsEncryptionAlgorithms #-}
{-# DEPRECATED encryptionAlgorithms "Use generic-lens or generic-optics with 'encryptionAlgorithms' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK from which the public key was downloaded.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsKeyId :: Lens.Lens' GetPublicKeyResponse (Core.Maybe Types.KeyId)
gpkrrsKeyId = Lens.field @"keyId"
{-# INLINEABLE gpkrrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The permitted use of the public key. Valid values are @ENCRYPT_DECRYPT@ or @SIGN_VERIFY@ . 
--
-- This information is critical. If a public key with @SIGN_VERIFY@ key usage encrypts data outside of AWS KMS, the ciphertext cannot be decrypted. 
--
-- /Note:/ Consider using 'keyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsKeyUsage :: Lens.Lens' GetPublicKeyResponse (Core.Maybe Types.KeyUsageType)
gpkrrsKeyUsage = Lens.field @"keyUsage"
{-# INLINEABLE gpkrrsKeyUsage #-}
{-# DEPRECATED keyUsage "Use generic-lens or generic-optics with 'keyUsage' instead"  #-}

-- | The exported public key. 
--
-- The value is a DER-encoded X.509 public key, also known as @SubjectPublicKeyInfo@ (SPKI), as defined in <https://tools.ietf.org/html/rfc5280 RFC 5280> . When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
----
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsPublicKey :: Lens.Lens' GetPublicKeyResponse (Core.Maybe Core.Base64)
gpkrrsPublicKey = Lens.field @"publicKey"
{-# INLINEABLE gpkrrsPublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

-- | The signing algorithms that AWS KMS supports for this key.
--
-- This field appears in the response only when the @KeyUsage@ of the public key is @SIGN_VERIFY@ .
--
-- /Note:/ Consider using 'signingAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsSigningAlgorithms :: Lens.Lens' GetPublicKeyResponse (Core.Maybe [Types.SigningAlgorithmSpec])
gpkrrsSigningAlgorithms = Lens.field @"signingAlgorithms"
{-# INLINEABLE gpkrrsSigningAlgorithms #-}
{-# DEPRECATED signingAlgorithms "Use generic-lens or generic-optics with 'signingAlgorithms' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkrrsResponseStatus :: Lens.Lens' GetPublicKeyResponse Core.Int
gpkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
