{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Sign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a <https://en.wikipedia.org/wiki/Digital_signature digital signature> for a message or message digest by using the private key in an asymmetric CMK. To verify the signature, use the 'Verify' operation, or use the public key in the same asymmetric CMK outside of AWS KMS. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric CMKs> in the /AWS Key Management Service Developer Guide/ .
--
-- Digital signatures are generated and verified by using asymmetric key pair, such as an RSA or ECC pair that is represented by an asymmetric customer master key (CMK). The key owner (or an authorized user) uses their private key to sign a message. Anyone with the public key can verify that the message was signed with that particular private key and that the message hasn't changed since it was signed. 
-- To use the @Sign@ operation, provide the following information:
--
--     * Use the @KeyId@ parameter to identify an asymmetric CMK with a @KeyUsage@ value of @SIGN_VERIFY@ . To get the @KeyUsage@ value of a CMK, use the 'DescribeKey' operation. The caller must have @kms:Sign@ permission on the CMK.
--
--
--     * Use the @Message@ parameter to specify the message or message digest to sign. You can submit messages of up to 4096 bytes. To sign a larger message, generate a hash digest of the message, and then provide the hash digest in the @Message@ parameter. To indicate whether the message is a full message or a digest, use the @MessageType@ parameter.
--
--
--     * Choose a signing algorithm that is compatible with the CMK. 
--
--
-- /Important:/ When signing a message, be sure to record the CMK and the signing algorithm. This information is required to verify the signature.
-- To verify the signature that this operation generates, use the 'Verify' operation. Or use the 'GetPublicKey' operation to download the public key and then use the public key to verify the signature outside of AWS KMS. 
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.Sign
    (
    -- * Creating a request
      Sign (..)
    , mkSign
    -- ** Request lenses
    , sKeyId
    , sMessage
    , sSigningAlgorithm
    , sGrantTokens
    , sMessageType

    -- * Destructuring the response
    , SignResponse (..)
    , mkSignResponse
    -- ** Response lenses
    , srrsKeyId
    , srrsSignature
    , srrsSigningAlgorithm
    , srrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSign' smart constructor.
data Sign = Sign'
  { keyId :: Types.KeyIdType
    -- ^ Identifies an asymmetric CMK. AWS KMS uses the private key in the asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must be @SIGN_VERIFY@ . To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation.
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
  , message :: Core.Sensitive Core.Base64
    -- ^ Specifies the message or message digest to sign. Messages can be 0-4096 bytes. To sign a larger message, provide the message digest.
--
-- If you provide a message, AWS KMS generates a hash digest of the message and then signs it.
  , signingAlgorithm :: Types.SigningAlgorithmSpec
    -- ^ Specifies the signing algorithm to use when signing the message. 
--
-- Choose an algorithm that is compatible with the type and size of the specified asymmetric CMK.
  , grantTokens :: Core.Maybe [Types.GrantTokenType]
    -- ^ A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
  , messageType :: Core.Maybe Types.MessageType
    -- ^ Tells AWS KMS whether the value of the @Message@ parameter is a message or message digest. The default value, RAW, indicates a message. To indicate a message digest, enter @DIGEST@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Sign' value with any optional fields omitted.
mkSign
    :: Types.KeyIdType -- ^ 'keyId'
    -> Core.Sensitive Core.Base64 -- ^ 'message'
    -> Types.SigningAlgorithmSpec -- ^ 'signingAlgorithm'
    -> Sign
mkSign keyId message signingAlgorithm
  = Sign'{keyId, message, signingAlgorithm,
          grantTokens = Core.Nothing, messageType = Core.Nothing}

-- | Identifies an asymmetric CMK. AWS KMS uses the private key in the asymmetric CMK to sign the message. The @KeyUsage@ type of the CMK must be @SIGN_VERIFY@ . To find the @KeyUsage@ of a CMK, use the 'DescribeKey' operation.
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
sKeyId :: Lens.Lens' Sign Types.KeyIdType
sKeyId = Lens.field @"keyId"
{-# INLINEABLE sKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Specifies the message or message digest to sign. Messages can be 0-4096 bytes. To sign a larger message, provide the message digest.
--
-- If you provide a message, AWS KMS generates a hash digest of the message and then signs it.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessage :: Lens.Lens' Sign (Core.Sensitive Core.Base64)
sMessage = Lens.field @"message"
{-# INLINEABLE sMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | Specifies the signing algorithm to use when signing the message. 
--
-- Choose an algorithm that is compatible with the type and size of the specified asymmetric CMK.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSigningAlgorithm :: Lens.Lens' Sign Types.SigningAlgorithmSpec
sSigningAlgorithm = Lens.field @"signingAlgorithm"
{-# INLINEABLE sSigningAlgorithm #-}
{-# DEPRECATED signingAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead"  #-}

-- | A list of grant tokens.
--
-- For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'grantTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sGrantTokens :: Lens.Lens' Sign (Core.Maybe [Types.GrantTokenType])
sGrantTokens = Lens.field @"grantTokens"
{-# INLINEABLE sGrantTokens #-}
{-# DEPRECATED grantTokens "Use generic-lens or generic-optics with 'grantTokens' instead"  #-}

-- | Tells AWS KMS whether the value of the @Message@ parameter is a message or message digest. The default value, RAW, indicates a message. To indicate a message digest, enter @DIGEST@ .
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessageType :: Lens.Lens' Sign (Core.Maybe Types.MessageType)
sMessageType = Lens.field @"messageType"
{-# INLINEABLE sMessageType #-}
{-# DEPRECATED messageType "Use generic-lens or generic-optics with 'messageType' instead"  #-}

instance Core.ToQuery Sign where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders Sign where
        toHeaders Sign{..}
          = Core.pure ("X-Amz-Target", "TrentService.Sign") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON Sign where
        toJSON Sign{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  Core.Just ("Message" Core..= message),
                  Core.Just ("SigningAlgorithm" Core..= signingAlgorithm),
                  ("GrantTokens" Core..=) Core.<$> grantTokens,
                  ("MessageType" Core..=) Core.<$> messageType])

instance Core.AWSRequest Sign where
        type Rs Sign = SignResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SignResponse' Core.<$>
                   (x Core..:? "KeyId") Core.<*> x Core..:? "Signature" Core.<*>
                     x Core..:? "SigningAlgorithm"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSignResponse' smart constructor.
data SignResponse = SignResponse'
  { keyId :: Core.Maybe Types.KeyIdType
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK that was used to sign the message.
  , signature :: Core.Maybe Core.Base64
    -- ^ The cryptographic signature that was generated for the message. 
--
--
--     * When used with the supported RSA signing algorithms, the encoding of this value is defined by <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017> .
--
--
--     * When used with the @ECDSA_SHA_256@ , @ECDSA_SHA_384@ , or @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded object as defined by ANS X9.62–2005 and <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3> . This is the most commonly used signature format and is appropriate for most uses. 
--
--
-- When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
  , signingAlgorithm :: Core.Maybe Types.SigningAlgorithmSpec
    -- ^ The signing algorithm that was used to sign the message.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignResponse' value with any optional fields omitted.
mkSignResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SignResponse
mkSignResponse responseStatus
  = SignResponse'{keyId = Core.Nothing, signature = Core.Nothing,
                  signingAlgorithm = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the asymmetric CMK that was used to sign the message.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsKeyId :: Lens.Lens' SignResponse (Core.Maybe Types.KeyIdType)
srrsKeyId = Lens.field @"keyId"
{-# INLINEABLE srrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The cryptographic signature that was generated for the message. 
--
--
--     * When used with the supported RSA signing algorithms, the encoding of this value is defined by <https://tools.ietf.org/html/rfc8017 PKCS #1 in RFC 8017> .
--
--
--     * When used with the @ECDSA_SHA_256@ , @ECDSA_SHA_384@ , or @ECDSA_SHA_512@ signing algorithms, this value is a DER-encoded object as defined by ANS X9.62–2005 and <https://tools.ietf.org/html/rfc3279#section-2.2.3 RFC 3279 Section 2.2.3> . This is the most commonly used signature format and is appropriate for most uses. 
--
--
-- When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsSignature :: Lens.Lens' SignResponse (Core.Maybe Core.Base64)
srrsSignature = Lens.field @"signature"
{-# INLINEABLE srrsSignature #-}
{-# DEPRECATED signature "Use generic-lens or generic-optics with 'signature' instead"  #-}

-- | The signing algorithm that was used to sign the message.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsSigningAlgorithm :: Lens.Lens' SignResponse (Core.Maybe Types.SigningAlgorithmSpec)
srrsSigningAlgorithm = Lens.field @"signingAlgorithm"
{-# INLINEABLE srrsSigningAlgorithm #-}
{-# DEPRECATED signingAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' SignResponse Core.Int
srrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
