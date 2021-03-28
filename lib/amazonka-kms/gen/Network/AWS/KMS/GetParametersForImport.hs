{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetParametersForImport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the items you need to import key material into a symmetric, customer managed customer master key (CMK). For more information about importing key material into AWS KMS, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ .
--
-- This operation returns a public key and an import token. Use the public key to encrypt the symmetric key material. Store the import token to send with a subsequent 'ImportKeyMaterial' request.
-- You must specify the key ID of the symmetric CMK into which you will import key material. This CMK's @Origin@ must be @EXTERNAL@ . You must also specify the wrapping algorithm and type of wrapping key (public key) that you will use to encrypt the key material. You cannot perform this operation on an asymmetric CMK or on any CMK in a different AWS account.
-- To import key material, you must use the public key and import token from the same response. These items are valid for 24 hours. The expiration date and time appear in the @GetParametersForImport@ response. You cannot use an expired token in an 'ImportKeyMaterial' request. If your key and token expire, send another @GetParametersForImport@ request.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.GetParametersForImport
    (
    -- * Creating a request
      GetParametersForImport (..)
    , mkGetParametersForImport
    -- ** Request lenses
    , gpfiKeyId
    , gpfiWrappingAlgorithm
    , gpfiWrappingKeySpec

    -- * Destructuring the response
    , GetParametersForImportResponse (..)
    , mkGetParametersForImportResponse
    -- ** Response lenses
    , gpfirrsImportToken
    , gpfirrsKeyId
    , gpfirrsParametersValidTo
    , gpfirrsPublicKey
    , gpfirrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetParametersForImport' smart constructor.
data GetParametersForImport = GetParametersForImport'
  { keyId :: Types.KeyIdType
    -- ^ The identifier of the symmetric CMK into which you will import key material. The @Origin@ of the CMK must be @EXTERNAL@ .
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
  , wrappingAlgorithm :: Types.AlgorithmSpec
    -- ^ The algorithm you will use to encrypt the key material before importing it with 'ImportKeyMaterial' . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material> in the /AWS Key Management Service Developer Guide/ .
  , wrappingKeySpec :: Types.WrappingKeySpec
    -- ^ The type of wrapping key (public key) to return in the response. Only 2048-bit RSA public keys are supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetParametersForImport' value with any optional fields omitted.
mkGetParametersForImport
    :: Types.KeyIdType -- ^ 'keyId'
    -> Types.AlgorithmSpec -- ^ 'wrappingAlgorithm'
    -> Types.WrappingKeySpec -- ^ 'wrappingKeySpec'
    -> GetParametersForImport
mkGetParametersForImport keyId wrappingAlgorithm wrappingKeySpec
  = GetParametersForImport'{keyId, wrappingAlgorithm,
                            wrappingKeySpec}

-- | The identifier of the symmetric CMK into which you will import key material. The @Origin@ of the CMK must be @EXTERNAL@ .
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
gpfiKeyId :: Lens.Lens' GetParametersForImport Types.KeyIdType
gpfiKeyId = Lens.field @"keyId"
{-# INLINEABLE gpfiKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The algorithm you will use to encrypt the key material before importing it with 'ImportKeyMaterial' . For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys-encrypt-key-material.html Encrypt the Key Material> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'wrappingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfiWrappingAlgorithm :: Lens.Lens' GetParametersForImport Types.AlgorithmSpec
gpfiWrappingAlgorithm = Lens.field @"wrappingAlgorithm"
{-# INLINEABLE gpfiWrappingAlgorithm #-}
{-# DEPRECATED wrappingAlgorithm "Use generic-lens or generic-optics with 'wrappingAlgorithm' instead"  #-}

-- | The type of wrapping key (public key) to return in the response. Only 2048-bit RSA public keys are supported.
--
-- /Note:/ Consider using 'wrappingKeySpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfiWrappingKeySpec :: Lens.Lens' GetParametersForImport Types.WrappingKeySpec
gpfiWrappingKeySpec = Lens.field @"wrappingKeySpec"
{-# INLINEABLE gpfiWrappingKeySpec #-}
{-# DEPRECATED wrappingKeySpec "Use generic-lens or generic-optics with 'wrappingKeySpec' instead"  #-}

instance Core.ToQuery GetParametersForImport where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetParametersForImport where
        toHeaders GetParametersForImport{..}
          = Core.pure ("X-Amz-Target", "TrentService.GetParametersForImport")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetParametersForImport where
        toJSON GetParametersForImport{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  Core.Just ("WrappingAlgorithm" Core..= wrappingAlgorithm),
                  Core.Just ("WrappingKeySpec" Core..= wrappingKeySpec)])

instance Core.AWSRequest GetParametersForImport where
        type Rs GetParametersForImport = GetParametersForImportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetParametersForImportResponse' Core.<$>
                   (x Core..:? "ImportToken") Core.<*> x Core..:? "KeyId" Core.<*>
                     x Core..:? "ParametersValidTo"
                     Core.<*> x Core..:? "PublicKey"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetParametersForImportResponse' smart constructor.
data GetParametersForImportResponse = GetParametersForImportResponse'
  { importToken :: Core.Maybe Core.Base64
    -- ^ The import token to send in a subsequent 'ImportKeyMaterial' request.
  , keyId :: Core.Maybe Types.KeyId
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK to use in a subsequent 'ImportKeyMaterial' request. This is the same CMK specified in the @GetParametersForImport@ request.
  , parametersValidTo :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the import token and public key are no longer valid. After this time, you cannot use them to make an 'ImportKeyMaterial' request and you must send another @GetParametersForImport@ request to get new ones.
  , publicKey :: Core.Maybe (Core.Sensitive Core.Base64)
    -- ^ The public key to use to encrypt the key material before importing it with 'ImportKeyMaterial' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetParametersForImportResponse' value with any optional fields omitted.
mkGetParametersForImportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetParametersForImportResponse
mkGetParametersForImportResponse responseStatus
  = GetParametersForImportResponse'{importToken = Core.Nothing,
                                    keyId = Core.Nothing, parametersValidTo = Core.Nothing,
                                    publicKey = Core.Nothing, responseStatus}

-- | The import token to send in a subsequent 'ImportKeyMaterial' request.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'importToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirrsImportToken :: Lens.Lens' GetParametersForImportResponse (Core.Maybe Core.Base64)
gpfirrsImportToken = Lens.field @"importToken"
{-# INLINEABLE gpfirrsImportToken #-}
{-# DEPRECATED importToken "Use generic-lens or generic-optics with 'importToken' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK to use in a subsequent 'ImportKeyMaterial' request. This is the same CMK specified in the @GetParametersForImport@ request.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirrsKeyId :: Lens.Lens' GetParametersForImportResponse (Core.Maybe Types.KeyId)
gpfirrsKeyId = Lens.field @"keyId"
{-# INLINEABLE gpfirrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The time at which the import token and public key are no longer valid. After this time, you cannot use them to make an 'ImportKeyMaterial' request and you must send another @GetParametersForImport@ request to get new ones.
--
-- /Note:/ Consider using 'parametersValidTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirrsParametersValidTo :: Lens.Lens' GetParametersForImportResponse (Core.Maybe Core.NominalDiffTime)
gpfirrsParametersValidTo = Lens.field @"parametersValidTo"
{-# INLINEABLE gpfirrsParametersValidTo #-}
{-# DEPRECATED parametersValidTo "Use generic-lens or generic-optics with 'parametersValidTo' instead"  #-}

-- | The public key to use to encrypt the key material before importing it with 'ImportKeyMaterial' .--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirrsPublicKey :: Lens.Lens' GetParametersForImportResponse (Core.Maybe (Core.Sensitive Core.Base64))
gpfirrsPublicKey = Lens.field @"publicKey"
{-# INLINEABLE gpfirrsPublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpfirrsResponseStatus :: Lens.Lens' GetParametersForImportResponse Core.Int
gpfirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpfirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
