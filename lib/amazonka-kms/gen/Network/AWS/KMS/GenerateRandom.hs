{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateRandom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a random byte string that is cryptographically secure.
--
-- By default, the random byte string is generated in AWS KMS. To generate the byte string in the AWS CloudHSM cluster that is associated with a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , specify the custom key store ID.
-- For more information about entropy and random number generation, see the <https://d0.awsstatic.com/whitepapers/KMS-Cryptographic-Details.pdf AWS Key Management Service Cryptographic Details> whitepaper.
module Network.AWS.KMS.GenerateRandom
    (
    -- * Creating a request
      GenerateRandom (..)
    , mkGenerateRandom
    -- ** Request lenses
    , grCustomKeyStoreId
    , grNumberOfBytes

    -- * Destructuring the response
    , GenerateRandomResponse (..)
    , mkGenerateRandomResponse
    -- ** Response lenses
    , grrrsPlaintext
    , grrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGenerateRandom' smart constructor.
data GenerateRandom = GenerateRandom'
  { customKeyStoreId :: Core.Maybe Types.CustomKeyStoreIdType
    -- ^ Generates the random byte string in the AWS CloudHSM cluster that is associated with the specified <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
  , numberOfBytes :: Core.Maybe Core.Natural
    -- ^ The length of the byte string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateRandom' value with any optional fields omitted.
mkGenerateRandom
    :: GenerateRandom
mkGenerateRandom
  = GenerateRandom'{customKeyStoreId = Core.Nothing,
                    numberOfBytes = Core.Nothing}

-- | Generates the random byte string in the AWS CloudHSM cluster that is associated with the specified <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . To find the ID of a custom key store, use the 'DescribeCustomKeyStores' operation.
--
-- /Note:/ Consider using 'customKeyStoreId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grCustomKeyStoreId :: Lens.Lens' GenerateRandom (Core.Maybe Types.CustomKeyStoreIdType)
grCustomKeyStoreId = Lens.field @"customKeyStoreId"
{-# INLINEABLE grCustomKeyStoreId #-}
{-# DEPRECATED customKeyStoreId "Use generic-lens or generic-optics with 'customKeyStoreId' instead"  #-}

-- | The length of the byte string.
--
-- /Note:/ Consider using 'numberOfBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grNumberOfBytes :: Lens.Lens' GenerateRandom (Core.Maybe Core.Natural)
grNumberOfBytes = Lens.field @"numberOfBytes"
{-# INLINEABLE grNumberOfBytes #-}
{-# DEPRECATED numberOfBytes "Use generic-lens or generic-optics with 'numberOfBytes' instead"  #-}

instance Core.ToQuery GenerateRandom where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GenerateRandom where
        toHeaders GenerateRandom{..}
          = Core.pure ("X-Amz-Target", "TrentService.GenerateRandom") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GenerateRandom where
        toJSON GenerateRandom{..}
          = Core.object
              (Core.catMaybes
                 [("CustomKeyStoreId" Core..=) Core.<$> customKeyStoreId,
                  ("NumberOfBytes" Core..=) Core.<$> numberOfBytes])

instance Core.AWSRequest GenerateRandom where
        type Rs GenerateRandom = GenerateRandomResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GenerateRandomResponse' Core.<$>
                   (x Core..:? "Plaintext") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGenerateRandomResponse' smart constructor.
data GenerateRandomResponse = GenerateRandomResponse'
  { plaintext :: Core.Maybe (Core.Sensitive Core.Base64)
    -- ^ The random byte string. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateRandomResponse' value with any optional fields omitted.
mkGenerateRandomResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GenerateRandomResponse
mkGenerateRandomResponse responseStatus
  = GenerateRandomResponse'{plaintext = Core.Nothing, responseStatus}

-- | The random byte string. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not Base64-encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'plaintext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsPlaintext :: Lens.Lens' GenerateRandomResponse (Core.Maybe (Core.Sensitive Core.Base64))
grrrsPlaintext = Lens.field @"plaintext"
{-# INLINEABLE grrrsPlaintext #-}
{-# DEPRECATED plaintext "Use generic-lens or generic-optics with 'plaintext' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GenerateRandomResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
