{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Boolean value that indicates whether <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material> is enabled for the specified customer master key (CMK).
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . The key rotation status for these CMKs is always @false@ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
--
--     * Disabled: The key rotation status does not change when you disable a CMK. However, while the CMK is disabled, AWS KMS does not rotate the backing key.
--
--
--     * Pending deletion: While a CMK is pending deletion, its key rotation status is @false@ and AWS KMS does not rotate the backing key. If you cancel the deletion, the original key rotation status is restored.
--
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the @KeyId@ parameter.
module Network.AWS.KMS.GetKeyRotationStatus
    (
    -- * Creating a request
      GetKeyRotationStatus (..)
    , mkGetKeyRotationStatus
    -- ** Request lenses
    , gkrsKeyId

    -- * Destructuring the response
    , GetKeyRotationStatusResponse (..)
    , mkGetKeyRotationStatusResponse
    -- ** Response lenses
    , gkrsrrsKeyRotationEnabled
    , gkrsrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetKeyRotationStatus' smart constructor.
newtype GetKeyRotationStatus = GetKeyRotationStatus'
  { keyId :: Types.KeyId
    -- ^ A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyRotationStatus' value with any optional fields omitted.
mkGetKeyRotationStatus
    :: Types.KeyId -- ^ 'keyId'
    -> GetKeyRotationStatus
mkGetKeyRotationStatus keyId = GetKeyRotationStatus'{keyId}

-- | A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
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
gkrsKeyId :: Lens.Lens' GetKeyRotationStatus Types.KeyId
gkrsKeyId = Lens.field @"keyId"
{-# INLINEABLE gkrsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToQuery GetKeyRotationStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetKeyRotationStatus where
        toHeaders GetKeyRotationStatus{..}
          = Core.pure ("X-Amz-Target", "TrentService.GetKeyRotationStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetKeyRotationStatus where
        toJSON GetKeyRotationStatus{..}
          = Core.object (Core.catMaybes [Core.Just ("KeyId" Core..= keyId)])

instance Core.AWSRequest GetKeyRotationStatus where
        type Rs GetKeyRotationStatus = GetKeyRotationStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetKeyRotationStatusResponse' Core.<$>
                   (x Core..:? "KeyRotationEnabled") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetKeyRotationStatusResponse' smart constructor.
data GetKeyRotationStatusResponse = GetKeyRotationStatusResponse'
  { keyRotationEnabled :: Core.Maybe Core.Bool
    -- ^ A Boolean value that specifies whether key rotation is enabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyRotationStatusResponse' value with any optional fields omitted.
mkGetKeyRotationStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetKeyRotationStatusResponse
mkGetKeyRotationStatusResponse responseStatus
  = GetKeyRotationStatusResponse'{keyRotationEnabled = Core.Nothing,
                                  responseStatus}

-- | A Boolean value that specifies whether key rotation is enabled.
--
-- /Note:/ Consider using 'keyRotationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkrsrrsKeyRotationEnabled :: Lens.Lens' GetKeyRotationStatusResponse (Core.Maybe Core.Bool)
gkrsrrsKeyRotationEnabled = Lens.field @"keyRotationEnabled"
{-# INLINEABLE gkrsrrsKeyRotationEnabled #-}
{-# DEPRECATED keyRotationEnabled "Use generic-lens or generic-optics with 'keyRotationEnabled' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkrsrrsResponseStatus :: Lens.Lens' GetKeyRotationStatusResponse Core.Int
gkrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gkrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
