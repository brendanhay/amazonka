{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetKeyPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key policy attached to the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.
module Network.AWS.KMS.GetKeyPolicy
    (
    -- * Creating a request
      GetKeyPolicy (..)
    , mkGetKeyPolicy
    -- ** Request lenses
    , gkpKeyId
    , gkpPolicyName

    -- * Destructuring the response
    , GetKeyPolicyResponse (..)
    , mkGetKeyPolicyResponse
    -- ** Response lenses
    , gkprrsPolicy
    , gkprrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetKeyPolicy' smart constructor.
data GetKeyPolicy = GetKeyPolicy'
  { keyId :: Types.KeyIdType
    -- ^ A unique identifier for the customer master key (CMK).
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
  , policyName :: Types.PolicyNameType
    -- ^ Specifies the name of the key policy. The only valid name is @default@ . To get the names of key policies, use 'ListKeyPolicies' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyPolicy' value with any optional fields omitted.
mkGetKeyPolicy
    :: Types.KeyIdType -- ^ 'keyId'
    -> Types.PolicyNameType -- ^ 'policyName'
    -> GetKeyPolicy
mkGetKeyPolicy keyId policyName = GetKeyPolicy'{keyId, policyName}

-- | A unique identifier for the customer master key (CMK).
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
gkpKeyId :: Lens.Lens' GetKeyPolicy Types.KeyIdType
gkpKeyId = Lens.field @"keyId"
{-# INLINEABLE gkpKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Specifies the name of the key policy. The only valid name is @default@ . To get the names of key policies, use 'ListKeyPolicies' .
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpPolicyName :: Lens.Lens' GetKeyPolicy Types.PolicyNameType
gkpPolicyName = Lens.field @"policyName"
{-# INLINEABLE gkpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery GetKeyPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetKeyPolicy where
        toHeaders GetKeyPolicy{..}
          = Core.pure ("X-Amz-Target", "TrentService.GetKeyPolicy") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetKeyPolicy where
        toJSON GetKeyPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  Core.Just ("PolicyName" Core..= policyName)])

instance Core.AWSRequest GetKeyPolicy where
        type Rs GetKeyPolicy = GetKeyPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetKeyPolicyResponse' Core.<$>
                   (x Core..:? "Policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetKeyPolicyResponse' smart constructor.
data GetKeyPolicyResponse = GetKeyPolicyResponse'
  { policy :: Core.Maybe Types.PolicyType
    -- ^ A key policy document in JSON format.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetKeyPolicyResponse' value with any optional fields omitted.
mkGetKeyPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetKeyPolicyResponse
mkGetKeyPolicyResponse responseStatus
  = GetKeyPolicyResponse'{policy = Core.Nothing, responseStatus}

-- | A key policy document in JSON format.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprrsPolicy :: Lens.Lens' GetKeyPolicyResponse (Core.Maybe Types.PolicyType)
gkprrsPolicy = Lens.field @"policy"
{-# INLINEABLE gkprrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprrsResponseStatus :: Lens.Lens' GetKeyPolicyResponse Core.Int
gkprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gkprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
