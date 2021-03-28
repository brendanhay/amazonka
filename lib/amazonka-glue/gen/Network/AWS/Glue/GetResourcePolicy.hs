{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified resource policy.
module Network.AWS.Glue.GetResourcePolicy
    (
    -- * Creating a request
      GetResourcePolicy (..)
    , mkGetResourcePolicy
    -- ** Request lenses
    , grpResourceArn

    -- * Destructuring the response
    , GetResourcePolicyResponse (..)
    , mkGetResourcePolicyResponse
    -- ** Response lenses
    , grprfrsCreateTime
    , grprfrsPolicyHash
    , grprfrsPolicyInJson
    , grprfrsUpdateTime
    , grprfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { resourceArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The ARN of the AWS Glue resource for the resource policy to be retrieved. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern> 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicy' value with any optional fields omitted.
mkGetResourcePolicy
    :: GetResourcePolicy
mkGetResourcePolicy
  = GetResourcePolicy'{resourceArn = Core.Nothing}

-- | The ARN of the AWS Glue resource for the resource policy to be retrieved. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern> 
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpResourceArn :: Lens.Lens' GetResourcePolicy (Core.Maybe Types.GlueResourceArn)
grpResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE grpResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery GetResourcePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetResourcePolicy where
        toHeaders GetResourcePolicy{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetResourcePolicy") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetResourcePolicy where
        toJSON GetResourcePolicy{..}
          = Core.object
              (Core.catMaybes [("ResourceArn" Core..=) Core.<$> resourceArn])

instance Core.AWSRequest GetResourcePolicy where
        type Rs GetResourcePolicy = GetResourcePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetResourcePolicyResponse' Core.<$>
                   (x Core..:? "CreateTime") Core.<*> x Core..:? "PolicyHash" Core.<*>
                     x Core..:? "PolicyInJson"
                     Core.<*> x Core..:? "UpdateTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { createTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time at which the policy was created.
  , policyHash :: Core.Maybe Types.HashString
    -- ^ Contains the hash value associated with this policy.
  , policyInJson :: Core.Maybe Types.PolicyInJson
    -- ^ Contains the requested policy document, in JSON format.
  , updateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time at which the policy was last updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetResourcePolicyResponse' value with any optional fields omitted.
mkGetResourcePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetResourcePolicyResponse
mkGetResourcePolicyResponse responseStatus
  = GetResourcePolicyResponse'{createTime = Core.Nothing,
                               policyHash = Core.Nothing, policyInJson = Core.Nothing,
                               updateTime = Core.Nothing, responseStatus}

-- | The date and time at which the policy was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsCreateTime :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Core.NominalDiffTime)
grprfrsCreateTime = Lens.field @"createTime"
{-# INLINEABLE grprfrsCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | Contains the hash value associated with this policy.
--
-- /Note:/ Consider using 'policyHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsPolicyHash :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.HashString)
grprfrsPolicyHash = Lens.field @"policyHash"
{-# INLINEABLE grprfrsPolicyHash #-}
{-# DEPRECATED policyHash "Use generic-lens or generic-optics with 'policyHash' instead"  #-}

-- | Contains the requested policy document, in JSON format.
--
-- /Note:/ Consider using 'policyInJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsPolicyInJson :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.PolicyInJson)
grprfrsPolicyInJson = Lens.field @"policyInJson"
{-# INLINEABLE grprfrsPolicyInJson #-}
{-# DEPRECATED policyInJson "Use generic-lens or generic-optics with 'policyInJson' instead"  #-}

-- | The date and time at which the policy was last updated.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsUpdateTime :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Core.NominalDiffTime)
grprfrsUpdateTime = Lens.field @"updateTime"
{-# INLINEABLE grprfrsUpdateTime #-}
{-# DEPRECATED updateTime "Use generic-lens or generic-optics with 'updateTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsResponseStatus :: Lens.Lens' GetResourcePolicyResponse Core.Int
grprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
