{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Data Catalog resource policy for access control.
module Network.AWS.Glue.PutResourcePolicy
    (
    -- * Creating a request
      PutResourcePolicy (..)
    , mkPutResourcePolicy
    -- ** Request lenses
    , prpPolicyInJson
    , prpEnableHybrid
    , prpPolicyExistsCondition
    , prpPolicyHashCondition
    , prpResourceArn

    -- * Destructuring the response
    , PutResourcePolicyResponse (..)
    , mkPutResourcePolicyResponse
    -- ** Response lenses
    , prprrsPolicyHash
    , prprrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { policyInJson :: Types.PolicyInJson
    -- ^ Contains the policy document to set, in JSON format.
  , enableHybrid :: Core.Maybe Types.EnableHybridValues
    -- ^ Allows you to specify if you want to use both resource-level and account/catalog-level resource policies. A resource-level policy is a policy attached to an individual resource such as a database or a table.
--
-- The default value of @NO@ indicates that resource-level policies cannot co-exist with an account-level policy. A value of @YES@ means the use of both resource-level and account/catalog-level resource policies is allowed.
  , policyExistsCondition :: Core.Maybe Types.ExistCondition
    -- ^ A value of @MUST_EXIST@ is used to update a policy. A value of @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a null value is used, the call will not depend on the existence of a policy.
  , policyHashCondition :: Core.Maybe Types.HashString
    -- ^ The hash value returned when the previous policy was set using @PutResourcePolicy@ . Its purpose is to prevent concurrent modifications of a policy. Do not use this parameter if no previous policy has been set.
  , resourceArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The ARN of the AWS Glue resource for the resource policy to be set. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern> 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourcePolicy' value with any optional fields omitted.
mkPutResourcePolicy
    :: Types.PolicyInJson -- ^ 'policyInJson'
    -> PutResourcePolicy
mkPutResourcePolicy policyInJson
  = PutResourcePolicy'{policyInJson, enableHybrid = Core.Nothing,
                       policyExistsCondition = Core.Nothing,
                       policyHashCondition = Core.Nothing, resourceArn = Core.Nothing}

-- | Contains the policy document to set, in JSON format.
--
-- /Note:/ Consider using 'policyInJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyInJson :: Lens.Lens' PutResourcePolicy Types.PolicyInJson
prpPolicyInJson = Lens.field @"policyInJson"
{-# INLINEABLE prpPolicyInJson #-}
{-# DEPRECATED policyInJson "Use generic-lens or generic-optics with 'policyInJson' instead"  #-}

-- | Allows you to specify if you want to use both resource-level and account/catalog-level resource policies. A resource-level policy is a policy attached to an individual resource such as a database or a table.
--
-- The default value of @NO@ indicates that resource-level policies cannot co-exist with an account-level policy. A value of @YES@ means the use of both resource-level and account/catalog-level resource policies is allowed.
--
-- /Note:/ Consider using 'enableHybrid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpEnableHybrid :: Lens.Lens' PutResourcePolicy (Core.Maybe Types.EnableHybridValues)
prpEnableHybrid = Lens.field @"enableHybrid"
{-# INLINEABLE prpEnableHybrid #-}
{-# DEPRECATED enableHybrid "Use generic-lens or generic-optics with 'enableHybrid' instead"  #-}

-- | A value of @MUST_EXIST@ is used to update a policy. A value of @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a null value is used, the call will not depend on the existence of a policy.
--
-- /Note:/ Consider using 'policyExistsCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyExistsCondition :: Lens.Lens' PutResourcePolicy (Core.Maybe Types.ExistCondition)
prpPolicyExistsCondition = Lens.field @"policyExistsCondition"
{-# INLINEABLE prpPolicyExistsCondition #-}
{-# DEPRECATED policyExistsCondition "Use generic-lens or generic-optics with 'policyExistsCondition' instead"  #-}

-- | The hash value returned when the previous policy was set using @PutResourcePolicy@ . Its purpose is to prevent concurrent modifications of a policy. Do not use this parameter if no previous policy has been set.
--
-- /Note:/ Consider using 'policyHashCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyHashCondition :: Lens.Lens' PutResourcePolicy (Core.Maybe Types.HashString)
prpPolicyHashCondition = Lens.field @"policyHashCondition"
{-# INLINEABLE prpPolicyHashCondition #-}
{-# DEPRECATED policyHashCondition "Use generic-lens or generic-optics with 'policyHashCondition' instead"  #-}

-- | The ARN of the AWS Glue resource for the resource policy to be set. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern> 
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpResourceArn :: Lens.Lens' PutResourcePolicy (Core.Maybe Types.GlueResourceArn)
prpResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE prpResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery PutResourcePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutResourcePolicy where
        toHeaders PutResourcePolicy{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.PutResourcePolicy") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutResourcePolicy where
        toJSON PutResourcePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyInJson" Core..= policyInJson),
                  ("EnableHybrid" Core..=) Core.<$> enableHybrid,
                  ("PolicyExistsCondition" Core..=) Core.<$> policyExistsCondition,
                  ("PolicyHashCondition" Core..=) Core.<$> policyHashCondition,
                  ("ResourceArn" Core..=) Core.<$> resourceArn])

instance Core.AWSRequest PutResourcePolicy where
        type Rs PutResourcePolicy = PutResourcePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutResourcePolicyResponse' Core.<$>
                   (x Core..:? "PolicyHash") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { policyHash :: Core.Maybe Types.HashString
    -- ^ A hash of the policy that has just been set. This must be included in a subsequent call that overwrites or updates this policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourcePolicyResponse' value with any optional fields omitted.
mkPutResourcePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutResourcePolicyResponse
mkPutResourcePolicyResponse responseStatus
  = PutResourcePolicyResponse'{policyHash = Core.Nothing,
                               responseStatus}

-- | A hash of the policy that has just been set. This must be included in a subsequent call that overwrites or updates this policy.
--
-- /Note:/ Consider using 'policyHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsPolicyHash :: Lens.Lens' PutResourcePolicyResponse (Core.Maybe Types.HashString)
prprrsPolicyHash = Lens.field @"policyHash"
{-# INLINEABLE prprrsPolicyHash #-}
{-# DEPRECATED policyHash "Use generic-lens or generic-optics with 'policyHash' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsResponseStatus :: Lens.Lens' PutResourcePolicyResponse Core.Int
prprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
