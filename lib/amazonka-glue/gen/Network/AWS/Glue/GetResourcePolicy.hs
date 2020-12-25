{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetResourcePolicy (..),
    mkGetResourcePolicy,

    -- ** Request lenses
    grpResourceArn,

    -- * Destructuring the response
    GetResourcePolicyResponse (..),
    mkGetResourcePolicyResponse,

    -- ** Response lenses
    grprfrsCreateTime,
    grprfrsPolicyHash,
    grprfrsPolicyInJson,
    grprfrsUpdateTime,
    grprfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { -- | The ARN of the AWS Glue resource for the resource policy to be retrieved. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
    resourceArn :: Core.Maybe Types.GlueResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicy' value with any optional fields omitted.
mkGetResourcePolicy ::
  GetResourcePolicy
mkGetResourcePolicy =
  GetResourcePolicy' {resourceArn = Core.Nothing}

-- | The ARN of the AWS Glue resource for the resource policy to be retrieved. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpResourceArn :: Lens.Lens' GetResourcePolicy (Core.Maybe Types.GlueResourceArn)
grpResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED grpResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON GetResourcePolicy where
  toJSON GetResourcePolicy {..} =
    Core.object
      (Core.catMaybes [("ResourceArn" Core..=) Core.<$> resourceArn])

instance Core.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetResourcePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Core.<$> (x Core..:? "CreateTime")
            Core.<*> (x Core..:? "PolicyHash")
            Core.<*> (x Core..:? "PolicyInJson")
            Core.<*> (x Core..:? "UpdateTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The date and time at which the policy was created.
    createTime :: Core.Maybe Core.NominalDiffTime,
    -- | Contains the hash value associated with this policy.
    policyHash :: Core.Maybe Types.HashString,
    -- | Contains the requested policy document, in JSON format.
    policyInJson :: Core.Maybe Types.PolicyInJson,
    -- | The date and time at which the policy was last updated.
    updateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetResourcePolicyResponse' value with any optional fields omitted.
mkGetResourcePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetResourcePolicyResponse
mkGetResourcePolicyResponse responseStatus =
  GetResourcePolicyResponse'
    { createTime = Core.Nothing,
      policyHash = Core.Nothing,
      policyInJson = Core.Nothing,
      updateTime = Core.Nothing,
      responseStatus
    }

-- | The date and time at which the policy was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsCreateTime :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Core.NominalDiffTime)
grprfrsCreateTime = Lens.field @"createTime"
{-# DEPRECATED grprfrsCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | Contains the hash value associated with this policy.
--
-- /Note:/ Consider using 'policyHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsPolicyHash :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.HashString)
grprfrsPolicyHash = Lens.field @"policyHash"
{-# DEPRECATED grprfrsPolicyHash "Use generic-lens or generic-optics with 'policyHash' instead." #-}

-- | Contains the requested policy document, in JSON format.
--
-- /Note:/ Consider using 'policyInJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsPolicyInJson :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.PolicyInJson)
grprfrsPolicyInJson = Lens.field @"policyInJson"
{-# DEPRECATED grprfrsPolicyInJson "Use generic-lens or generic-optics with 'policyInJson' instead." #-}

-- | The date and time at which the policy was last updated.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsUpdateTime :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Core.NominalDiffTime)
grprfrsUpdateTime = Lens.field @"updateTime"
{-# DEPRECATED grprfrsUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprfrsResponseStatus :: Lens.Lens' GetResourcePolicyResponse Core.Int
grprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
