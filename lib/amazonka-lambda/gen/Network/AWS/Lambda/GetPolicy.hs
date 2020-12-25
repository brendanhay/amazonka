{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the <https://docs.aws.amazon.com/lambda/latest/dg/access-control-resource-based.html resource-based IAM policy> for a function, version, or alias.
module Network.AWS.Lambda.GetPolicy
  ( -- * Creating a request
    GetPolicy (..),
    mkGetPolicy,

    -- ** Request lenses
    gpFunctionName,
    gpQualifier,

    -- * Destructuring the response
    GetPolicyResponse (..),
    mkGetPolicyResponse,

    -- ** Response lenses
    gprrsPolicy,
    gprrsRevisionId,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:my-function@ .
    --
    --
    -- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Types.NamespacedFunctionName,
    -- | Specify a version or alias to get the policy for that resource.
    qualifier :: Core.Maybe Types.Qualifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicy' value with any optional fields omitted.
mkGetPolicy ::
  -- | 'functionName'
  Types.NamespacedFunctionName ->
  GetPolicy
mkGetPolicy functionName =
  GetPolicy' {functionName, qualifier = Core.Nothing}

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpFunctionName :: Lens.Lens' GetPolicy Types.NamespacedFunctionName
gpFunctionName = Lens.field @"functionName"
{-# DEPRECATED gpFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a version or alias to get the policy for that resource.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpQualifier :: Lens.Lens' GetPolicy (Core.Maybe Types.Qualifier)
gpQualifier = Lens.field @"qualifier"
{-# DEPRECATED gpQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-03-31/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/policy")
            ),
        Core._rqQuery = Core.toQueryValue "Qualifier" Core.<$> qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Core.<$> (x Core..:? "Policy")
            Core.<*> (x Core..:? "RevisionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | The resource-based policy.
    policy :: Core.Maybe Types.String,
    -- | A unique identifier for the current revision of the policy.
    revisionId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicyResponse' value with any optional fields omitted.
mkGetPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPolicyResponse
mkGetPolicyResponse responseStatus =
  GetPolicyResponse'
    { policy = Core.Nothing,
      revisionId = Core.Nothing,
      responseStatus
    }

-- | The resource-based policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicy :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.String)
gprrsPolicy = Lens.field @"policy"
{-# DEPRECATED gprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | A unique identifier for the current revision of the policy.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsRevisionId :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.String)
gprrsRevisionId = Lens.field @"revisionId"
{-# DEPRECATED gprrsRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPolicyResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
