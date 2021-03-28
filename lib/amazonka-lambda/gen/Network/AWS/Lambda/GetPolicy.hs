{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetPolicy (..)
    , mkGetPolicy
    -- ** Request lenses
    , gpFunctionName
    , gpQualifier

    -- * Destructuring the response
    , GetPolicyResponse (..)
    , mkGetPolicyResponse
    -- ** Response lenses
    , gprrsPolicy
    , gprrsRevisionId
    , gprrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { functionName :: Types.NamespacedFunctionName
    -- ^ The name of the Lambda function, version, or alias.
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
  , qualifier :: Core.Maybe Types.Qualifier
    -- ^ Specify a version or alias to get the policy for that resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicy' value with any optional fields omitted.
mkGetPolicy
    :: Types.NamespacedFunctionName -- ^ 'functionName'
    -> GetPolicy
mkGetPolicy functionName
  = GetPolicy'{functionName, qualifier = Core.Nothing}

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
{-# INLINEABLE gpFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | Specify a version or alias to get the policy for that resource.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpQualifier :: Lens.Lens' GetPolicy (Core.Maybe Types.Qualifier)
gpQualifier = Lens.field @"qualifier"
{-# INLINEABLE gpQualifier #-}
{-# DEPRECATED qualifier "Use generic-lens or generic-optics with 'qualifier' instead"  #-}

instance Core.ToQuery GetPolicy where
        toQuery GetPolicy{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Qualifier") qualifier

instance Core.ToHeaders GetPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPolicy where
        type Rs GetPolicy = GetPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-03-31/functions/" Core.<> Core.toText functionName Core.<>
                             "/policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPolicyResponse' Core.<$>
                   (x Core..:? "Policy") Core.<*> x Core..:? "RevisionId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { policy :: Core.Maybe Core.Text
    -- ^ The resource-based policy.
  , revisionId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the current revision of the policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicyResponse' value with any optional fields omitted.
mkGetPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPolicyResponse
mkGetPolicyResponse responseStatus
  = GetPolicyResponse'{policy = Core.Nothing,
                       revisionId = Core.Nothing, responseStatus}

-- | The resource-based policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicy :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
gprrsPolicy = Lens.field @"policy"
{-# INLINEABLE gprrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | A unique identifier for the current revision of the policy.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsRevisionId :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
gprrsRevisionId = Lens.field @"revisionId"
{-# INLINEABLE gprrsRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPolicyResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
