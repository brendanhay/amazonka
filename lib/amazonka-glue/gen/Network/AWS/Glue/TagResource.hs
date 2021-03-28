{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a resource. A tag is a label you can assign to an AWS resource. In AWS Glue, you can tag only certain resources. For information about what resources you can tag, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> .
module Network.AWS.Glue.TagResource
    (
    -- * Creating a request
      TagResource (..)
    , mkTagResource
    -- ** Request lenses
    , trResourceArn
    , trTagsToAdd

    -- * Destructuring the response
    , TagResourceResponse (..)
    , mkTagResourceResponse
    -- ** Response lenses
    , trrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { resourceArn :: Types.GlueResourceArn
    -- ^ The ARN of the AWS Glue resource to which to add the tags. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern> .
  , tagsToAdd :: Core.HashMap Types.TagKey Types.TagValue
    -- ^ Tags to add to this resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResource' value with any optional fields omitted.
mkTagResource
    :: Types.GlueResourceArn -- ^ 'resourceArn'
    -> TagResource
mkTagResource resourceArn
  = TagResource'{resourceArn, tagsToAdd = Core.mempty}

-- | The ARN of the AWS Glue resource to which to add the tags. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern> .
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceArn :: Lens.Lens' TagResource Types.GlueResourceArn
trResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE trResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | Tags to add to this resource.
--
-- /Note:/ Consider using 'tagsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTagsToAdd :: Lens.Lens' TagResource (Core.HashMap Types.TagKey Types.TagValue)
trTagsToAdd = Lens.field @"tagsToAdd"
{-# INLINEABLE trTagsToAdd #-}
{-# DEPRECATED tagsToAdd "Use generic-lens or generic-optics with 'tagsToAdd' instead"  #-}

instance Core.ToQuery TagResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TagResource where
        toHeaders TagResource{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.TagResource") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TagResource where
        toJSON TagResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
                  Core.Just ("TagsToAdd" Core..= tagsToAdd)])

instance Core.AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 TagResourceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagResourceResponse' value with any optional fields omitted.
mkTagResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TagResourceResponse
mkTagResourceResponse responseStatus
  = TagResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trrrsResponseStatus :: Lens.Lens' TagResourceResponse Core.Int
trrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE trrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
