{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an AWS DMS resource, including replication instance, endpoint, security group, and migration task. These tags can also be used with cost allocation reporting to track cost associated with DMS resources, or used in a Condition statement in an IAM policy for DMS. For more information, see <https://docs.aws.amazon.com/dms/latest/APIReference/API_Tag.html @Tag@ > data type description.
module Network.AWS.DMS.AddTagsToResource
    (
    -- * Creating a request
      AddTagsToResource (..)
    , mkAddTagsToResource
    -- ** Request lenses
    , attrResourceArn
    , attrTags

    -- * Destructuring the response
    , AddTagsToResourceResponse (..)
    , mkAddTagsToResourceResponse
    -- ** Response lenses
    , attrrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Associates a set of tags with an AWS DMS resource.
--
-- /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceArn :: Core.Text
    -- ^ Identifies the AWS DMS resource to which tags should be added. The value for this parameter is an Amazon Resource Name (ARN).
--
-- For AWS DMS, you can tag a replication instance, an endpoint, or a replication task.
  , tags :: [Types.Tag]
    -- ^ One or more tags to be assigned to the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResource' value with any optional fields omitted.
mkAddTagsToResource
    :: Core.Text -- ^ 'resourceArn'
    -> AddTagsToResource
mkAddTagsToResource resourceArn
  = AddTagsToResource'{resourceArn, tags = Core.mempty}

-- | Identifies the AWS DMS resource to which tags should be added. The value for this parameter is an Amazon Resource Name (ARN).
--
-- For AWS DMS, you can tag a replication instance, an endpoint, or a replication task.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceArn :: Lens.Lens' AddTagsToResource Core.Text
attrResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE attrResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | One or more tags to be assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTags :: Lens.Lens' AddTagsToResource [Types.Tag]
attrTags = Lens.field @"tags"
{-# INLINEABLE attrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery AddTagsToResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddTagsToResource where
        toHeaders AddTagsToResource{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.AddTagsToResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddTagsToResource where
        toJSON AddTagsToResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
                  Core.Just ("Tags" Core..= tags)])

instance Core.AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddTagsToResourceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkAddTagsToResourceResponse' smart constructor.
newtype AddTagsToResourceResponse = AddTagsToResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResourceResponse' value with any optional fields omitted.
mkAddTagsToResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddTagsToResourceResponse
mkAddTagsToResourceResponse responseStatus
  = AddTagsToResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Core.Int
attrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE attrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
