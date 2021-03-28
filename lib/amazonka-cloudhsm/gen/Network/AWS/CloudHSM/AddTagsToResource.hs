{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.AddTagsToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Adds or overwrites one or more tags for the specified AWS CloudHSM resource.
-- Each tag consists of a key and a value. Tag keys must be unique to each resource.
module Network.AWS.CloudHSM.AddTagsToResource
    (
    -- * Creating a request
      AddTagsToResource (..)
    , mkAddTagsToResource
    -- ** Request lenses
    , attrResourceArn
    , attrTagList

    -- * Destructuring the response
    , AddTagsToResourceResponse (..)
    , mkAddTagsToResourceResponse
    -- ** Response lenses
    , attrrrsStatus
    , attrrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { resourceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
  , tagList :: [Types.Tag]
    -- ^ One or more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResource' value with any optional fields omitted.
mkAddTagsToResource
    :: Core.Text -- ^ 'resourceArn'
    -> AddTagsToResource
mkAddTagsToResource resourceArn
  = AddTagsToResource'{resourceArn, tagList = Core.mempty}

-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrResourceArn :: Lens.Lens' AddTagsToResource Core.Text
attrResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE attrResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrTagList :: Lens.Lens' AddTagsToResource [Types.Tag]
attrTagList = Lens.field @"tagList"
{-# INLINEABLE attrTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

instance Core.ToQuery AddTagsToResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddTagsToResource where
        toHeaders AddTagsToResource{..}
          = Core.pure
              ("X-Amz-Target", "CloudHsmFrontendService.AddTagsToResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddTagsToResource where
        toJSON AddTagsToResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
                  Core.Just ("TagList" Core..= tagList)])

instance Core.AWSRequest AddTagsToResource where
        type Rs AddTagsToResource = AddTagsToResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddTagsToResourceResponse' Core.<$>
                   (x Core..: "Status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { status :: Core.Text
    -- ^ The status of the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddTagsToResourceResponse' value with any optional fields omitted.
mkAddTagsToResourceResponse
    :: Core.Text -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> AddTagsToResourceResponse
mkAddTagsToResourceResponse status responseStatus
  = AddTagsToResourceResponse'{status, responseStatus}

-- | The status of the operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrrsStatus :: Lens.Lens' AddTagsToResourceResponse Core.Text
attrrrsStatus = Lens.field @"status"
{-# INLINEABLE attrrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attrrrsResponseStatus :: Lens.Lens' AddTagsToResourceResponse Core.Int
attrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE attrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
