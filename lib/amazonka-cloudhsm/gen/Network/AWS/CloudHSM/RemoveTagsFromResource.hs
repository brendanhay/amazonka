{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Removes one or more tags from the specified AWS CloudHSM resource.
-- To remove a tag, specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
module Network.AWS.CloudHSM.RemoveTagsFromResource
    (
    -- * Creating a request
      RemoveTagsFromResource (..)
    , mkRemoveTagsFromResource
    -- ** Request lenses
    , rtfrResourceArn
    , rtfrTagKeyList

    -- * Destructuring the response
    , RemoveTagsFromResourceResponse (..)
    , mkRemoveTagsFromResourceResponse
    -- ** Response lenses
    , rtfrrrsStatus
    , rtfrrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { resourceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
  , tagKeyList :: [Types.TagKey]
    -- ^ The tag key or keys to remove.
--
-- Specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource
    :: Core.Text -- ^ 'resourceArn'
    -> RemoveTagsFromResource
mkRemoveTagsFromResource resourceArn
  = RemoveTagsFromResource'{resourceArn, tagKeyList = Core.mempty}

-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceArn :: Lens.Lens' RemoveTagsFromResource Core.Text
rtfrResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE rtfrResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The tag key or keys to remove.
--
-- Specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
--
-- /Note:/ Consider using 'tagKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeyList :: Lens.Lens' RemoveTagsFromResource [Types.TagKey]
rtfrTagKeyList = Lens.field @"tagKeyList"
{-# INLINEABLE rtfrTagKeyList #-}
{-# DEPRECATED tagKeyList "Use generic-lens or generic-optics with 'tagKeyList' instead"  #-}

instance Core.ToQuery RemoveTagsFromResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveTagsFromResource where
        toHeaders RemoveTagsFromResource{..}
          = Core.pure
              ("X-Amz-Target", "CloudHsmFrontendService.RemoveTagsFromResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveTagsFromResource where
        toJSON RemoveTagsFromResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
                  Core.Just ("TagKeyList" Core..= tagKeyList)])

instance Core.AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RemoveTagsFromResourceResponse' Core.<$>
                   (x Core..: "Status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { status :: Core.Text
    -- ^ The status of the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResourceResponse' value with any optional fields omitted.
mkRemoveTagsFromResourceResponse
    :: Core.Text -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse status responseStatus
  = RemoveTagsFromResourceResponse'{status, responseStatus}

-- | The status of the operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsStatus :: Lens.Lens' RemoveTagsFromResourceResponse Core.Text
rtfrrrsStatus = Lens.field @"status"
{-# INLINEABLE rtfrrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Core.Int
rtfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
