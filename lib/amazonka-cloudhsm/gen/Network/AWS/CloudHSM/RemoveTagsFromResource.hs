{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RemoveTagsFromResource (..),
    mkRemoveTagsFromResource,

    -- ** Request lenses
    rtfrResourceArn,
    rtfrTagKeyList,

    -- * Destructuring the response
    RemoveTagsFromResourceResponse (..),
    mkRemoveTagsFromResourceResponse,

    -- ** Response lenses
    rtfrrrsStatus,
    rtfrrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
    resourceArn :: Types.ResourceArn,
    -- | The tag key or keys to remove.
    --
    -- Specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
    tagKeyList :: [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResource' value with any optional fields omitted.
mkRemoveTagsFromResource ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  RemoveTagsFromResource
mkRemoveTagsFromResource resourceArn =
  RemoveTagsFromResource' {resourceArn, tagKeyList = Core.mempty}

-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrResourceArn :: Lens.Lens' RemoveTagsFromResource Types.ResourceArn
rtfrResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED rtfrResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | The tag key or keys to remove.
--
-- Specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use 'AddTagsToResource' .
--
-- /Note:/ Consider using 'tagKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrTagKeyList :: Lens.Lens' RemoveTagsFromResource [Types.TagKey]
rtfrTagKeyList = Lens.field @"tagKeyList"
{-# DEPRECATED rtfrTagKeyList "Use generic-lens or generic-optics with 'tagKeyList' instead." #-}

instance Core.FromJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceArn" Core..= resourceArn),
            Core.Just ("TagKeyList" Core..= tagKeyList)
          ]
      )

instance Core.AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CloudHsmFrontendService.RemoveTagsFromResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Core.<$> (x Core..: "Status") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The status of the operation.
    status :: Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromResourceResponse' value with any optional fields omitted.
mkRemoveTagsFromResourceResponse ::
  -- | 'status'
  Types.String ->
  -- | 'responseStatus'
  Core.Int ->
  RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse status responseStatus =
  RemoveTagsFromResourceResponse' {status, responseStatus}

-- | The status of the operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsStatus :: Lens.Lens' RemoveTagsFromResourceResponse Types.String
rtfrrrsStatus = Lens.field @"status"
{-# DEPRECATED rtfrrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfrrrsResponseStatus :: Lens.Lens' RemoveTagsFromResourceResponse Core.Int
rtfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
