{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Glue machine learning transform. Machine learning transforms are a special type of transform that use machine learning to learn the details of the transformation to be performed by learning from examples provided by humans. These transformations are then saved by AWS Glue. If you no longer need a transform, you can delete it by calling @DeleteMLTransforms@ . However, any AWS Glue jobs that still reference the deleted transform will no longer succeed.
module Network.AWS.Glue.DeleteMLTransform
  ( -- * Creating a request
    DeleteMLTransform (..),
    mkDeleteMLTransform,

    -- ** Request lenses
    dmltTransformId,

    -- * Destructuring the response
    DeleteMLTransformResponse (..),
    mkDeleteMLTransformResponse,

    -- ** Response lenses
    dmltrrsTransformId,
    dmltrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMLTransform' smart constructor.
newtype DeleteMLTransform = DeleteMLTransform'
  { -- | The unique identifier of the transform to delete.
    transformId :: Types.HashString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMLTransform' value with any optional fields omitted.
mkDeleteMLTransform ::
  -- | 'transformId'
  Types.HashString ->
  DeleteMLTransform
mkDeleteMLTransform transformId = DeleteMLTransform' {transformId}

-- | The unique identifier of the transform to delete.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmltTransformId :: Lens.Lens' DeleteMLTransform Types.HashString
dmltTransformId = Lens.field @"transformId"
{-# DEPRECATED dmltTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

instance Core.FromJSON DeleteMLTransform where
  toJSON DeleteMLTransform {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TransformId" Core..= transformId)])

instance Core.AWSRequest DeleteMLTransform where
  type Rs DeleteMLTransform = DeleteMLTransformResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteMLTransform")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMLTransformResponse'
            Core.<$> (x Core..:? "TransformId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteMLTransformResponse' smart constructor.
data DeleteMLTransformResponse = DeleteMLTransformResponse'
  { -- | The unique identifier of the transform that was deleted.
    transformId :: Core.Maybe Types.HashString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMLTransformResponse' value with any optional fields omitted.
mkDeleteMLTransformResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMLTransformResponse
mkDeleteMLTransformResponse responseStatus =
  DeleteMLTransformResponse'
    { transformId = Core.Nothing,
      responseStatus
    }

-- | The unique identifier of the transform that was deleted.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmltrrsTransformId :: Lens.Lens' DeleteMLTransformResponse (Core.Maybe Types.HashString)
dmltrrsTransformId = Lens.field @"transformId"
{-# DEPRECATED dmltrrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmltrrsResponseStatus :: Lens.Lens' DeleteMLTransformResponse Core.Int
dmltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
