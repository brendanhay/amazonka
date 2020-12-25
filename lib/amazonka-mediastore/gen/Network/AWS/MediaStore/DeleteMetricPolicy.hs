{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteMetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the metric policy that is associated with the specified container. If there is no metric policy associated with the container, MediaStore doesn't send metrics to CloudWatch.
module Network.AWS.MediaStore.DeleteMetricPolicy
  ( -- * Creating a request
    DeleteMetricPolicy (..),
    mkDeleteMetricPolicy,

    -- ** Request lenses
    dmpContainerName,

    -- * Destructuring the response
    DeleteMetricPolicyResponse (..),
    mkDeleteMetricPolicyResponse,

    -- ** Response lenses
    dmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMetricPolicy' smart constructor.
newtype DeleteMetricPolicy = DeleteMetricPolicy'
  { -- | The name of the container that is associated with the metric policy that you want to delete.
    containerName :: Types.ContainerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMetricPolicy' value with any optional fields omitted.
mkDeleteMetricPolicy ::
  -- | 'containerName'
  Types.ContainerName ->
  DeleteMetricPolicy
mkDeleteMetricPolicy containerName =
  DeleteMetricPolicy' {containerName}

-- | The name of the container that is associated with the metric policy that you want to delete.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpContainerName :: Lens.Lens' DeleteMetricPolicy Types.ContainerName
dmpContainerName = Lens.field @"containerName"
{-# DEPRECATED dmpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Core.FromJSON DeleteMetricPolicy where
  toJSON DeleteMetricPolicy {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.AWSRequest DeleteMetricPolicy where
  type Rs DeleteMetricPolicy = DeleteMetricPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MediaStore_20170901.DeleteMetricPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMetricPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteMetricPolicyResponse' smart constructor.
newtype DeleteMetricPolicyResponse = DeleteMetricPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMetricPolicyResponse' value with any optional fields omitted.
mkDeleteMetricPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMetricPolicyResponse
mkDeleteMetricPolicyResponse responseStatus =
  DeleteMetricPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsResponseStatus :: Lens.Lens' DeleteMetricPolicyResponse Core.Int
dmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
