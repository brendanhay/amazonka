{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of a SageMaker image. The container image the version represents isn't deleted.
module Network.AWS.SageMaker.DeleteImageVersion
  ( -- * Creating a request
    DeleteImageVersion (..),
    mkDeleteImageVersion,

    -- ** Request lenses
    divImageName,
    divVersion,

    -- * Destructuring the response
    DeleteImageVersionResponse (..),
    mkDeleteImageVersionResponse,

    -- ** Response lenses
    divrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteImageVersion' smart constructor.
data DeleteImageVersion = DeleteImageVersion'
  { -- | The name of the image.
    imageName :: Types.ImageName,
    -- | The version to delete.
    version :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImageVersion' value with any optional fields omitted.
mkDeleteImageVersion ::
  -- | 'imageName'
  Types.ImageName ->
  -- | 'version'
  Core.Natural ->
  DeleteImageVersion
mkDeleteImageVersion imageName version =
  DeleteImageVersion' {imageName, version}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divImageName :: Lens.Lens' DeleteImageVersion Types.ImageName
divImageName = Lens.field @"imageName"
{-# DEPRECATED divImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The version to delete.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divVersion :: Lens.Lens' DeleteImageVersion Core.Natural
divVersion = Lens.field @"version"
{-# DEPRECATED divVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON DeleteImageVersion where
  toJSON DeleteImageVersion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ImageName" Core..= imageName),
            Core.Just ("Version" Core..= version)
          ]
      )

instance Core.AWSRequest DeleteImageVersion where
  type Rs DeleteImageVersion = DeleteImageVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteImageVersion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteImageVersionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteImageVersionResponse' smart constructor.
newtype DeleteImageVersionResponse = DeleteImageVersionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImageVersionResponse' value with any optional fields omitted.
mkDeleteImageVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteImageVersionResponse
mkDeleteImageVersionResponse responseStatus =
  DeleteImageVersionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrrsResponseStatus :: Lens.Lens' DeleteImageVersionResponse Core.Int
divrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED divrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
