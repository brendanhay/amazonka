{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Selenium testing project and all content generated under it.
--
-- /Important:/ You cannot undo this operation.
module Network.AWS.DeviceFarm.DeleteTestGridProject
  ( -- * Creating a request
    DeleteTestGridProject (..),
    mkDeleteTestGridProject,

    -- ** Request lenses
    dtgpProjectArn,

    -- * Destructuring the response
    DeleteTestGridProjectResponse (..),
    mkDeleteTestGridProjectResponse,

    -- ** Response lenses
    dtgprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTestGridProject' smart constructor.
newtype DeleteTestGridProject = DeleteTestGridProject'
  { -- | The ARN of the project to delete, from 'CreateTestGridProject' or 'ListTestGridProjects' .
    projectArn :: Types.DeviceFarmArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTestGridProject' value with any optional fields omitted.
mkDeleteTestGridProject ::
  -- | 'projectArn'
  Types.DeviceFarmArn ->
  DeleteTestGridProject
mkDeleteTestGridProject projectArn =
  DeleteTestGridProject' {projectArn}

-- | The ARN of the project to delete, from 'CreateTestGridProject' or 'ListTestGridProjects' .
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpProjectArn :: Lens.Lens' DeleteTestGridProject Types.DeviceFarmArn
dtgpProjectArn = Lens.field @"projectArn"
{-# DEPRECATED dtgpProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

instance Core.FromJSON DeleteTestGridProject where
  toJSON DeleteTestGridProject {..} =
    Core.object
      (Core.catMaybes [Core.Just ("projectArn" Core..= projectArn)])

instance Core.AWSRequest DeleteTestGridProject where
  type Rs DeleteTestGridProject = DeleteTestGridProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.DeleteTestGridProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTestGridProjectResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTestGridProjectResponse' smart constructor.
newtype DeleteTestGridProjectResponse = DeleteTestGridProjectResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTestGridProjectResponse' value with any optional fields omitted.
mkDeleteTestGridProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTestGridProjectResponse
mkDeleteTestGridProjectResponse responseStatus =
  DeleteTestGridProjectResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgprrsResponseStatus :: Lens.Lens' DeleteTestGridProjectResponse Core.Int
dtgprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
