{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a project.
module Network.AWS.DeviceFarm.GetProject
  ( -- * Creating a request
    GetProject (..),
    mkGetProject,

    -- ** Request lenses
    gpArn,

    -- * Destructuring the response
    GetProjectResponse (..),
    mkGetProjectResponse,

    -- ** Response lenses
    gprrsProject,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get project operation.
--
-- /See:/ 'mkGetProject' smart constructor.
newtype GetProject = GetProject'
  { -- | The project's ARN.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetProject' value with any optional fields omitted.
mkGetProject ::
  -- | 'arn'
  Types.Arn ->
  GetProject
mkGetProject arn = GetProject' {arn}

-- | The project's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpArn :: Lens.Lens' GetProject Types.Arn
gpArn = Lens.field @"arn"
{-# DEPRECATED gpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetProject where
  toJSON GetProject {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetProject where
  type Rs GetProject = GetProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProjectResponse'
            Core.<$> (x Core..:? "project") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a get project request.
--
-- /See:/ 'mkGetProjectResponse' smart constructor.
data GetProjectResponse = GetProjectResponse'
  { -- | The project to get information about.
    project :: Core.Maybe Types.Project,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetProjectResponse' value with any optional fields omitted.
mkGetProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetProjectResponse
mkGetProjectResponse responseStatus =
  GetProjectResponse' {project = Core.Nothing, responseStatus}

-- | The project to get information about.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsProject :: Lens.Lens' GetProjectResponse (Core.Maybe Types.Project)
gprrsProject = Lens.field @"project"
{-# DEPRECATED gprrsProject "Use generic-lens or generic-optics with 'project' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetProjectResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
