{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing project.
module Network.AWS.Mobile.UpdateProject
  ( -- * Creating a request
    UpdateProject (..),
    mkUpdateProject,

    -- ** Request lenses
    upProjectId,
    upContents,

    -- * Destructuring the response
    UpdateProjectResponse (..),
    mkUpdateProjectResponse,

    -- ** Response lenses
    uprrsDetails,
    uprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used for requests to update project configuration.
--
-- /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | Unique project identifier.
    projectId :: Types.ProjectId,
    -- | ZIP or YAML file which contains project configuration to be updated. This should be the contents of the file downloaded from the URL provided in an export project operation.
    contents :: Core.Maybe Core.ByteString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProject' value with any optional fields omitted.
mkUpdateProject ::
  -- | 'projectId'
  Types.ProjectId ->
  UpdateProject
mkUpdateProject projectId =
  UpdateProject' {projectId, contents = Core.Nothing}

-- | Unique project identifier.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProjectId :: Lens.Lens' UpdateProject Types.ProjectId
upProjectId = Lens.field @"projectId"
{-# DEPRECATED upProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | ZIP or YAML file which contains project configuration to be updated. This should be the contents of the file downloaded from the URL provided in an export project operation.
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContents :: Lens.Lens' UpdateProject (Core.Maybe Core.ByteString)
upContents = Lens.field @"contents"
{-# DEPRECATED upContents "Use generic-lens or generic-optics with 'contents' instead." #-}

instance Core.AWSRequest UpdateProject where
  type Rs UpdateProject = UpdateProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/update",
        Core._rqQuery = Core.toQueryValue "projectId" projectId,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toBody contents
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Core.<$> (x Core..:? "details") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Result structure used for requests to updated project configuration.
--
-- /See:/ 'mkUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | Detailed information about the updated AWS Mobile Hub project.
    details :: Core.Maybe Types.ProjectDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateProjectResponse' value with any optional fields omitted.
mkUpdateProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateProjectResponse
mkUpdateProjectResponse responseStatus =
  UpdateProjectResponse' {details = Core.Nothing, responseStatus}

-- | Detailed information about the updated AWS Mobile Hub project.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsDetails :: Lens.Lens' UpdateProjectResponse (Core.Maybe Types.ProjectDetails)
uprrsDetails = Lens.field @"details"
{-# DEPRECATED uprrsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProjectResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
