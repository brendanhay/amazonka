{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a project in AWS CodeStar.
module Network.AWS.CodeStar.UpdateProject
  ( -- * Creating a request
    UpdateProject (..),
    mkUpdateProject,

    -- ** Request lenses
    upId,
    upDescription,
    upName,

    -- * Destructuring the response
    UpdateProjectResponse (..),
    mkUpdateProjectResponse,

    -- ** Response lenses
    uprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | The ID of the project you want to update.
    id :: Types.Id,
    -- | The description of the project, if any.
    description :: Core.Maybe Types.Description,
    -- | The name of the project you want to update.
    name :: Core.Maybe Types.ProjectName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProject' value with any optional fields omitted.
mkUpdateProject ::
  -- | 'id'
  Types.Id ->
  UpdateProject
mkUpdateProject id =
  UpdateProject'
    { id,
      description = Core.Nothing,
      name = Core.Nothing
    }

-- | The ID of the project you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UpdateProject Types.Id
upId = Lens.field @"id"
{-# DEPRECATED upId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdateProject (Core.Maybe Types.Description)
upDescription = Lens.field @"description"
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the project you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectName)
upName = Lens.field @"name"
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateProject where
  toJSON UpdateProject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("id" Core..= id),
            ("description" Core..=) Core.<$> description,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateProject where
  type Rs UpdateProject = UpdateProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.UpdateProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProjectResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateProjectResponse' smart constructor.
newtype UpdateProjectResponse = UpdateProjectResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProjectResponse' value with any optional fields omitted.
mkUpdateProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateProjectResponse
mkUpdateProjectResponse responseStatus =
  UpdateProjectResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProjectResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
