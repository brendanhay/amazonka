{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Rekognition Custom Labels project. A project is a logical grouping of resources (images, Labels, models) and operations (training, evaluation and detection).
--
-- This operation requires permissions to perform the @rekognition:CreateProject@ action.
module Network.AWS.Rekognition.CreateProject
  ( -- * Creating a request
    CreateProject (..),
    mkCreateProject,

    -- ** Request lenses
    cpProjectName,

    -- * Destructuring the response
    CreateProjectResponse (..),
    mkCreateProjectResponse,

    -- ** Response lenses
    cprrsProjectArn,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProject' smart constructor.
newtype CreateProject = CreateProject'
  { -- | The name of the project to create.
    projectName :: Types.ProjectName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProject' value with any optional fields omitted.
mkCreateProject ::
  -- | 'projectName'
  Types.ProjectName ->
  CreateProject
mkCreateProject projectName = CreateProject' {projectName}

-- | The name of the project to create.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProjectName :: Lens.Lens' CreateProject Types.ProjectName
cpProjectName = Lens.field @"projectName"
{-# DEPRECATED cpProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

instance Core.FromJSON CreateProject where
  toJSON CreateProject {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ProjectName" Core..= projectName)])

instance Core.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "RekognitionService.CreateProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Core.<$> (x Core..:? "ProjectArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project.
    projectArn :: Core.Maybe Types.ProjectArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProjectResponse' value with any optional fields omitted.
mkCreateProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProjectResponse
mkCreateProjectResponse responseStatus =
  CreateProjectResponse' {projectArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProjectArn :: Lens.Lens' CreateProjectResponse (Core.Maybe Types.ProjectArn)
cprrsProjectArn = Lens.field @"projectArn"
{-# DEPRECATED cprrsProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProjectResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
