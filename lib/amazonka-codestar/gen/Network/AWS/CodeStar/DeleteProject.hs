{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a project, including project resources. Does not delete users associated with the project, but does delete the IAM roles that allowed access to the project.
module Network.AWS.CodeStar.DeleteProject
  ( -- * Creating a request
    DeleteProject (..),
    mkDeleteProject,

    -- ** Request lenses
    dpId,
    dpClientRequestToken,
    dpDeleteStack,

    -- * Destructuring the response
    DeleteProjectResponse (..),
    mkDeleteProjectResponse,

    -- ** Response lenses
    dprrsProjectArn,
    dprrsStackId,
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | The ID of the project to be deleted in AWS CodeStar.
    id :: Types.Id,
    -- | A user- or system-generated token that identifies the entity that requested project deletion. This token can be used to repeat the request.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | Whether to send a delete request for the primary stack in AWS CloudFormation originally used to generate the project and its resources. This option will delete all AWS resources for the project (except for any buckets in Amazon S3) as well as deleting the project itself. Recommended for most use cases.
    deleteStack :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProject' value with any optional fields omitted.
mkDeleteProject ::
  -- | 'id'
  Types.Id ->
  DeleteProject
mkDeleteProject id =
  DeleteProject'
    { id,
      clientRequestToken = Core.Nothing,
      deleteStack = Core.Nothing
    }

-- | The ID of the project to be deleted in AWS CodeStar.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpId :: Lens.Lens' DeleteProject Types.Id
dpId = Lens.field @"id"
{-# DEPRECATED dpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A user- or system-generated token that identifies the entity that requested project deletion. This token can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpClientRequestToken :: Lens.Lens' DeleteProject (Core.Maybe Types.ClientRequestToken)
dpClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED dpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Whether to send a delete request for the primary stack in AWS CloudFormation originally used to generate the project and its resources. This option will delete all AWS resources for the project (except for any buckets in Amazon S3) as well as deleting the project itself. Recommended for most use cases.
--
-- /Note:/ Consider using 'deleteStack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDeleteStack :: Lens.Lens' DeleteProject (Core.Maybe Core.Bool)
dpDeleteStack = Lens.field @"deleteStack"
{-# DEPRECATED dpDeleteStack "Use generic-lens or generic-optics with 'deleteStack' instead." #-}

instance Core.FromJSON DeleteProject where
  toJSON DeleteProject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("id" Core..= id),
            ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("deleteStack" Core..=) Core.<$> deleteStack
          ]
      )

instance Core.AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.DeleteProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Core.<$> (x Core..:? "projectArn")
            Core.<*> (x Core..:? "stackId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted project.
    projectArn :: Core.Maybe Types.ProjectArn,
    -- | The ID of the primary stack in AWS CloudFormation that will be deleted as part of deleting the project and its resources.
    stackId :: Core.Maybe Types.StackId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProjectResponse' value with any optional fields omitted.
mkDeleteProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteProjectResponse
mkDeleteProjectResponse responseStatus =
  DeleteProjectResponse'
    { projectArn = Core.Nothing,
      stackId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the deleted project.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsProjectArn :: Lens.Lens' DeleteProjectResponse (Core.Maybe Types.ProjectArn)
dprrsProjectArn = Lens.field @"projectArn"
{-# DEPRECATED dprrsProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | The ID of the primary stack in AWS CloudFormation that will be deleted as part of deleting the project and its resources.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsStackId :: Lens.Lens' DeleteProjectResponse (Core.Maybe Types.StackId)
dprrsStackId = Lens.field @"stackId"
{-# DEPRECATED dprrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DeleteProjectResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
