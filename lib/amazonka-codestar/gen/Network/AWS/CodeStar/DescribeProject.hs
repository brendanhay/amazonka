{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DescribeProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a project and its resources.
module Network.AWS.CodeStar.DescribeProject
  ( -- * Creating a request
    DescribeProject (..),
    mkDescribeProject,

    -- ** Request lenses
    dId,

    -- * Destructuring the response
    DescribeProjectResponse (..),
    mkDescribeProjectResponse,

    -- ** Response lenses
    drsArn,
    drsClientRequestToken,
    drsCreatedTimeStamp,
    drsDescription,
    drsId,
    drsName,
    drsProjectTemplateId,
    drsStackId,
    drsStatus,
    drsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeProject' smart constructor.
newtype DescribeProject = DescribeProject'
  { -- | The ID of the project.
    id :: Types.ProjectId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProject' value with any optional fields omitted.
mkDescribeProject ::
  -- | 'id'
  Types.ProjectId ->
  DescribeProject
mkDescribeProject id = DescribeProject' {id}

-- | The ID of the project.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DescribeProject Types.ProjectId
dId = Lens.field @"id"
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON DescribeProject where
  toJSON DescribeProject {..} =
    Core.object (Core.catMaybes [Core.Just ("id" Core..= id)])

instance Core.AWSRequest DescribeProject where
  type Rs DescribeProject = DescribeProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.DescribeProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "clientRequestToken")
            Core.<*> (x Core..:? "createdTimeStamp")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "projectTemplateId")
            Core.<*> (x Core..:? "stackId")
            Core.<*> (x Core..:? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | The Amazon Resource Name (ARN) for the project.
    arn :: Core.Maybe Types.Arn,
    -- | A user- or system-generated token that identifies the entity that requested project creation.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The date and time the project was created, in timestamp format.
    createdTimeStamp :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the project, if any.
    description :: Core.Maybe Types.Description,
    -- | The ID of the project.
    id :: Core.Maybe Types.Id,
    -- | The display name for the project.
    name :: Core.Maybe Types.Name,
    -- | The ID for the AWS CodeStar project template used to create the project.
    projectTemplateId :: Core.Maybe Types.ProjectTemplateId,
    -- | The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
    stackId :: Core.Maybe Types.StackId,
    -- | The project creation or deletion status.
    status :: Core.Maybe Types.ProjectStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProjectResponse' value with any optional fields omitted.
mkDescribeProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProjectResponse
mkDescribeProjectResponse responseStatus =
  DescribeProjectResponse'
    { arn = Core.Nothing,
      clientRequestToken = Core.Nothing,
      createdTimeStamp = Core.Nothing,
      description = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      projectTemplateId = Core.Nothing,
      stackId = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) for the project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsArn :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.Arn)
drsArn = Lens.field @"arn"
{-# DEPRECATED drsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A user- or system-generated token that identifies the entity that requested project creation.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsClientRequestToken :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.ClientRequestToken)
drsClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED drsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The date and time the project was created, in timestamp format.
--
-- /Note:/ Consider using 'createdTimeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreatedTimeStamp :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.NominalDiffTime)
drsCreatedTimeStamp = Lens.field @"createdTimeStamp"
{-# DEPRECATED drsCreatedTimeStamp "Use generic-lens or generic-optics with 'createdTimeStamp' instead." #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.Description)
drsDescription = Lens.field @"description"
{-# DEPRECATED drsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the project.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsId :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.Id)
drsId = Lens.field @"id"
{-# DEPRECATED drsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The display name for the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsName :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.Name)
drsName = Lens.field @"name"
{-# DEPRECATED drsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID for the AWS CodeStar project template used to create the project.
--
-- /Note:/ Consider using 'projectTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsProjectTemplateId :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.ProjectTemplateId)
drsProjectTemplateId = Lens.field @"projectTemplateId"
{-# DEPRECATED drsProjectTemplateId "Use generic-lens or generic-optics with 'projectTemplateId' instead." #-}

-- | The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStackId :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.StackId)
drsStackId = Lens.field @"stackId"
{-# DEPRECATED drsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The project creation or deletion status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStatus :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.ProjectStatus)
drsStatus = Lens.field @"status"
{-# DEPRECATED drsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeProjectResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
