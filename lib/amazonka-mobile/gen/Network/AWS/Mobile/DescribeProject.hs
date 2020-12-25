{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.DescribeProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a project in AWS Mobile Hub.
module Network.AWS.Mobile.DescribeProject
  ( -- * Creating a request
    DescribeProject (..),
    mkDescribeProject,

    -- ** Request lenses
    dProjectId,
    dSyncFromResources,

    -- * Destructuring the response
    DescribeProjectResponse (..),
    mkDescribeProjectResponse,

    -- ** Response lenses
    drsDetails,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request details about a project.
--
-- /See:/ 'mkDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | Unique project identifier.
    projectId :: Types.ProjectId,
    -- | If set to true, causes AWS Mobile Hub to synchronize information from other services, e.g., update state of AWS CloudFormation stacks in the AWS Mobile Hub project.
    syncFromResources :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProject' value with any optional fields omitted.
mkDescribeProject ::
  -- | 'projectId'
  Types.ProjectId ->
  DescribeProject
mkDescribeProject projectId =
  DescribeProject' {projectId, syncFromResources = Core.Nothing}

-- | Unique project identifier.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dProjectId :: Lens.Lens' DescribeProject Types.ProjectId
dProjectId = Lens.field @"projectId"
{-# DEPRECATED dProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | If set to true, causes AWS Mobile Hub to synchronize information from other services, e.g., update state of AWS CloudFormation stacks in the AWS Mobile Hub project.
--
-- /Note:/ Consider using 'syncFromResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSyncFromResources :: Lens.Lens' DescribeProject (Core.Maybe Core.Bool)
dSyncFromResources = Lens.field @"syncFromResources"
{-# DEPRECATED dSyncFromResources "Use generic-lens or generic-optics with 'syncFromResources' instead." #-}

instance Core.AWSRequest DescribeProject where
  type Rs DescribeProject = DescribeProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/project",
        Core._rqQuery =
          Core.toQueryValue "projectId" projectId
            Core.<> (Core.toQueryValue "syncFromResources" Core.<$> syncFromResources),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Core.<$> (x Core..:? "details") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Result structure used for requests of project details.
--
-- /See:/ 'mkDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { details :: Core.Maybe Types.ProjectDetails,
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
  DescribeProjectResponse' {details = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDetails :: Lens.Lens' DescribeProjectResponse (Core.Maybe Types.ProjectDetails)
drsDetails = Lens.field @"details"
{-# DEPRECATED drsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeProjectResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
