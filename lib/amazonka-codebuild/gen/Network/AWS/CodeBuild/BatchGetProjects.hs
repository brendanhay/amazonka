{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more build projects.
module Network.AWS.CodeBuild.BatchGetProjects
  ( -- * Creating a request
    BatchGetProjects (..),
    mkBatchGetProjects,

    -- ** Request lenses
    bgpNames,

    -- * Destructuring the response
    BatchGetProjectsResponse (..),
    mkBatchGetProjectsResponse,

    -- ** Response lenses
    bgprrsProjects,
    bgprrsProjectsNotFound,
    bgprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetProjects' smart constructor.
newtype BatchGetProjects = BatchGetProjects'
  { -- | The names or ARNs of the build projects. To get information about a project shared with your AWS account, its ARN must be specified. You cannot specify a shared project using its name.
    names :: Core.NonEmpty Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetProjects' value with any optional fields omitted.
mkBatchGetProjects ::
  -- | 'names'
  Core.NonEmpty Types.NonEmptyString ->
  BatchGetProjects
mkBatchGetProjects names = BatchGetProjects' {names}

-- | The names or ARNs of the build projects. To get information about a project shared with your AWS account, its ARN must be specified. You cannot specify a shared project using its name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpNames :: Lens.Lens' BatchGetProjects (Core.NonEmpty Types.NonEmptyString)
bgpNames = Lens.field @"names"
{-# DEPRECATED bgpNames "Use generic-lens or generic-optics with 'names' instead." #-}

instance Core.FromJSON BatchGetProjects where
  toJSON BatchGetProjects {..} =
    Core.object (Core.catMaybes [Core.Just ("names" Core..= names)])

instance Core.AWSRequest BatchGetProjects where
  type Rs BatchGetProjects = BatchGetProjectsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.BatchGetProjects")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetProjectsResponse'
            Core.<$> (x Core..:? "projects")
            Core.<*> (x Core..:? "projectsNotFound")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchGetProjectsResponse' smart constructor.
data BatchGetProjectsResponse = BatchGetProjectsResponse'
  { -- | Information about the requested build projects.
    projects :: Core.Maybe [Types.Project],
    -- | The names of build projects for which information could not be found.
    projectsNotFound :: Core.Maybe (Core.NonEmpty Types.NonEmptyString),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetProjectsResponse' value with any optional fields omitted.
mkBatchGetProjectsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetProjectsResponse
mkBatchGetProjectsResponse responseStatus =
  BatchGetProjectsResponse'
    { projects = Core.Nothing,
      projectsNotFound = Core.Nothing,
      responseStatus
    }

-- | Information about the requested build projects.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsProjects :: Lens.Lens' BatchGetProjectsResponse (Core.Maybe [Types.Project])
bgprrsProjects = Lens.field @"projects"
{-# DEPRECATED bgprrsProjects "Use generic-lens or generic-optics with 'projects' instead." #-}

-- | The names of build projects for which information could not be found.
--
-- /Note:/ Consider using 'projectsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsProjectsNotFound :: Lens.Lens' BatchGetProjectsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
bgprrsProjectsNotFound = Lens.field @"projectsNotFound"
{-# DEPRECATED bgprrsProjectsNotFound "Use generic-lens or generic-optics with 'projectsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsResponseStatus :: Lens.Lens' BatchGetProjectsResponse Core.Int
bgprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
