{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      BatchGetProjects (..)
    , mkBatchGetProjects
    -- ** Request lenses
    , bgpNames

    -- * Destructuring the response
    , BatchGetProjectsResponse (..)
    , mkBatchGetProjectsResponse
    -- ** Response lenses
    , bgprrsProjects
    , bgprrsProjectsNotFound
    , bgprrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetProjects' smart constructor.
newtype BatchGetProjects = BatchGetProjects'
  { names :: Core.NonEmpty Types.NonEmptyString
    -- ^ The names or ARNs of the build projects. To get information about a project shared with your AWS account, its ARN must be specified. You cannot specify a shared project using its name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetProjects' value with any optional fields omitted.
mkBatchGetProjects
    :: Core.NonEmpty Types.NonEmptyString -- ^ 'names'
    -> BatchGetProjects
mkBatchGetProjects names = BatchGetProjects'{names}

-- | The names or ARNs of the build projects. To get information about a project shared with your AWS account, its ARN must be specified. You cannot specify a shared project using its name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpNames :: Lens.Lens' BatchGetProjects (Core.NonEmpty Types.NonEmptyString)
bgpNames = Lens.field @"names"
{-# INLINEABLE bgpNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

instance Core.ToQuery BatchGetProjects where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetProjects where
        toHeaders BatchGetProjects{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.BatchGetProjects")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetProjects where
        toJSON BatchGetProjects{..}
          = Core.object (Core.catMaybes [Core.Just ("names" Core..= names)])

instance Core.AWSRequest BatchGetProjects where
        type Rs BatchGetProjects = BatchGetProjectsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetProjectsResponse' Core.<$>
                   (x Core..:? "projects") Core.<*> x Core..:? "projectsNotFound"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetProjectsResponse' smart constructor.
data BatchGetProjectsResponse = BatchGetProjectsResponse'
  { projects :: Core.Maybe [Types.Project]
    -- ^ Information about the requested build projects.
  , projectsNotFound :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ The names of build projects for which information could not be found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetProjectsResponse' value with any optional fields omitted.
mkBatchGetProjectsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetProjectsResponse
mkBatchGetProjectsResponse responseStatus
  = BatchGetProjectsResponse'{projects = Core.Nothing,
                              projectsNotFound = Core.Nothing, responseStatus}

-- | Information about the requested build projects.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsProjects :: Lens.Lens' BatchGetProjectsResponse (Core.Maybe [Types.Project])
bgprrsProjects = Lens.field @"projects"
{-# INLINEABLE bgprrsProjects #-}
{-# DEPRECATED projects "Use generic-lens or generic-optics with 'projects' instead"  #-}

-- | The names of build projects for which information could not be found.
--
-- /Note:/ Consider using 'projectsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsProjectsNotFound :: Lens.Lens' BatchGetProjectsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
bgprrsProjectsNotFound = Lens.field @"projectsNotFound"
{-# INLINEABLE bgprrsProjectsNotFound #-}
{-# DEPRECATED projectsNotFound "Use generic-lens or generic-optics with 'projectsNotFound' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprrsResponseStatus :: Lens.Lens' BatchGetProjectsResponse Core.Int
bgprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
