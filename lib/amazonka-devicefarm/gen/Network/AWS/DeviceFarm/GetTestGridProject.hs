{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Selenium testing project.
module Network.AWS.DeviceFarm.GetTestGridProject
  ( -- * Creating a request
    GetTestGridProject (..),
    mkGetTestGridProject,

    -- ** Request lenses
    gtgpProjectArn,

    -- * Destructuring the response
    GetTestGridProjectResponse (..),
    mkGetTestGridProjectResponse,

    -- ** Response lenses
    gtgprrsTestGridProject,
    gtgprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTestGridProject' smart constructor.
newtype GetTestGridProject = GetTestGridProject'
  { -- | The ARN of the Selenium testing project, from either 'CreateTestGridProject' or 'ListTestGridProjects' .
    projectArn :: Types.ProjectArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTestGridProject' value with any optional fields omitted.
mkGetTestGridProject ::
  -- | 'projectArn'
  Types.ProjectArn ->
  GetTestGridProject
mkGetTestGridProject projectArn = GetTestGridProject' {projectArn}

-- | The ARN of the Selenium testing project, from either 'CreateTestGridProject' or 'ListTestGridProjects' .
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgpProjectArn :: Lens.Lens' GetTestGridProject Types.ProjectArn
gtgpProjectArn = Lens.field @"projectArn"
{-# DEPRECATED gtgpProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

instance Core.FromJSON GetTestGridProject where
  toJSON GetTestGridProject {..} =
    Core.object
      (Core.catMaybes [Core.Just ("projectArn" Core..= projectArn)])

instance Core.AWSRequest GetTestGridProject where
  type Rs GetTestGridProject = GetTestGridProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.GetTestGridProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTestGridProjectResponse'
            Core.<$> (x Core..:? "testGridProject")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTestGridProjectResponse' smart constructor.
data GetTestGridProjectResponse = GetTestGridProjectResponse'
  { -- | A 'TestGridProject' .
    testGridProject :: Core.Maybe Types.TestGridProject,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetTestGridProjectResponse' value with any optional fields omitted.
mkGetTestGridProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTestGridProjectResponse
mkGetTestGridProjectResponse responseStatus =
  GetTestGridProjectResponse'
    { testGridProject = Core.Nothing,
      responseStatus
    }

-- | A 'TestGridProject' .
--
-- /Note:/ Consider using 'testGridProject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgprrsTestGridProject :: Lens.Lens' GetTestGridProjectResponse (Core.Maybe Types.TestGridProject)
gtgprrsTestGridProject = Lens.field @"testGridProject"
{-# DEPRECATED gtgprrsTestGridProject "Use generic-lens or generic-optics with 'testGridProject' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgprrsResponseStatus :: Lens.Lens' GetTestGridProjectResponse Core.Int
gtgprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtgprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
