{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Selenium testing project. Projects are used to track 'TestGridSession' instances.
module Network.AWS.DeviceFarm.CreateTestGridProject
  ( -- * Creating a request
    CreateTestGridProject (..),
    mkCreateTestGridProject,

    -- ** Request lenses
    ctgpName,
    ctgpDescription,

    -- * Destructuring the response
    CreateTestGridProjectResponse (..),
    mkCreateTestGridProjectResponse,

    -- ** Response lenses
    ctgprrsTestGridProject,
    ctgprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTestGridProject' smart constructor.
data CreateTestGridProject = CreateTestGridProject'
  { -- | Human-readable name of the Selenium testing project.
    name :: Types.ResourceName,
    -- | Human-readable description of the project.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTestGridProject' value with any optional fields omitted.
mkCreateTestGridProject ::
  -- | 'name'
  Types.ResourceName ->
  CreateTestGridProject
mkCreateTestGridProject name =
  CreateTestGridProject' {name, description = Core.Nothing}

-- | Human-readable name of the Selenium testing project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpName :: Lens.Lens' CreateTestGridProject Types.ResourceName
ctgpName = Lens.field @"name"
{-# DEPRECATED ctgpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Human-readable description of the project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpDescription :: Lens.Lens' CreateTestGridProject (Core.Maybe Types.Description)
ctgpDescription = Lens.field @"description"
{-# DEPRECATED ctgpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CreateTestGridProject where
  toJSON CreateTestGridProject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest CreateTestGridProject where
  type Rs CreateTestGridProject = CreateTestGridProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.CreateTestGridProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTestGridProjectResponse'
            Core.<$> (x Core..:? "testGridProject")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTestGridProjectResponse' smart constructor.
data CreateTestGridProjectResponse = CreateTestGridProjectResponse'
  { -- | ARN of the Selenium testing project that was created.
    testGridProject :: Core.Maybe Types.TestGridProject,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTestGridProjectResponse' value with any optional fields omitted.
mkCreateTestGridProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTestGridProjectResponse
mkCreateTestGridProjectResponse responseStatus =
  CreateTestGridProjectResponse'
    { testGridProject = Core.Nothing,
      responseStatus
    }

-- | ARN of the Selenium testing project that was created.
--
-- /Note:/ Consider using 'testGridProject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgprrsTestGridProject :: Lens.Lens' CreateTestGridProjectResponse (Core.Maybe Types.TestGridProject)
ctgprrsTestGridProject = Lens.field @"testGridProject"
{-# DEPRECATED ctgprrsTestGridProject "Use generic-lens or generic-optics with 'testGridProject' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgprrsResponseStatus :: Lens.Lens' CreateTestGridProjectResponse Core.Int
ctgprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctgprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
