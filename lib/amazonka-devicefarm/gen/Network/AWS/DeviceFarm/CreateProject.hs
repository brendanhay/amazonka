{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project.
module Network.AWS.DeviceFarm.CreateProject
    (
    -- * Creating a request
      CreateProject (..)
    , mkCreateProject
    -- ** Request lenses
    , cpName
    , cpDefaultJobTimeoutMinutes

    -- * Destructuring the response
    , CreateProjectResponse (..)
    , mkCreateProjectResponse
    -- ** Response lenses
    , cprrsProject
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the create project operation.
--
-- /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { name :: Types.Name
    -- ^ The project's name.
  , defaultJobTimeoutMinutes :: Core.Maybe Core.Int
    -- ^ Sets the execution timeout value (in minutes) for a project. All test runs in this project use the specified execution timeout value unless overridden when scheduling a run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProject' value with any optional fields omitted.
mkCreateProject
    :: Types.Name -- ^ 'name'
    -> CreateProject
mkCreateProject name
  = CreateProject'{name, defaultJobTimeoutMinutes = Core.Nothing}

-- | The project's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject Types.Name
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Sets the execution timeout value (in minutes) for a project. All test runs in this project use the specified execution timeout value unless overridden when scheduling a run.
--
-- /Note:/ Consider using 'defaultJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDefaultJobTimeoutMinutes :: Lens.Lens' CreateProject (Core.Maybe Core.Int)
cpDefaultJobTimeoutMinutes = Lens.field @"defaultJobTimeoutMinutes"
{-# INLINEABLE cpDefaultJobTimeoutMinutes #-}
{-# DEPRECATED defaultJobTimeoutMinutes "Use generic-lens or generic-optics with 'defaultJobTimeoutMinutes' instead"  #-}

instance Core.ToQuery CreateProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProject where
        toHeaders CreateProject{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.CreateProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProject where
        toJSON CreateProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("defaultJobTimeoutMinutes" Core..=) Core.<$>
                    defaultJobTimeoutMinutes])

instance Core.AWSRequest CreateProject where
        type Rs CreateProject = CreateProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProjectResponse' Core.<$>
                   (x Core..:? "project") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a create project request.
--
-- /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { project :: Core.Maybe Types.Project
    -- ^ The newly created project.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateProjectResponse' value with any optional fields omitted.
mkCreateProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProjectResponse
mkCreateProjectResponse responseStatus
  = CreateProjectResponse'{project = Core.Nothing, responseStatus}

-- | The newly created project.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProject :: Lens.Lens' CreateProjectResponse (Core.Maybe Types.Project)
cprrsProject = Lens.field @"project"
{-# INLINEABLE cprrsProject #-}
{-# DEPRECATED project "Use generic-lens or generic-optics with 'project' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProjectResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
