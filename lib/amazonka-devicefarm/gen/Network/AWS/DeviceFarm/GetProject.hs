{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a project.
module Network.AWS.DeviceFarm.GetProject
    (
    -- * Creating a request
      GetProject (..)
    , mkGetProject
    -- ** Request lenses
    , gpArn

    -- * Destructuring the response
    , GetProjectResponse (..)
    , mkGetProjectResponse
    -- ** Response lenses
    , gprrsProject
    , gprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get project operation.
--
-- /See:/ 'mkGetProject' smart constructor.
newtype GetProject = GetProject'
  { arn :: Types.Arn
    -- ^ The project's ARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetProject' value with any optional fields omitted.
mkGetProject
    :: Types.Arn -- ^ 'arn'
    -> GetProject
mkGetProject arn = GetProject'{arn}

-- | The project's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpArn :: Lens.Lens' GetProject Types.Arn
gpArn = Lens.field @"arn"
{-# INLINEABLE gpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetProject where
        toHeaders GetProject{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetProject where
        toJSON GetProject{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetProject where
        type Rs GetProject = GetProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetProjectResponse' Core.<$>
                   (x Core..:? "project") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a get project request.
--
-- /See:/ 'mkGetProjectResponse' smart constructor.
data GetProjectResponse = GetProjectResponse'
  { project :: Core.Maybe Types.Project
    -- ^ The project to get information about.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetProjectResponse' value with any optional fields omitted.
mkGetProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetProjectResponse
mkGetProjectResponse responseStatus
  = GetProjectResponse'{project = Core.Nothing, responseStatus}

-- | The project to get information about.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsProject :: Lens.Lens' GetProjectResponse (Core.Maybe Types.Project)
gprrsProject = Lens.field @"project"
{-# INLINEABLE gprrsProject #-}
{-# DEPRECATED project "Use generic-lens or generic-optics with 'project' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetProjectResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
