{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTestGridProject (..)
    , mkGetTestGridProject
    -- ** Request lenses
    , gtgpProjectArn

    -- * Destructuring the response
    , GetTestGridProjectResponse (..)
    , mkGetTestGridProjectResponse
    -- ** Response lenses
    , gtgprrsTestGridProject
    , gtgprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTestGridProject' smart constructor.
newtype GetTestGridProject = GetTestGridProject'
  { projectArn :: Types.ProjectArn
    -- ^ The ARN of the Selenium testing project, from either 'CreateTestGridProject' or 'ListTestGridProjects' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTestGridProject' value with any optional fields omitted.
mkGetTestGridProject
    :: Types.ProjectArn -- ^ 'projectArn'
    -> GetTestGridProject
mkGetTestGridProject projectArn = GetTestGridProject'{projectArn}

-- | The ARN of the Selenium testing project, from either 'CreateTestGridProject' or 'ListTestGridProjects' .
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgpProjectArn :: Lens.Lens' GetTestGridProject Types.ProjectArn
gtgpProjectArn = Lens.field @"projectArn"
{-# INLINEABLE gtgpProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

instance Core.ToQuery GetTestGridProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTestGridProject where
        toHeaders GetTestGridProject{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.GetTestGridProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTestGridProject where
        toJSON GetTestGridProject{..}
          = Core.object
              (Core.catMaybes [Core.Just ("projectArn" Core..= projectArn)])

instance Core.AWSRequest GetTestGridProject where
        type Rs GetTestGridProject = GetTestGridProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTestGridProjectResponse' Core.<$>
                   (x Core..:? "testGridProject") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTestGridProjectResponse' smart constructor.
data GetTestGridProjectResponse = GetTestGridProjectResponse'
  { testGridProject :: Core.Maybe Types.TestGridProject
    -- ^ A 'TestGridProject' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTestGridProjectResponse' value with any optional fields omitted.
mkGetTestGridProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTestGridProjectResponse
mkGetTestGridProjectResponse responseStatus
  = GetTestGridProjectResponse'{testGridProject = Core.Nothing,
                                responseStatus}

-- | A 'TestGridProject' .
--
-- /Note:/ Consider using 'testGridProject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgprrsTestGridProject :: Lens.Lens' GetTestGridProjectResponse (Core.Maybe Types.TestGridProject)
gtgprrsTestGridProject = Lens.field @"testGridProject"
{-# INLINEABLE gtgprrsTestGridProject #-}
{-# DEPRECATED testGridProject "Use generic-lens or generic-optics with 'testGridProject' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgprrsResponseStatus :: Lens.Lens' GetTestGridProjectResponse Core.Int
gtgprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtgprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
