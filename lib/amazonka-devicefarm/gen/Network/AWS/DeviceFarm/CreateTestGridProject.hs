{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTestGridProject (..)
    , mkCreateTestGridProject
    -- ** Request lenses
    , ctgpName
    , ctgpDescription

    -- * Destructuring the response
    , CreateTestGridProjectResponse (..)
    , mkCreateTestGridProjectResponse
    -- ** Response lenses
    , ctgprrsTestGridProject
    , ctgprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTestGridProject' smart constructor.
data CreateTestGridProject = CreateTestGridProject'
  { name :: Types.ResourceName
    -- ^ Human-readable name of the Selenium testing project.
  , description :: Core.Maybe Types.Description
    -- ^ Human-readable description of the project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTestGridProject' value with any optional fields omitted.
mkCreateTestGridProject
    :: Types.ResourceName -- ^ 'name'
    -> CreateTestGridProject
mkCreateTestGridProject name
  = CreateTestGridProject'{name, description = Core.Nothing}

-- | Human-readable name of the Selenium testing project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpName :: Lens.Lens' CreateTestGridProject Types.ResourceName
ctgpName = Lens.field @"name"
{-# INLINEABLE ctgpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Human-readable description of the project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpDescription :: Lens.Lens' CreateTestGridProject (Core.Maybe Types.Description)
ctgpDescription = Lens.field @"description"
{-# INLINEABLE ctgpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery CreateTestGridProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTestGridProject where
        toHeaders CreateTestGridProject{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.CreateTestGridProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTestGridProject where
        toJSON CreateTestGridProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("description" Core..=) Core.<$> description])

instance Core.AWSRequest CreateTestGridProject where
        type Rs CreateTestGridProject = CreateTestGridProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTestGridProjectResponse' Core.<$>
                   (x Core..:? "testGridProject") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTestGridProjectResponse' smart constructor.
data CreateTestGridProjectResponse = CreateTestGridProjectResponse'
  { testGridProject :: Core.Maybe Types.TestGridProject
    -- ^ ARN of the Selenium testing project that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateTestGridProjectResponse' value with any optional fields omitted.
mkCreateTestGridProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTestGridProjectResponse
mkCreateTestGridProjectResponse responseStatus
  = CreateTestGridProjectResponse'{testGridProject = Core.Nothing,
                                   responseStatus}

-- | ARN of the Selenium testing project that was created.
--
-- /Note:/ Consider using 'testGridProject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgprrsTestGridProject :: Lens.Lens' CreateTestGridProjectResponse (Core.Maybe Types.TestGridProject)
ctgprrsTestGridProject = Lens.field @"testGridProject"
{-# INLINEABLE ctgprrsTestGridProject #-}
{-# DEPRECATED testGridProject "Use generic-lens or generic-optics with 'testGridProject' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgprrsResponseStatus :: Lens.Lens' CreateTestGridProjectResponse Core.Int
ctgprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
