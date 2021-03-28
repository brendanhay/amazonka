{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the launch configuration for the specified application.
module Network.AWS.SMS.DeleteAppLaunchConfiguration
    (
    -- * Creating a request
      DeleteAppLaunchConfiguration (..)
    , mkDeleteAppLaunchConfiguration
    -- ** Request lenses
    , dalcAppId

    -- * Destructuring the response
    , DeleteAppLaunchConfigurationResponse (..)
    , mkDeleteAppLaunchConfigurationResponse
    -- ** Response lenses
    , dalcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDeleteAppLaunchConfiguration' smart constructor.
newtype DeleteAppLaunchConfiguration = DeleteAppLaunchConfiguration'
  { appId :: Core.Maybe Types.AppId
    -- ^ The ID of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppLaunchConfiguration' value with any optional fields omitted.
mkDeleteAppLaunchConfiguration
    :: DeleteAppLaunchConfiguration
mkDeleteAppLaunchConfiguration
  = DeleteAppLaunchConfiguration'{appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalcAppId :: Lens.Lens' DeleteAppLaunchConfiguration (Core.Maybe Types.AppId)
dalcAppId = Lens.field @"appId"
{-# INLINEABLE dalcAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

instance Core.ToQuery DeleteAppLaunchConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAppLaunchConfiguration where
        toHeaders DeleteAppLaunchConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.DeleteAppLaunchConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAppLaunchConfiguration where
        toJSON DeleteAppLaunchConfiguration{..}
          = Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest DeleteAppLaunchConfiguration where
        type Rs DeleteAppLaunchConfiguration =
             DeleteAppLaunchConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteAppLaunchConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAppLaunchConfigurationResponse' smart constructor.
newtype DeleteAppLaunchConfigurationResponse = DeleteAppLaunchConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppLaunchConfigurationResponse' value with any optional fields omitted.
mkDeleteAppLaunchConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAppLaunchConfigurationResponse
mkDeleteAppLaunchConfigurationResponse responseStatus
  = DeleteAppLaunchConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalcrrsResponseStatus :: Lens.Lens' DeleteAppLaunchConfigurationResponse Core.Int
dalcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dalcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
