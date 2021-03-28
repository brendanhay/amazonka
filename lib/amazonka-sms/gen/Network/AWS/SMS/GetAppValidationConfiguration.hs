{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a configuration for validating an application.
module Network.AWS.SMS.GetAppValidationConfiguration
    (
    -- * Creating a request
      GetAppValidationConfiguration (..)
    , mkGetAppValidationConfiguration
    -- ** Request lenses
    , gavcAppId

    -- * Destructuring the response
    , GetAppValidationConfigurationResponse (..)
    , mkGetAppValidationConfigurationResponse
    -- ** Response lenses
    , gavcrrsAppValidationConfigurations
    , gavcrrsServerGroupValidationConfigurations
    , gavcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetAppValidationConfiguration' smart constructor.
newtype GetAppValidationConfiguration = GetAppValidationConfiguration'
  { appId :: Types.AppId
    -- ^ The ID of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppValidationConfiguration' value with any optional fields omitted.
mkGetAppValidationConfiguration
    :: Types.AppId -- ^ 'appId'
    -> GetAppValidationConfiguration
mkGetAppValidationConfiguration appId
  = GetAppValidationConfiguration'{appId}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcAppId :: Lens.Lens' GetAppValidationConfiguration Types.AppId
gavcAppId = Lens.field @"appId"
{-# INLINEABLE gavcAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

instance Core.ToQuery GetAppValidationConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAppValidationConfiguration where
        toHeaders GetAppValidationConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.GetAppValidationConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAppValidationConfiguration where
        toJSON GetAppValidationConfiguration{..}
          = Core.object (Core.catMaybes [Core.Just ("appId" Core..= appId)])

instance Core.AWSRequest GetAppValidationConfiguration where
        type Rs GetAppValidationConfiguration =
             GetAppValidationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAppValidationConfigurationResponse' Core.<$>
                   (x Core..:? "appValidationConfigurations") Core.<*>
                     x Core..:? "serverGroupValidationConfigurations"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAppValidationConfigurationResponse' smart constructor.
data GetAppValidationConfigurationResponse = GetAppValidationConfigurationResponse'
  { appValidationConfigurations :: Core.Maybe [Types.AppValidationConfiguration]
    -- ^ The configuration for application validation.
  , serverGroupValidationConfigurations :: Core.Maybe [Types.ServerGroupValidationConfiguration]
    -- ^ The configuration for instance validation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppValidationConfigurationResponse' value with any optional fields omitted.
mkGetAppValidationConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAppValidationConfigurationResponse
mkGetAppValidationConfigurationResponse responseStatus
  = GetAppValidationConfigurationResponse'{appValidationConfigurations
                                             = Core.Nothing,
                                           serverGroupValidationConfigurations = Core.Nothing,
                                           responseStatus}

-- | The configuration for application validation.
--
-- /Note:/ Consider using 'appValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrrsAppValidationConfigurations :: Lens.Lens' GetAppValidationConfigurationResponse (Core.Maybe [Types.AppValidationConfiguration])
gavcrrsAppValidationConfigurations = Lens.field @"appValidationConfigurations"
{-# INLINEABLE gavcrrsAppValidationConfigurations #-}
{-# DEPRECATED appValidationConfigurations "Use generic-lens or generic-optics with 'appValidationConfigurations' instead"  #-}

-- | The configuration for instance validation.
--
-- /Note:/ Consider using 'serverGroupValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrrsServerGroupValidationConfigurations :: Lens.Lens' GetAppValidationConfigurationResponse (Core.Maybe [Types.ServerGroupValidationConfiguration])
gavcrrsServerGroupValidationConfigurations = Lens.field @"serverGroupValidationConfigurations"
{-# INLINEABLE gavcrrsServerGroupValidationConfigurations #-}
{-# DEPRECATED serverGroupValidationConfigurations "Use generic-lens or generic-optics with 'serverGroupValidationConfigurations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrrsResponseStatus :: Lens.Lens' GetAppValidationConfigurationResponse Core.Int
gavcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gavcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
