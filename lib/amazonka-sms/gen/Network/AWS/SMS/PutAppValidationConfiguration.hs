{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.PutAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a validation configuration for the specified application.
module Network.AWS.SMS.PutAppValidationConfiguration
    (
    -- * Creating a request
      PutAppValidationConfiguration (..)
    , mkPutAppValidationConfiguration
    -- ** Request lenses
    , pavcAppId
    , pavcAppValidationConfigurations
    , pavcServerGroupValidationConfigurations

    -- * Destructuring the response
    , PutAppValidationConfigurationResponse (..)
    , mkPutAppValidationConfigurationResponse
    -- ** Response lenses
    , pavcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkPutAppValidationConfiguration' smart constructor.
data PutAppValidationConfiguration = PutAppValidationConfiguration'
  { appId :: Types.AppIdWithValidation
    -- ^ The ID of the application.
  , appValidationConfigurations :: Core.Maybe [Types.AppValidationConfiguration]
    -- ^ The configuration for application validation.
  , serverGroupValidationConfigurations :: Core.Maybe [Types.ServerGroupValidationConfiguration]
    -- ^ The configuration for instance validation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAppValidationConfiguration' value with any optional fields omitted.
mkPutAppValidationConfiguration
    :: Types.AppIdWithValidation -- ^ 'appId'
    -> PutAppValidationConfiguration
mkPutAppValidationConfiguration appId
  = PutAppValidationConfiguration'{appId,
                                   appValidationConfigurations = Core.Nothing,
                                   serverGroupValidationConfigurations = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcAppId :: Lens.Lens' PutAppValidationConfiguration Types.AppIdWithValidation
pavcAppId = Lens.field @"appId"
{-# INLINEABLE pavcAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | The configuration for application validation.
--
-- /Note:/ Consider using 'appValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcAppValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Core.Maybe [Types.AppValidationConfiguration])
pavcAppValidationConfigurations = Lens.field @"appValidationConfigurations"
{-# INLINEABLE pavcAppValidationConfigurations #-}
{-# DEPRECATED appValidationConfigurations "Use generic-lens or generic-optics with 'appValidationConfigurations' instead"  #-}

-- | The configuration for instance validation.
--
-- /Note:/ Consider using 'serverGroupValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcServerGroupValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Core.Maybe [Types.ServerGroupValidationConfiguration])
pavcServerGroupValidationConfigurations = Lens.field @"serverGroupValidationConfigurations"
{-# INLINEABLE pavcServerGroupValidationConfigurations #-}
{-# DEPRECATED serverGroupValidationConfigurations "Use generic-lens or generic-optics with 'serverGroupValidationConfigurations' instead"  #-}

instance Core.ToQuery PutAppValidationConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutAppValidationConfiguration where
        toHeaders PutAppValidationConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.PutAppValidationConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutAppValidationConfiguration where
        toJSON PutAppValidationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("appId" Core..= appId),
                  ("appValidationConfigurations" Core..=) Core.<$>
                    appValidationConfigurations,
                  ("serverGroupValidationConfigurations" Core..=) Core.<$>
                    serverGroupValidationConfigurations])

instance Core.AWSRequest PutAppValidationConfiguration where
        type Rs PutAppValidationConfiguration =
             PutAppValidationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutAppValidationConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutAppValidationConfigurationResponse' smart constructor.
newtype PutAppValidationConfigurationResponse = PutAppValidationConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutAppValidationConfigurationResponse' value with any optional fields omitted.
mkPutAppValidationConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutAppValidationConfigurationResponse
mkPutAppValidationConfigurationResponse responseStatus
  = PutAppValidationConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcrrsResponseStatus :: Lens.Lens' PutAppValidationConfigurationResponse Core.Int
pavcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pavcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
