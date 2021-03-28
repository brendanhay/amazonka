{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the runtime configuration of a thing.
module Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
    (
    -- * Creating a request
      UpdateThingRuntimeConfiguration (..)
    , mkUpdateThingRuntimeConfiguration
    -- ** Request lenses
    , utrcThingName
    , utrcTelemetryConfiguration

    -- * Destructuring the response
    , UpdateThingRuntimeConfigurationResponse (..)
    , mkUpdateThingRuntimeConfigurationResponse
    -- ** Response lenses
    , utrcrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateThingRuntimeConfiguration' smart constructor.
data UpdateThingRuntimeConfiguration = UpdateThingRuntimeConfiguration'
  { thingName :: Core.Text
    -- ^ The thing name.
  , telemetryConfiguration :: Core.Maybe Types.TelemetryConfigurationUpdate
    -- ^ Configuration for telemetry service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThingRuntimeConfiguration' value with any optional fields omitted.
mkUpdateThingRuntimeConfiguration
    :: Core.Text -- ^ 'thingName'
    -> UpdateThingRuntimeConfiguration
mkUpdateThingRuntimeConfiguration thingName
  = UpdateThingRuntimeConfiguration'{thingName,
                                     telemetryConfiguration = Core.Nothing}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrcThingName :: Lens.Lens' UpdateThingRuntimeConfiguration Core.Text
utrcThingName = Lens.field @"thingName"
{-# INLINEABLE utrcThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | Configuration for telemetry service.
--
-- /Note:/ Consider using 'telemetryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrcTelemetryConfiguration :: Lens.Lens' UpdateThingRuntimeConfiguration (Core.Maybe Types.TelemetryConfigurationUpdate)
utrcTelemetryConfiguration = Lens.field @"telemetryConfiguration"
{-# INLINEABLE utrcTelemetryConfiguration #-}
{-# DEPRECATED telemetryConfiguration "Use generic-lens or generic-optics with 'telemetryConfiguration' instead"  #-}

instance Core.ToQuery UpdateThingRuntimeConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateThingRuntimeConfiguration where
        toHeaders UpdateThingRuntimeConfiguration{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateThingRuntimeConfiguration where
        toJSON UpdateThingRuntimeConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("TelemetryConfiguration" Core..=) Core.<$>
                    telemetryConfiguration])

instance Core.AWSRequest UpdateThingRuntimeConfiguration where
        type Rs UpdateThingRuntimeConfiguration =
             UpdateThingRuntimeConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/greengrass/things/" Core.<> Core.toText thingName Core.<>
                             "/runtimeconfig",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateThingRuntimeConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateThingRuntimeConfigurationResponse' smart constructor.
newtype UpdateThingRuntimeConfigurationResponse = UpdateThingRuntimeConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThingRuntimeConfigurationResponse' value with any optional fields omitted.
mkUpdateThingRuntimeConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateThingRuntimeConfigurationResponse
mkUpdateThingRuntimeConfigurationResponse responseStatus
  = UpdateThingRuntimeConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrcrrsResponseStatus :: Lens.Lens' UpdateThingRuntimeConfigurationResponse Core.Int
utrcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utrcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
