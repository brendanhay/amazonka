{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the indexing configuration.
module Network.AWS.IoT.GetIndexingConfiguration
    (
    -- * Creating a request
      GetIndexingConfiguration (..)
    , mkGetIndexingConfiguration

    -- * Destructuring the response
    , GetIndexingConfigurationResponse (..)
    , mkGetIndexingConfigurationResponse
    -- ** Response lenses
    , gicrrsThingGroupIndexingConfiguration
    , gicrrsThingIndexingConfiguration
    , gicrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIndexingConfiguration' smart constructor.
data GetIndexingConfiguration = GetIndexingConfiguration'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIndexingConfiguration' value with any optional fields omitted.
mkGetIndexingConfiguration
    :: GetIndexingConfiguration
mkGetIndexingConfiguration = GetIndexingConfiguration'

instance Core.ToQuery GetIndexingConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetIndexingConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetIndexingConfiguration where
        type Rs GetIndexingConfiguration = GetIndexingConfigurationResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/indexing/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetIndexingConfigurationResponse' Core.<$>
                   (x Core..:? "thingGroupIndexingConfiguration") Core.<*>
                     x Core..:? "thingIndexingConfiguration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetIndexingConfigurationResponse' smart constructor.
data GetIndexingConfigurationResponse = GetIndexingConfigurationResponse'
  { thingGroupIndexingConfiguration :: Core.Maybe Types.ThingGroupIndexingConfiguration
    -- ^ The index configuration.
  , thingIndexingConfiguration :: Core.Maybe Types.ThingIndexingConfiguration
    -- ^ Thing indexing configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIndexingConfigurationResponse' value with any optional fields omitted.
mkGetIndexingConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetIndexingConfigurationResponse
mkGetIndexingConfigurationResponse responseStatus
  = GetIndexingConfigurationResponse'{thingGroupIndexingConfiguration
                                        = Core.Nothing,
                                      thingIndexingConfiguration = Core.Nothing, responseStatus}

-- | The index configuration.
--
-- /Note:/ Consider using 'thingGroupIndexingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsThingGroupIndexingConfiguration :: Lens.Lens' GetIndexingConfigurationResponse (Core.Maybe Types.ThingGroupIndexingConfiguration)
gicrrsThingGroupIndexingConfiguration = Lens.field @"thingGroupIndexingConfiguration"
{-# INLINEABLE gicrrsThingGroupIndexingConfiguration #-}
{-# DEPRECATED thingGroupIndexingConfiguration "Use generic-lens or generic-optics with 'thingGroupIndexingConfiguration' instead"  #-}

-- | Thing indexing configuration.
--
-- /Note:/ Consider using 'thingIndexingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsThingIndexingConfiguration :: Lens.Lens' GetIndexingConfigurationResponse (Core.Maybe Types.ThingIndexingConfiguration)
gicrrsThingIndexingConfiguration = Lens.field @"thingIndexingConfiguration"
{-# INLINEABLE gicrrsThingIndexingConfiguration #-}
{-# DEPRECATED thingIndexingConfiguration "Use generic-lens or generic-optics with 'thingIndexingConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsResponseStatus :: Lens.Lens' GetIndexingConfigurationResponse Core.Int
gicrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gicrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
