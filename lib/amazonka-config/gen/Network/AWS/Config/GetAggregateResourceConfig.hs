{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetAggregateResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns configuration item that is aggregated for your specific resource in a specific source account and region.
module Network.AWS.Config.GetAggregateResourceConfig
    (
    -- * Creating a request
      GetAggregateResourceConfig (..)
    , mkGetAggregateResourceConfig
    -- ** Request lenses
    , garcConfigurationAggregatorName
    , garcResourceIdentifier

    -- * Destructuring the response
    , GetAggregateResourceConfigResponse (..)
    , mkGetAggregateResourceConfigResponse
    -- ** Response lenses
    , garcrrsConfigurationItem
    , garcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAggregateResourceConfig' smart constructor.
data GetAggregateResourceConfig = GetAggregateResourceConfig'
  { configurationAggregatorName :: Types.ConfigurationAggregatorName
    -- ^ The name of the configuration aggregator.
  , resourceIdentifier :: Types.AggregateResourceIdentifier
    -- ^ An object that identifies aggregate resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAggregateResourceConfig' value with any optional fields omitted.
mkGetAggregateResourceConfig
    :: Types.ConfigurationAggregatorName -- ^ 'configurationAggregatorName'
    -> Types.AggregateResourceIdentifier -- ^ 'resourceIdentifier'
    -> GetAggregateResourceConfig
mkGetAggregateResourceConfig configurationAggregatorName
  resourceIdentifier
  = GetAggregateResourceConfig'{configurationAggregatorName,
                                resourceIdentifier}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcConfigurationAggregatorName :: Lens.Lens' GetAggregateResourceConfig Types.ConfigurationAggregatorName
garcConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# INLINEABLE garcConfigurationAggregatorName #-}
{-# DEPRECATED configurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead"  #-}

-- | An object that identifies aggregate resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcResourceIdentifier :: Lens.Lens' GetAggregateResourceConfig Types.AggregateResourceIdentifier
garcResourceIdentifier = Lens.field @"resourceIdentifier"
{-# INLINEABLE garcResourceIdentifier #-}
{-# DEPRECATED resourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead"  #-}

instance Core.ToQuery GetAggregateResourceConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAggregateResourceConfig where
        toHeaders GetAggregateResourceConfig{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.GetAggregateResourceConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAggregateResourceConfig where
        toJSON GetAggregateResourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConfigurationAggregatorName" Core..=
                       configurationAggregatorName),
                  Core.Just ("ResourceIdentifier" Core..= resourceIdentifier)])

instance Core.AWSRequest GetAggregateResourceConfig where
        type Rs GetAggregateResourceConfig =
             GetAggregateResourceConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAggregateResourceConfigResponse' Core.<$>
                   (x Core..:? "ConfigurationItem") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAggregateResourceConfigResponse' smart constructor.
data GetAggregateResourceConfigResponse = GetAggregateResourceConfigResponse'
  { configurationItem :: Core.Maybe Types.ConfigurationItem
    -- ^ Returns a @ConfigurationItem@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAggregateResourceConfigResponse' value with any optional fields omitted.
mkGetAggregateResourceConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAggregateResourceConfigResponse
mkGetAggregateResourceConfigResponse responseStatus
  = GetAggregateResourceConfigResponse'{configurationItem =
                                          Core.Nothing,
                                        responseStatus}

-- | Returns a @ConfigurationItem@ object.
--
-- /Note:/ Consider using 'configurationItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrrsConfigurationItem :: Lens.Lens' GetAggregateResourceConfigResponse (Core.Maybe Types.ConfigurationItem)
garcrrsConfigurationItem = Lens.field @"configurationItem"
{-# INLINEABLE garcrrsConfigurationItem #-}
{-# DEPRECATED configurationItem "Use generic-lens or generic-optics with 'configurationItem' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrrsResponseStatus :: Lens.Lens' GetAggregateResourceConfigResponse Core.Int
garcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
