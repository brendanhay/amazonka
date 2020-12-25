{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetAggregateResourceConfig (..),
    mkGetAggregateResourceConfig,

    -- ** Request lenses
    garcConfigurationAggregatorName,
    garcResourceIdentifier,

    -- * Destructuring the response
    GetAggregateResourceConfigResponse (..),
    mkGetAggregateResourceConfigResponse,

    -- ** Response lenses
    garcrrsConfigurationItem,
    garcrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAggregateResourceConfig' smart constructor.
data GetAggregateResourceConfig = GetAggregateResourceConfig'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Types.ConfigurationAggregatorName,
    -- | An object that identifies aggregate resource.
    resourceIdentifier :: Types.AggregateResourceIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAggregateResourceConfig' value with any optional fields omitted.
mkGetAggregateResourceConfig ::
  -- | 'configurationAggregatorName'
  Types.ConfigurationAggregatorName ->
  -- | 'resourceIdentifier'
  Types.AggregateResourceIdentifier ->
  GetAggregateResourceConfig
mkGetAggregateResourceConfig
  configurationAggregatorName
  resourceIdentifier =
    GetAggregateResourceConfig'
      { configurationAggregatorName,
        resourceIdentifier
      }

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcConfigurationAggregatorName :: Lens.Lens' GetAggregateResourceConfig Types.ConfigurationAggregatorName
garcConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# DEPRECATED garcConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | An object that identifies aggregate resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcResourceIdentifier :: Lens.Lens' GetAggregateResourceConfig Types.AggregateResourceIdentifier
garcResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED garcResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Core.FromJSON GetAggregateResourceConfig where
  toJSON GetAggregateResourceConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            Core.Just ("ResourceIdentifier" Core..= resourceIdentifier)
          ]
      )

instance Core.AWSRequest GetAggregateResourceConfig where
  type
    Rs GetAggregateResourceConfig =
      GetAggregateResourceConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.GetAggregateResourceConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateResourceConfigResponse'
            Core.<$> (x Core..:? "ConfigurationItem")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAggregateResourceConfigResponse' smart constructor.
data GetAggregateResourceConfigResponse = GetAggregateResourceConfigResponse'
  { -- | Returns a @ConfigurationItem@ object.
    configurationItem :: Core.Maybe Types.ConfigurationItem,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetAggregateResourceConfigResponse' value with any optional fields omitted.
mkGetAggregateResourceConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAggregateResourceConfigResponse
mkGetAggregateResourceConfigResponse responseStatus =
  GetAggregateResourceConfigResponse'
    { configurationItem =
        Core.Nothing,
      responseStatus
    }

-- | Returns a @ConfigurationItem@ object.
--
-- /Note:/ Consider using 'configurationItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrrsConfigurationItem :: Lens.Lens' GetAggregateResourceConfigResponse (Core.Maybe Types.ConfigurationItem)
garcrrsConfigurationItem = Lens.field @"configurationItem"
{-# DEPRECATED garcrrsConfigurationItem "Use generic-lens or generic-optics with 'configurationItem' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garcrrsResponseStatus :: Lens.Lens' GetAggregateResourceConfigResponse Core.Int
garcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED garcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
