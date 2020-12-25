{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetThingRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the runtime configuration of a thing.
module Network.AWS.Greengrass.GetThingRuntimeConfiguration
  ( -- * Creating a request
    GetThingRuntimeConfiguration (..),
    mkGetThingRuntimeConfiguration,

    -- ** Request lenses
    gtrcThingName,

    -- * Destructuring the response
    GetThingRuntimeConfigurationResponse (..),
    mkGetThingRuntimeConfigurationResponse,

    -- ** Response lenses
    gtrcrrsRuntimeConfiguration,
    gtrcrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetThingRuntimeConfiguration' smart constructor.
newtype GetThingRuntimeConfiguration = GetThingRuntimeConfiguration'
  { -- | The thing name.
    thingName :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetThingRuntimeConfiguration' value with any optional fields omitted.
mkGetThingRuntimeConfiguration ::
  -- | 'thingName'
  Core.Text ->
  GetThingRuntimeConfiguration
mkGetThingRuntimeConfiguration thingName =
  GetThingRuntimeConfiguration' {thingName}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrcThingName :: Lens.Lens' GetThingRuntimeConfiguration Core.Text
gtrcThingName = Lens.field @"thingName"
{-# DEPRECATED gtrcThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.AWSRequest GetThingRuntimeConfiguration where
  type
    Rs GetThingRuntimeConfiguration =
      GetThingRuntimeConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/things/" Core.<> (Core.toText thingName)
                Core.<> ("/runtimeconfig")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThingRuntimeConfigurationResponse'
            Core.<$> (x Core..:? "RuntimeConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetThingRuntimeConfigurationResponse' smart constructor.
data GetThingRuntimeConfigurationResponse = GetThingRuntimeConfigurationResponse'
  { -- | Runtime configuration for a thing.
    runtimeConfiguration :: Core.Maybe Types.RuntimeConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThingRuntimeConfigurationResponse' value with any optional fields omitted.
mkGetThingRuntimeConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetThingRuntimeConfigurationResponse
mkGetThingRuntimeConfigurationResponse responseStatus =
  GetThingRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | Runtime configuration for a thing.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrcrrsRuntimeConfiguration :: Lens.Lens' GetThingRuntimeConfigurationResponse (Core.Maybe Types.RuntimeConfiguration)
gtrcrrsRuntimeConfiguration = Lens.field @"runtimeConfiguration"
{-# DEPRECATED gtrcrrsRuntimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrcrrsResponseStatus :: Lens.Lens' GetThingRuntimeConfigurationResponse Core.Int
gtrcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
