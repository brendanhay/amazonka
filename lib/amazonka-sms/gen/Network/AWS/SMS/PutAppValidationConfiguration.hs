{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutAppValidationConfiguration (..),
    mkPutAppValidationConfiguration,

    -- ** Request lenses
    pavcAppId,
    pavcAppValidationConfigurations,
    pavcServerGroupValidationConfigurations,

    -- * Destructuring the response
    PutAppValidationConfigurationResponse (..),
    mkPutAppValidationConfigurationResponse,

    -- ** Response lenses
    pavcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkPutAppValidationConfiguration' smart constructor.
data PutAppValidationConfiguration = PutAppValidationConfiguration'
  { -- | The ID of the application.
    appId :: Types.AppIdWithValidation,
    -- | The configuration for application validation.
    appValidationConfigurations :: Core.Maybe [Types.AppValidationConfiguration],
    -- | The configuration for instance validation.
    serverGroupValidationConfigurations :: Core.Maybe [Types.ServerGroupValidationConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAppValidationConfiguration' value with any optional fields omitted.
mkPutAppValidationConfiguration ::
  -- | 'appId'
  Types.AppIdWithValidation ->
  PutAppValidationConfiguration
mkPutAppValidationConfiguration appId =
  PutAppValidationConfiguration'
    { appId,
      appValidationConfigurations = Core.Nothing,
      serverGroupValidationConfigurations = Core.Nothing
    }

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcAppId :: Lens.Lens' PutAppValidationConfiguration Types.AppIdWithValidation
pavcAppId = Lens.field @"appId"
{-# DEPRECATED pavcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The configuration for application validation.
--
-- /Note:/ Consider using 'appValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcAppValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Core.Maybe [Types.AppValidationConfiguration])
pavcAppValidationConfigurations = Lens.field @"appValidationConfigurations"
{-# DEPRECATED pavcAppValidationConfigurations "Use generic-lens or generic-optics with 'appValidationConfigurations' instead." #-}

-- | The configuration for instance validation.
--
-- /Note:/ Consider using 'serverGroupValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcServerGroupValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Core.Maybe [Types.ServerGroupValidationConfiguration])
pavcServerGroupValidationConfigurations = Lens.field @"serverGroupValidationConfigurations"
{-# DEPRECATED pavcServerGroupValidationConfigurations "Use generic-lens or generic-optics with 'serverGroupValidationConfigurations' instead." #-}

instance Core.FromJSON PutAppValidationConfiguration where
  toJSON PutAppValidationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("appId" Core..= appId),
            ("appValidationConfigurations" Core..=)
              Core.<$> appValidationConfigurations,
            ("serverGroupValidationConfigurations" Core..=)
              Core.<$> serverGroupValidationConfigurations
          ]
      )

instance Core.AWSRequest PutAppValidationConfiguration where
  type
    Rs PutAppValidationConfiguration =
      PutAppValidationConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.PutAppValidationConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppValidationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutAppValidationConfigurationResponse' smart constructor.
newtype PutAppValidationConfigurationResponse = PutAppValidationConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutAppValidationConfigurationResponse' value with any optional fields omitted.
mkPutAppValidationConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutAppValidationConfigurationResponse
mkPutAppValidationConfigurationResponse responseStatus =
  PutAppValidationConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pavcrrsResponseStatus :: Lens.Lens' PutAppValidationConfigurationResponse Core.Int
pavcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pavcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
