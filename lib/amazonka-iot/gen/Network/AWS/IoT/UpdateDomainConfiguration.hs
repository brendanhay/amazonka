{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates values stored in the domain configuration. Domain configurations for default endpoints can't be updated.
module Network.AWS.IoT.UpdateDomainConfiguration
  ( -- * Creating a request
    UpdateDomainConfiguration (..),
    mkUpdateDomainConfiguration,

    -- ** Request lenses
    udcDomainConfigurationName,
    udcAuthorizerConfig,
    udcDomainConfigurationStatus,
    udcRemoveAuthorizerConfig,

    -- * Destructuring the response
    UpdateDomainConfigurationResponse (..),
    mkUpdateDomainConfigurationResponse,

    -- ** Response lenses
    udcrrsDomainConfigurationArn,
    udcrrsDomainConfigurationName,
    udcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDomainConfiguration' smart constructor.
data UpdateDomainConfiguration = UpdateDomainConfiguration'
  { -- | The name of the domain configuration to be updated.
    domainConfigurationName :: Types.DomainConfigurationName,
    -- | An object that specifies the authorization service for a domain.
    authorizerConfig :: Core.Maybe Types.AuthorizerConfig,
    -- | The status to which the domain configuration should be updated.
    domainConfigurationStatus :: Core.Maybe Types.DomainConfigurationStatus,
    -- | Removes the authorization configuration from a domain.
    removeAuthorizerConfig :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainConfiguration' value with any optional fields omitted.
mkUpdateDomainConfiguration ::
  -- | 'domainConfigurationName'
  Types.DomainConfigurationName ->
  UpdateDomainConfiguration
mkUpdateDomainConfiguration domainConfigurationName =
  UpdateDomainConfiguration'
    { domainConfigurationName,
      authorizerConfig = Core.Nothing,
      domainConfigurationStatus = Core.Nothing,
      removeAuthorizerConfig = Core.Nothing
    }

-- | The name of the domain configuration to be updated.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDomainConfigurationName :: Lens.Lens' UpdateDomainConfiguration Types.DomainConfigurationName
udcDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# DEPRECATED udcDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | An object that specifies the authorization service for a domain.
--
-- /Note:/ Consider using 'authorizerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcAuthorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Core.Maybe Types.AuthorizerConfig)
udcAuthorizerConfig = Lens.field @"authorizerConfig"
{-# DEPRECATED udcAuthorizerConfig "Use generic-lens or generic-optics with 'authorizerConfig' instead." #-}

-- | The status to which the domain configuration should be updated.
--
-- /Note:/ Consider using 'domainConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDomainConfigurationStatus :: Lens.Lens' UpdateDomainConfiguration (Core.Maybe Types.DomainConfigurationStatus)
udcDomainConfigurationStatus = Lens.field @"domainConfigurationStatus"
{-# DEPRECATED udcDomainConfigurationStatus "Use generic-lens or generic-optics with 'domainConfigurationStatus' instead." #-}

-- | Removes the authorization configuration from a domain.
--
-- /Note:/ Consider using 'removeAuthorizerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcRemoveAuthorizerConfig :: Lens.Lens' UpdateDomainConfiguration (Core.Maybe Core.Bool)
udcRemoveAuthorizerConfig = Lens.field @"removeAuthorizerConfig"
{-# DEPRECATED udcRemoveAuthorizerConfig "Use generic-lens or generic-optics with 'removeAuthorizerConfig' instead." #-}

instance Core.FromJSON UpdateDomainConfiguration where
  toJSON UpdateDomainConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("authorizerConfig" Core..=) Core.<$> authorizerConfig,
            ("domainConfigurationStatus" Core..=)
              Core.<$> domainConfigurationStatus,
            ("removeAuthorizerConfig" Core..=)
              Core.<$> removeAuthorizerConfig
          ]
      )

instance Core.AWSRequest UpdateDomainConfiguration where
  type
    Rs UpdateDomainConfiguration =
      UpdateDomainConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/domainConfigurations/"
                Core.<> (Core.toText domainConfigurationName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainConfigurationResponse'
            Core.<$> (x Core..:? "domainConfigurationArn")
            Core.<*> (x Core..:? "domainConfigurationName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDomainConfigurationResponse' smart constructor.
data UpdateDomainConfigurationResponse = UpdateDomainConfigurationResponse'
  { -- | The ARN of the domain configuration that was updated.
    domainConfigurationArn :: Core.Maybe Types.DomainConfigurationArn,
    -- | The name of the domain configuration that was updated.
    domainConfigurationName :: Core.Maybe Types.DomainConfigurationName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainConfigurationResponse' value with any optional fields omitted.
mkUpdateDomainConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDomainConfigurationResponse
mkUpdateDomainConfigurationResponse responseStatus =
  UpdateDomainConfigurationResponse'
    { domainConfigurationArn =
        Core.Nothing,
      domainConfigurationName = Core.Nothing,
      responseStatus
    }

-- | The ARN of the domain configuration that was updated.
--
-- /Note:/ Consider using 'domainConfigurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrrsDomainConfigurationArn :: Lens.Lens' UpdateDomainConfigurationResponse (Core.Maybe Types.DomainConfigurationArn)
udcrrsDomainConfigurationArn = Lens.field @"domainConfigurationArn"
{-# DEPRECATED udcrrsDomainConfigurationArn "Use generic-lens or generic-optics with 'domainConfigurationArn' instead." #-}

-- | The name of the domain configuration that was updated.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrrsDomainConfigurationName :: Lens.Lens' UpdateDomainConfigurationResponse (Core.Maybe Types.DomainConfigurationName)
udcrrsDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# DEPRECATED udcrrsDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrrsResponseStatus :: Lens.Lens' UpdateDomainConfigurationResponse Core.Int
udcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
