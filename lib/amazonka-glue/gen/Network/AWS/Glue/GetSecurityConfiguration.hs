{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified security configuration.
module Network.AWS.Glue.GetSecurityConfiguration
  ( -- * Creating a request
    GetSecurityConfiguration (..),
    mkGetSecurityConfiguration,

    -- ** Request lenses
    gscName,

    -- * Destructuring the response
    GetSecurityConfigurationResponse (..),
    mkGetSecurityConfigurationResponse,

    -- ** Response lenses
    gscrrsSecurityConfiguration,
    gscrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSecurityConfiguration' smart constructor.
newtype GetSecurityConfiguration = GetSecurityConfiguration'
  { -- | The name of the security configuration to retrieve.
    name :: Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSecurityConfiguration' value with any optional fields omitted.
mkGetSecurityConfiguration ::
  -- | 'name'
  Types.NameString ->
  GetSecurityConfiguration
mkGetSecurityConfiguration name = GetSecurityConfiguration' {name}

-- | The name of the security configuration to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscName :: Lens.Lens' GetSecurityConfiguration Types.NameString
gscName = Lens.field @"name"
{-# DEPRECATED gscName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GetSecurityConfiguration where
  toJSON GetSecurityConfiguration {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetSecurityConfiguration where
  type Rs GetSecurityConfiguration = GetSecurityConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetSecurityConfiguration")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationResponse'
            Core.<$> (x Core..:? "SecurityConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSecurityConfigurationResponse' smart constructor.
data GetSecurityConfigurationResponse = GetSecurityConfigurationResponse'
  { -- | The requested security configuration.
    securityConfiguration :: Core.Maybe Types.SecurityConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSecurityConfigurationResponse' value with any optional fields omitted.
mkGetSecurityConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSecurityConfigurationResponse
mkGetSecurityConfigurationResponse responseStatus =
  GetSecurityConfigurationResponse'
    { securityConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | The requested security configuration.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsSecurityConfiguration :: Lens.Lens' GetSecurityConfigurationResponse (Core.Maybe Types.SecurityConfiguration)
gscrrsSecurityConfiguration = Lens.field @"securityConfiguration"
{-# DEPRECATED gscrrsSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsResponseStatus :: Lens.Lens' GetSecurityConfigurationResponse Core.Int
gscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
