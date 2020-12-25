{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetLoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'LoggingConfiguration' for the specified web ACL.
module Network.AWS.WAFRegional.GetLoggingConfiguration
  ( -- * Creating a request
    GetLoggingConfiguration (..),
    mkGetLoggingConfiguration,

    -- ** Request lenses
    glcResourceArn,

    -- * Destructuring the response
    GetLoggingConfigurationResponse (..),
    mkGetLoggingConfigurationResponse,

    -- ** Response lenses
    glcrrsLoggingConfiguration,
    glcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetLoggingConfiguration' smart constructor.
newtype GetLoggingConfiguration = GetLoggingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the web ACL for which you want to get the 'LoggingConfiguration' .
    resourceArn :: Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggingConfiguration' value with any optional fields omitted.
mkGetLoggingConfiguration ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  GetLoggingConfiguration
mkGetLoggingConfiguration resourceArn =
  GetLoggingConfiguration' {resourceArn}

-- | The Amazon Resource Name (ARN) of the web ACL for which you want to get the 'LoggingConfiguration' .
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcResourceArn :: Lens.Lens' GetLoggingConfiguration Types.ResourceArn
glcResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED glcResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON GetLoggingConfiguration where
  toJSON GetLoggingConfiguration {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetLoggingConfiguration where
  type Rs GetLoggingConfiguration = GetLoggingConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSWAF_Regional_20161128.GetLoggingConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggingConfigurationResponse'
            Core.<$> (x Core..:? "LoggingConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLoggingConfigurationResponse' smart constructor.
data GetLoggingConfigurationResponse = GetLoggingConfigurationResponse'
  { -- | The 'LoggingConfiguration' for the specified web ACL.
    loggingConfiguration :: Core.Maybe Types.LoggingConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggingConfigurationResponse' value with any optional fields omitted.
mkGetLoggingConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLoggingConfigurationResponse
mkGetLoggingConfigurationResponse responseStatus =
  GetLoggingConfigurationResponse'
    { loggingConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | The 'LoggingConfiguration' for the specified web ACL.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcrrsLoggingConfiguration :: Lens.Lens' GetLoggingConfigurationResponse (Core.Maybe Types.LoggingConfiguration)
glcrrsLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# DEPRECATED glcrrsLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcrrsResponseStatus :: Lens.Lens' GetLoggingConfigurationResponse Core.Int
glcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
