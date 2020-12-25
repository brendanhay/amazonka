{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApplicationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the settings for an application.
module Network.AWS.Pinpoint.GetApplicationSettings
  ( -- * Creating a request
    GetApplicationSettings (..),
    mkGetApplicationSettings,

    -- ** Request lenses
    gasApplicationId,

    -- * Destructuring the response
    GetApplicationSettingsResponse (..),
    mkGetApplicationSettingsResponse,

    -- ** Response lenses
    gasrrsApplicationSettingsResource,
    gasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApplicationSettings' smart constructor.
newtype GetApplicationSettings = GetApplicationSettings'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplicationSettings' value with any optional fields omitted.
mkGetApplicationSettings ::
  -- | 'applicationId'
  Core.Text ->
  GetApplicationSettings
mkGetApplicationSettings applicationId =
  GetApplicationSettings' {applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasApplicationId :: Lens.Lens' GetApplicationSettings Core.Text
gasApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gasApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest GetApplicationSettings where
  type Rs GetApplicationSettings = GetApplicationSettingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/settings")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationSettingsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetApplicationSettingsResponse' smart constructor.
data GetApplicationSettingsResponse = GetApplicationSettingsResponse'
  { applicationSettingsResource :: Types.ApplicationSettingsResource,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplicationSettingsResponse' value with any optional fields omitted.
mkGetApplicationSettingsResponse ::
  -- | 'applicationSettingsResource'
  Types.ApplicationSettingsResource ->
  -- | 'responseStatus'
  Core.Int ->
  GetApplicationSettingsResponse
mkGetApplicationSettingsResponse
  applicationSettingsResource
  responseStatus =
    GetApplicationSettingsResponse'
      { applicationSettingsResource,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationSettingsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsApplicationSettingsResource :: Lens.Lens' GetApplicationSettingsResponse Types.ApplicationSettingsResource
gasrrsApplicationSettingsResource = Lens.field @"applicationSettingsResource"
{-# DEPRECATED gasrrsApplicationSettingsResource "Use generic-lens or generic-optics with 'applicationSettingsResource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetApplicationSettingsResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
