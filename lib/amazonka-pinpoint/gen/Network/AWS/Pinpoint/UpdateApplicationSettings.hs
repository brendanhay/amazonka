{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateApplicationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for an application.
module Network.AWS.Pinpoint.UpdateApplicationSettings
  ( -- * Creating a request
    UpdateApplicationSettings (..),
    mkUpdateApplicationSettings,

    -- ** Request lenses
    uasApplicationId,
    uasWriteApplicationSettingsRequest,

    -- * Destructuring the response
    UpdateApplicationSettingsResponse (..),
    mkUpdateApplicationSettingsResponse,

    -- ** Response lenses
    uasrrsApplicationSettingsResource,
    uasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApplicationSettings' smart constructor.
data UpdateApplicationSettings = UpdateApplicationSettings'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    writeApplicationSettingsRequest :: Types.WriteApplicationSettingsRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationSettings' value with any optional fields omitted.
mkUpdateApplicationSettings ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'writeApplicationSettingsRequest'
  Types.WriteApplicationSettingsRequest ->
  UpdateApplicationSettings
mkUpdateApplicationSettings
  applicationId
  writeApplicationSettingsRequest =
    UpdateApplicationSettings'
      { applicationId,
        writeApplicationSettingsRequest
      }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasApplicationId :: Lens.Lens' UpdateApplicationSettings Core.Text
uasApplicationId = Lens.field @"applicationId"
{-# DEPRECATED uasApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeApplicationSettingsRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasWriteApplicationSettingsRequest :: Lens.Lens' UpdateApplicationSettings Types.WriteApplicationSettingsRequest
uasWriteApplicationSettingsRequest = Lens.field @"writeApplicationSettingsRequest"
{-# DEPRECATED uasWriteApplicationSettingsRequest "Use generic-lens or generic-optics with 'writeApplicationSettingsRequest' instead." #-}

instance Core.FromJSON UpdateApplicationSettings where
  toJSON UpdateApplicationSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "WriteApplicationSettingsRequest"
                  Core..= writeApplicationSettingsRequest
              )
          ]
      )

instance Core.AWSRequest UpdateApplicationSettings where
  type
    Rs UpdateApplicationSettings =
      UpdateApplicationSettingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/settings")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationSettingsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateApplicationSettingsResponse' smart constructor.
data UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse'
  { applicationSettingsResource :: Types.ApplicationSettingsResource,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationSettingsResponse' value with any optional fields omitted.
mkUpdateApplicationSettingsResponse ::
  -- | 'applicationSettingsResource'
  Types.ApplicationSettingsResource ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateApplicationSettingsResponse
mkUpdateApplicationSettingsResponse
  applicationSettingsResource
  responseStatus =
    UpdateApplicationSettingsResponse'
      { applicationSettingsResource,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationSettingsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsApplicationSettingsResource :: Lens.Lens' UpdateApplicationSettingsResponse Types.ApplicationSettingsResource
uasrrsApplicationSettingsResource = Lens.field @"applicationSettingsResource"
{-# DEPRECATED uasrrsApplicationSettingsResource "Use generic-lens or generic-optics with 'applicationSettingsResource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsResponseStatus :: Lens.Lens' UpdateApplicationSettingsResponse Core.Int
uasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
