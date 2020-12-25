{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetOTAUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an OTA update.
module Network.AWS.IoT.GetOTAUpdate
  ( -- * Creating a request
    GetOTAUpdate (..),
    mkGetOTAUpdate,

    -- ** Request lenses
    gotauOtaUpdateId,

    -- * Destructuring the response
    GetOTAUpdateResponse (..),
    mkGetOTAUpdateResponse,

    -- ** Response lenses
    gotaurrsOtaUpdateInfo,
    gotaurrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOTAUpdate' smart constructor.
newtype GetOTAUpdate = GetOTAUpdate'
  { -- | The OTA update ID.
    otaUpdateId :: Types.OtaUpdateId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOTAUpdate' value with any optional fields omitted.
mkGetOTAUpdate ::
  -- | 'otaUpdateId'
  Types.OtaUpdateId ->
  GetOTAUpdate
mkGetOTAUpdate otaUpdateId = GetOTAUpdate' {otaUpdateId}

-- | The OTA update ID.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotauOtaUpdateId :: Lens.Lens' GetOTAUpdate Types.OtaUpdateId
gotauOtaUpdateId = Lens.field @"otaUpdateId"
{-# DEPRECATED gotauOtaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead." #-}

instance Core.AWSRequest GetOTAUpdate where
  type Rs GetOTAUpdate = GetOTAUpdateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/otaUpdates/" Core.<> (Core.toText otaUpdateId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOTAUpdateResponse'
            Core.<$> (x Core..:? "otaUpdateInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetOTAUpdateResponse' smart constructor.
data GetOTAUpdateResponse = GetOTAUpdateResponse'
  { -- | The OTA update info.
    otaUpdateInfo :: Core.Maybe Types.OTAUpdateInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetOTAUpdateResponse' value with any optional fields omitted.
mkGetOTAUpdateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetOTAUpdateResponse
mkGetOTAUpdateResponse responseStatus =
  GetOTAUpdateResponse'
    { otaUpdateInfo = Core.Nothing,
      responseStatus
    }

-- | The OTA update info.
--
-- /Note:/ Consider using 'otaUpdateInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotaurrsOtaUpdateInfo :: Lens.Lens' GetOTAUpdateResponse (Core.Maybe Types.OTAUpdateInfo)
gotaurrsOtaUpdateInfo = Lens.field @"otaUpdateInfo"
{-# DEPRECATED gotaurrsOtaUpdateInfo "Use generic-lens or generic-optics with 'otaUpdateInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotaurrsResponseStatus :: Lens.Lens' GetOTAUpdateResponse Core.Int
gotaurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gotaurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
