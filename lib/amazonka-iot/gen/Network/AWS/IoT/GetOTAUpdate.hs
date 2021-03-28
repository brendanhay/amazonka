{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetOTAUpdate (..)
    , mkGetOTAUpdate
    -- ** Request lenses
    , gotauOtaUpdateId

    -- * Destructuring the response
    , GetOTAUpdateResponse (..)
    , mkGetOTAUpdateResponse
    -- ** Response lenses
    , gotaurrsOtaUpdateInfo
    , gotaurrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOTAUpdate' smart constructor.
newtype GetOTAUpdate = GetOTAUpdate'
  { otaUpdateId :: Types.OtaUpdateId
    -- ^ The OTA update ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOTAUpdate' value with any optional fields omitted.
mkGetOTAUpdate
    :: Types.OtaUpdateId -- ^ 'otaUpdateId'
    -> GetOTAUpdate
mkGetOTAUpdate otaUpdateId = GetOTAUpdate'{otaUpdateId}

-- | The OTA update ID.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotauOtaUpdateId :: Lens.Lens' GetOTAUpdate Types.OtaUpdateId
gotauOtaUpdateId = Lens.field @"otaUpdateId"
{-# INLINEABLE gotauOtaUpdateId #-}
{-# DEPRECATED otaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead"  #-}

instance Core.ToQuery GetOTAUpdate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOTAUpdate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetOTAUpdate where
        type Rs GetOTAUpdate = GetOTAUpdateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/otaUpdates/" Core.<> Core.toText otaUpdateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOTAUpdateResponse' Core.<$>
                   (x Core..:? "otaUpdateInfo") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetOTAUpdateResponse' smart constructor.
data GetOTAUpdateResponse = GetOTAUpdateResponse'
  { otaUpdateInfo :: Core.Maybe Types.OTAUpdateInfo
    -- ^ The OTA update info.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetOTAUpdateResponse' value with any optional fields omitted.
mkGetOTAUpdateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOTAUpdateResponse
mkGetOTAUpdateResponse responseStatus
  = GetOTAUpdateResponse'{otaUpdateInfo = Core.Nothing,
                          responseStatus}

-- | The OTA update info.
--
-- /Note:/ Consider using 'otaUpdateInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotaurrsOtaUpdateInfo :: Lens.Lens' GetOTAUpdateResponse (Core.Maybe Types.OTAUpdateInfo)
gotaurrsOtaUpdateInfo = Lens.field @"otaUpdateInfo"
{-# INLINEABLE gotaurrsOtaUpdateInfo #-}
{-# DEPRECATED otaUpdateInfo "Use generic-lens or generic-optics with 'otaUpdateInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotaurrsResponseStatus :: Lens.Lens' GetOTAUpdateResponse Core.Int
gotaurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gotaurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
