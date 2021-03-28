{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.RtmpGroupSettings
  ( RtmpGroupSettings (..)
  -- * Smart constructor
  , mkRtmpGroupSettings
  -- * Lenses
  , rgsAdMarkers
  , rgsAuthenticationScheme
  , rgsCacheFullBehavior
  , rgsCacheLength
  , rgsCaptionData
  , rgsInputLossAction
  , rgsRestartDelay
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AuthenticationScheme as Types
import qualified Network.AWS.MediaLive.Types.InputLossActionForRtmpOut as Types
import qualified Network.AWS.MediaLive.Types.RtmpAdMarkers as Types
import qualified Network.AWS.MediaLive.Types.RtmpCacheFullBehavior as Types
import qualified Network.AWS.MediaLive.Types.RtmpCaptionData as Types
import qualified Network.AWS.Prelude as Core

-- | Rtmp Group Settings
--
-- /See:/ 'mkRtmpGroupSettings' smart constructor.
data RtmpGroupSettings = RtmpGroupSettings'
  { adMarkers :: Core.Maybe [Types.RtmpAdMarkers]
    -- ^ Choose the ad marker type for this output group. MediaLive will create a message based on the content of each SCTE-35 message, format it for that marker type, and insert it in the datastream.
  , authenticationScheme :: Core.Maybe Types.AuthenticationScheme
    -- ^ Authentication scheme to use when connecting with CDN
  , cacheFullBehavior :: Core.Maybe Types.RtmpCacheFullBehavior
    -- ^ Controls behavior when content cache fills up. If remote origin server stalls the RTMP connection and does not accept content fast enough the 'Media Cache' will fill up. When the cache reaches the duration specified by cacheLength the cache will stop accepting new content. If set to disconnectImmediately, the RTMP output will force a disconnect. Clear the media cache, and reconnect after restartDelay seconds. If set to waitForServer, the RTMP output will wait up to 5 minutes to allow the origin server to begin accepting data again.
  , cacheLength :: Core.Maybe Core.Natural
    -- ^ Cache length, in seconds, is used to calculate buffer size.
  , captionData :: Core.Maybe Types.RtmpCaptionData
    -- ^ Controls the types of data that passes to onCaptionInfo outputs.  If set to 'all' then 608 and 708 carried DTVCC data will be passed.  If set to 'field1AndField2608' then DTVCC data will be stripped out, but 608 data from both fields will be passed. If set to 'field1608' then only the data carried in 608 from field 1 video will be passed.
  , inputLossAction :: Core.Maybe Types.InputLossActionForRtmpOut
    -- ^ Controls the behavior of this RTMP group if input becomes unavailable.
--
--
-- - emitOutput: Emit a slate until input returns.
-- - pauseOutput: Stop transmitting data until input returns. This does not close the underlying RTMP connection.
  , restartDelay :: Core.Maybe Core.Natural
    -- ^ If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RtmpGroupSettings' value with any optional fields omitted.
mkRtmpGroupSettings
    :: RtmpGroupSettings
mkRtmpGroupSettings
  = RtmpGroupSettings'{adMarkers = Core.Nothing,
                       authenticationScheme = Core.Nothing,
                       cacheFullBehavior = Core.Nothing, cacheLength = Core.Nothing,
                       captionData = Core.Nothing, inputLossAction = Core.Nothing,
                       restartDelay = Core.Nothing}

-- | Choose the ad marker type for this output group. MediaLive will create a message based on the content of each SCTE-35 message, format it for that marker type, and insert it in the datastream.
--
-- /Note:/ Consider using 'adMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsAdMarkers :: Lens.Lens' RtmpGroupSettings (Core.Maybe [Types.RtmpAdMarkers])
rgsAdMarkers = Lens.field @"adMarkers"
{-# INLINEABLE rgsAdMarkers #-}
{-# DEPRECATED adMarkers "Use generic-lens or generic-optics with 'adMarkers' instead"  #-}

-- | Authentication scheme to use when connecting with CDN
--
-- /Note:/ Consider using 'authenticationScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsAuthenticationScheme :: Lens.Lens' RtmpGroupSettings (Core.Maybe Types.AuthenticationScheme)
rgsAuthenticationScheme = Lens.field @"authenticationScheme"
{-# INLINEABLE rgsAuthenticationScheme #-}
{-# DEPRECATED authenticationScheme "Use generic-lens or generic-optics with 'authenticationScheme' instead"  #-}

-- | Controls behavior when content cache fills up. If remote origin server stalls the RTMP connection and does not accept content fast enough the 'Media Cache' will fill up. When the cache reaches the duration specified by cacheLength the cache will stop accepting new content. If set to disconnectImmediately, the RTMP output will force a disconnect. Clear the media cache, and reconnect after restartDelay seconds. If set to waitForServer, the RTMP output will wait up to 5 minutes to allow the origin server to begin accepting data again.
--
-- /Note:/ Consider using 'cacheFullBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsCacheFullBehavior :: Lens.Lens' RtmpGroupSettings (Core.Maybe Types.RtmpCacheFullBehavior)
rgsCacheFullBehavior = Lens.field @"cacheFullBehavior"
{-# INLINEABLE rgsCacheFullBehavior #-}
{-# DEPRECATED cacheFullBehavior "Use generic-lens or generic-optics with 'cacheFullBehavior' instead"  #-}

-- | Cache length, in seconds, is used to calculate buffer size.
--
-- /Note:/ Consider using 'cacheLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsCacheLength :: Lens.Lens' RtmpGroupSettings (Core.Maybe Core.Natural)
rgsCacheLength = Lens.field @"cacheLength"
{-# INLINEABLE rgsCacheLength #-}
{-# DEPRECATED cacheLength "Use generic-lens or generic-optics with 'cacheLength' instead"  #-}

-- | Controls the types of data that passes to onCaptionInfo outputs.  If set to 'all' then 608 and 708 carried DTVCC data will be passed.  If set to 'field1AndField2608' then DTVCC data will be stripped out, but 608 data from both fields will be passed. If set to 'field1608' then only the data carried in 608 from field 1 video will be passed.
--
-- /Note:/ Consider using 'captionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsCaptionData :: Lens.Lens' RtmpGroupSettings (Core.Maybe Types.RtmpCaptionData)
rgsCaptionData = Lens.field @"captionData"
{-# INLINEABLE rgsCaptionData #-}
{-# DEPRECATED captionData "Use generic-lens or generic-optics with 'captionData' instead"  #-}

-- | Controls the behavior of this RTMP group if input becomes unavailable.
--
--
-- - emitOutput: Emit a slate until input returns.
-- - pauseOutput: Stop transmitting data until input returns. This does not close the underlying RTMP connection.
--
-- /Note:/ Consider using 'inputLossAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsInputLossAction :: Lens.Lens' RtmpGroupSettings (Core.Maybe Types.InputLossActionForRtmpOut)
rgsInputLossAction = Lens.field @"inputLossAction"
{-# INLINEABLE rgsInputLossAction #-}
{-# DEPRECATED inputLossAction "Use generic-lens or generic-optics with 'inputLossAction' instead"  #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsRestartDelay :: Lens.Lens' RtmpGroupSettings (Core.Maybe Core.Natural)
rgsRestartDelay = Lens.field @"restartDelay"
{-# INLINEABLE rgsRestartDelay #-}
{-# DEPRECATED restartDelay "Use generic-lens or generic-optics with 'restartDelay' instead"  #-}

instance Core.FromJSON RtmpGroupSettings where
        toJSON RtmpGroupSettings{..}
          = Core.object
              (Core.catMaybes
                 [("adMarkers" Core..=) Core.<$> adMarkers,
                  ("authenticationScheme" Core..=) Core.<$> authenticationScheme,
                  ("cacheFullBehavior" Core..=) Core.<$> cacheFullBehavior,
                  ("cacheLength" Core..=) Core.<$> cacheLength,
                  ("captionData" Core..=) Core.<$> captionData,
                  ("inputLossAction" Core..=) Core.<$> inputLossAction,
                  ("restartDelay" Core..=) Core.<$> restartDelay])

instance Core.FromJSON RtmpGroupSettings where
        parseJSON
          = Core.withObject "RtmpGroupSettings" Core.$
              \ x ->
                RtmpGroupSettings' Core.<$>
                  (x Core..:? "adMarkers") Core.<*> x Core..:? "authenticationScheme"
                    Core.<*> x Core..:? "cacheFullBehavior"
                    Core.<*> x Core..:? "cacheLength"
                    Core.<*> x Core..:? "captionData"
                    Core.<*> x Core..:? "inputLossAction"
                    Core.<*> x Core..:? "restartDelay"
