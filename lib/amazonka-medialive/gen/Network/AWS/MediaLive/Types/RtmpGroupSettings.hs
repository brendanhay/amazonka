{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpGroupSettings
  ( RtmpGroupSettings (..),

    -- * Smart constructor
    mkRtmpGroupSettings,

    -- * Lenses
    rgsInputLossAction,
    rgsCaptionData,
    rgsAdMarkers,
    rgsRestartDelay,
    rgsAuthenticationScheme,
    rgsCacheLength,
    rgsCacheFullBehavior,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AuthenticationScheme
import Network.AWS.MediaLive.Types.InputLossActionForRtmpOut
import Network.AWS.MediaLive.Types.RtmpAdMarkers
import Network.AWS.MediaLive.Types.RtmpCacheFullBehavior
import Network.AWS.MediaLive.Types.RtmpCaptionData
import qualified Network.AWS.Prelude as Lude

-- | Rtmp Group Settings
--
-- /See:/ 'mkRtmpGroupSettings' smart constructor.
data RtmpGroupSettings = RtmpGroupSettings'
  { inputLossAction ::
      Lude.Maybe InputLossActionForRtmpOut,
    captionData :: Lude.Maybe RtmpCaptionData,
    adMarkers :: Lude.Maybe [RtmpAdMarkers],
    restartDelay :: Lude.Maybe Lude.Natural,
    authenticationScheme :: Lude.Maybe AuthenticationScheme,
    cacheLength :: Lude.Maybe Lude.Natural,
    cacheFullBehavior :: Lude.Maybe RtmpCacheFullBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RtmpGroupSettings' with the minimum fields required to make a request.
--
-- * 'adMarkers' - Choose the ad marker type for this output group. MediaLive will create a message based on the content of each SCTE-35 message, format it for that marker type, and insert it in the datastream.
-- * 'authenticationScheme' - Authentication scheme to use when connecting with CDN
-- * 'cacheFullBehavior' - Controls behavior when content cache fills up. If remote origin server stalls the RTMP connection and does not accept content fast enough the 'Media Cache' will fill up. When the cache reaches the duration specified by cacheLength the cache will stop accepting new content. If set to disconnectImmediately, the RTMP output will force a disconnect. Clear the media cache, and reconnect after restartDelay seconds. If set to waitForServer, the RTMP output will wait up to 5 minutes to allow the origin server to begin accepting data again.
-- * 'cacheLength' - Cache length, in seconds, is used to calculate buffer size.
-- * 'captionData' - Controls the types of data that passes to onCaptionInfo outputs.  If set to 'all' then 608 and 708 carried DTVCC data will be passed.  If set to 'field1AndField2608' then DTVCC data will be stripped out, but 608 data from both fields will be passed. If set to 'field1608' then only the data carried in 608 from field 1 video will be passed.
-- * 'inputLossAction' - Controls the behavior of this RTMP group if input becomes unavailable.
--
--
-- - emitOutput: Emit a slate until input returns.
-- - pauseOutput: Stop transmitting data until input returns. This does not close the underlying RTMP connection.
-- * 'restartDelay' - If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
mkRtmpGroupSettings ::
  RtmpGroupSettings
mkRtmpGroupSettings =
  RtmpGroupSettings'
    { inputLossAction = Lude.Nothing,
      captionData = Lude.Nothing,
      adMarkers = Lude.Nothing,
      restartDelay = Lude.Nothing,
      authenticationScheme = Lude.Nothing,
      cacheLength = Lude.Nothing,
      cacheFullBehavior = Lude.Nothing
    }

-- | Controls the behavior of this RTMP group if input becomes unavailable.
--
--
-- - emitOutput: Emit a slate until input returns.
-- - pauseOutput: Stop transmitting data until input returns. This does not close the underlying RTMP connection.
--
-- /Note:/ Consider using 'inputLossAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsInputLossAction :: Lens.Lens' RtmpGroupSettings (Lude.Maybe InputLossActionForRtmpOut)
rgsInputLossAction = Lens.lens (inputLossAction :: RtmpGroupSettings -> Lude.Maybe InputLossActionForRtmpOut) (\s a -> s {inputLossAction = a} :: RtmpGroupSettings)
{-# DEPRECATED rgsInputLossAction "Use generic-lens or generic-optics with 'inputLossAction' instead." #-}

-- | Controls the types of data that passes to onCaptionInfo outputs.  If set to 'all' then 608 and 708 carried DTVCC data will be passed.  If set to 'field1AndField2608' then DTVCC data will be stripped out, but 608 data from both fields will be passed. If set to 'field1608' then only the data carried in 608 from field 1 video will be passed.
--
-- /Note:/ Consider using 'captionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsCaptionData :: Lens.Lens' RtmpGroupSettings (Lude.Maybe RtmpCaptionData)
rgsCaptionData = Lens.lens (captionData :: RtmpGroupSettings -> Lude.Maybe RtmpCaptionData) (\s a -> s {captionData = a} :: RtmpGroupSettings)
{-# DEPRECATED rgsCaptionData "Use generic-lens or generic-optics with 'captionData' instead." #-}

-- | Choose the ad marker type for this output group. MediaLive will create a message based on the content of each SCTE-35 message, format it for that marker type, and insert it in the datastream.
--
-- /Note:/ Consider using 'adMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsAdMarkers :: Lens.Lens' RtmpGroupSettings (Lude.Maybe [RtmpAdMarkers])
rgsAdMarkers = Lens.lens (adMarkers :: RtmpGroupSettings -> Lude.Maybe [RtmpAdMarkers]) (\s a -> s {adMarkers = a} :: RtmpGroupSettings)
{-# DEPRECATED rgsAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | If a streaming output fails, number of seconds to wait until a restart is initiated. A value of 0 means never restart.
--
-- /Note:/ Consider using 'restartDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsRestartDelay :: Lens.Lens' RtmpGroupSettings (Lude.Maybe Lude.Natural)
rgsRestartDelay = Lens.lens (restartDelay :: RtmpGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {restartDelay = a} :: RtmpGroupSettings)
{-# DEPRECATED rgsRestartDelay "Use generic-lens or generic-optics with 'restartDelay' instead." #-}

-- | Authentication scheme to use when connecting with CDN
--
-- /Note:/ Consider using 'authenticationScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsAuthenticationScheme :: Lens.Lens' RtmpGroupSettings (Lude.Maybe AuthenticationScheme)
rgsAuthenticationScheme = Lens.lens (authenticationScheme :: RtmpGroupSettings -> Lude.Maybe AuthenticationScheme) (\s a -> s {authenticationScheme = a} :: RtmpGroupSettings)
{-# DEPRECATED rgsAuthenticationScheme "Use generic-lens or generic-optics with 'authenticationScheme' instead." #-}

-- | Cache length, in seconds, is used to calculate buffer size.
--
-- /Note:/ Consider using 'cacheLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsCacheLength :: Lens.Lens' RtmpGroupSettings (Lude.Maybe Lude.Natural)
rgsCacheLength = Lens.lens (cacheLength :: RtmpGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {cacheLength = a} :: RtmpGroupSettings)
{-# DEPRECATED rgsCacheLength "Use generic-lens or generic-optics with 'cacheLength' instead." #-}

-- | Controls behavior when content cache fills up. If remote origin server stalls the RTMP connection and does not accept content fast enough the 'Media Cache' will fill up. When the cache reaches the duration specified by cacheLength the cache will stop accepting new content. If set to disconnectImmediately, the RTMP output will force a disconnect. Clear the media cache, and reconnect after restartDelay seconds. If set to waitForServer, the RTMP output will wait up to 5 minutes to allow the origin server to begin accepting data again.
--
-- /Note:/ Consider using 'cacheFullBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsCacheFullBehavior :: Lens.Lens' RtmpGroupSettings (Lude.Maybe RtmpCacheFullBehavior)
rgsCacheFullBehavior = Lens.lens (cacheFullBehavior :: RtmpGroupSettings -> Lude.Maybe RtmpCacheFullBehavior) (\s a -> s {cacheFullBehavior = a} :: RtmpGroupSettings)
{-# DEPRECATED rgsCacheFullBehavior "Use generic-lens or generic-optics with 'cacheFullBehavior' instead." #-}

instance Lude.FromJSON RtmpGroupSettings where
  parseJSON =
    Lude.withObject
      "RtmpGroupSettings"
      ( \x ->
          RtmpGroupSettings'
            Lude.<$> (x Lude..:? "inputLossAction")
            Lude.<*> (x Lude..:? "captionData")
            Lude.<*> (x Lude..:? "adMarkers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "restartDelay")
            Lude.<*> (x Lude..:? "authenticationScheme")
            Lude.<*> (x Lude..:? "cacheLength")
            Lude.<*> (x Lude..:? "cacheFullBehavior")
      )

instance Lude.ToJSON RtmpGroupSettings where
  toJSON RtmpGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inputLossAction" Lude..=) Lude.<$> inputLossAction,
            ("captionData" Lude..=) Lude.<$> captionData,
            ("adMarkers" Lude..=) Lude.<$> adMarkers,
            ("restartDelay" Lude..=) Lude.<$> restartDelay,
            ("authenticationScheme" Lude..=) Lude.<$> authenticationScheme,
            ("cacheLength" Lude..=) Lude.<$> cacheLength,
            ("cacheFullBehavior" Lude..=) Lude.<$> cacheFullBehavior
          ]
      )
