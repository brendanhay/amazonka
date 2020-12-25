{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDestinationSettings
  ( CaptionDestinationSettings (..),

    -- * Smart constructor
    mkCaptionDestinationSettings,

    -- * Lenses
    cdsAribDestinationSettings,
    cdsBurnInDestinationSettings,
    cdsDvbSubDestinationSettings,
    cdsEbuTtDDestinationSettings,
    cdsEmbeddedDestinationSettings,
    cdsEmbeddedPlusScte20DestinationSettings,
    cdsRtmpCaptionInfoDestinationSettings,
    cdsScte20PlusEmbeddedDestinationSettings,
    cdsScte27DestinationSettings,
    cdsSmpteTtDestinationSettings,
    cdsTeletextDestinationSettings,
    cdsTtmlDestinationSettings,
    cdsWebvttDestinationSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AribDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.BurnInDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.DvbSubDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.EbuTtDDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.EmbeddedDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.Scte27DestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.SmpteTtDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.TeletextDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.TtmlDestinationSettings as Types
import qualified Network.AWS.MediaLive.Types.WebvttDestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Caption Destination Settings
--
-- /See:/ 'mkCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { aribDestinationSettings :: Core.Maybe Types.AribDestinationSettings,
    burnInDestinationSettings :: Core.Maybe Types.BurnInDestinationSettings,
    dvbSubDestinationSettings :: Core.Maybe Types.DvbSubDestinationSettings,
    ebuTtDDestinationSettings :: Core.Maybe Types.EbuTtDDestinationSettings,
    embeddedDestinationSettings :: Core.Maybe Types.EmbeddedDestinationSettings,
    embeddedPlusScte20DestinationSettings :: Core.Maybe Types.EmbeddedPlusScte20DestinationSettings,
    rtmpCaptionInfoDestinationSettings :: Core.Maybe Types.RtmpCaptionInfoDestinationSettings,
    scte20PlusEmbeddedDestinationSettings :: Core.Maybe Types.Scte20PlusEmbeddedDestinationSettings,
    scte27DestinationSettings :: Core.Maybe Types.Scte27DestinationSettings,
    smpteTtDestinationSettings :: Core.Maybe Types.SmpteTtDestinationSettings,
    teletextDestinationSettings :: Core.Maybe Types.TeletextDestinationSettings,
    ttmlDestinationSettings :: Core.Maybe Types.TtmlDestinationSettings,
    webvttDestinationSettings :: Core.Maybe Types.WebvttDestinationSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionDestinationSettings' value with any optional fields omitted.
mkCaptionDestinationSettings ::
  CaptionDestinationSettings
mkCaptionDestinationSettings =
  CaptionDestinationSettings'
    { aribDestinationSettings =
        Core.Nothing,
      burnInDestinationSettings = Core.Nothing,
      dvbSubDestinationSettings = Core.Nothing,
      ebuTtDDestinationSettings = Core.Nothing,
      embeddedDestinationSettings = Core.Nothing,
      embeddedPlusScte20DestinationSettings = Core.Nothing,
      rtmpCaptionInfoDestinationSettings = Core.Nothing,
      scte20PlusEmbeddedDestinationSettings = Core.Nothing,
      scte27DestinationSettings = Core.Nothing,
      smpteTtDestinationSettings = Core.Nothing,
      teletextDestinationSettings = Core.Nothing,
      ttmlDestinationSettings = Core.Nothing,
      webvttDestinationSettings = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'aribDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsAribDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.AribDestinationSettings)
cdsAribDestinationSettings = Lens.field @"aribDestinationSettings"
{-# DEPRECATED cdsAribDestinationSettings "Use generic-lens or generic-optics with 'aribDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'burnInDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsBurnInDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.BurnInDestinationSettings)
cdsBurnInDestinationSettings = Lens.field @"burnInDestinationSettings"
{-# DEPRECATED cdsBurnInDestinationSettings "Use generic-lens or generic-optics with 'burnInDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dvbSubDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.DvbSubDestinationSettings)
cdsDvbSubDestinationSettings = Lens.field @"dvbSubDestinationSettings"
{-# DEPRECATED cdsDvbSubDestinationSettings "Use generic-lens or generic-optics with 'dvbSubDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ebuTtDDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsEbuTtDDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.EbuTtDDestinationSettings)
cdsEbuTtDDestinationSettings = Lens.field @"ebuTtDDestinationSettings"
{-# DEPRECATED cdsEbuTtDDestinationSettings "Use generic-lens or generic-optics with 'ebuTtDDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'embeddedDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.EmbeddedDestinationSettings)
cdsEmbeddedDestinationSettings = Lens.field @"embeddedDestinationSettings"
{-# DEPRECATED cdsEmbeddedDestinationSettings "Use generic-lens or generic-optics with 'embeddedDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'embeddedPlusScte20DestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsEmbeddedPlusScte20DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.EmbeddedPlusScte20DestinationSettings)
cdsEmbeddedPlusScte20DestinationSettings = Lens.field @"embeddedPlusScte20DestinationSettings"
{-# DEPRECATED cdsEmbeddedPlusScte20DestinationSettings "Use generic-lens or generic-optics with 'embeddedPlusScte20DestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rtmpCaptionInfoDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsRtmpCaptionInfoDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.RtmpCaptionInfoDestinationSettings)
cdsRtmpCaptionInfoDestinationSettings = Lens.field @"rtmpCaptionInfoDestinationSettings"
{-# DEPRECATED cdsRtmpCaptionInfoDestinationSettings "Use generic-lens or generic-optics with 'rtmpCaptionInfoDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte20PlusEmbeddedDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsScte20PlusEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.Scte20PlusEmbeddedDestinationSettings)
cdsScte20PlusEmbeddedDestinationSettings = Lens.field @"scte20PlusEmbeddedDestinationSettings"
{-# DEPRECATED cdsScte20PlusEmbeddedDestinationSettings "Use generic-lens or generic-optics with 'scte20PlusEmbeddedDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte27DestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsScte27DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.Scte27DestinationSettings)
cdsScte27DestinationSettings = Lens.field @"scte27DestinationSettings"
{-# DEPRECATED cdsScte27DestinationSettings "Use generic-lens or generic-optics with 'scte27DestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'smpteTtDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsSmpteTtDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.SmpteTtDestinationSettings)
cdsSmpteTtDestinationSettings = Lens.field @"smpteTtDestinationSettings"
{-# DEPRECATED cdsSmpteTtDestinationSettings "Use generic-lens or generic-optics with 'smpteTtDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'teletextDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTeletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.TeletextDestinationSettings)
cdsTeletextDestinationSettings = Lens.field @"teletextDestinationSettings"
{-# DEPRECATED cdsTeletextDestinationSettings "Use generic-lens or generic-optics with 'teletextDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ttmlDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTtmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.TtmlDestinationSettings)
cdsTtmlDestinationSettings = Lens.field @"ttmlDestinationSettings"
{-# DEPRECATED cdsTtmlDestinationSettings "Use generic-lens or generic-optics with 'ttmlDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'webvttDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsWebvttDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.WebvttDestinationSettings)
cdsWebvttDestinationSettings = Lens.field @"webvttDestinationSettings"
{-# DEPRECATED cdsWebvttDestinationSettings "Use generic-lens or generic-optics with 'webvttDestinationSettings' instead." #-}

instance Core.FromJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("aribDestinationSettings" Core..=)
              Core.<$> aribDestinationSettings,
            ("burnInDestinationSettings" Core..=)
              Core.<$> burnInDestinationSettings,
            ("dvbSubDestinationSettings" Core..=)
              Core.<$> dvbSubDestinationSettings,
            ("ebuTtDDestinationSettings" Core..=)
              Core.<$> ebuTtDDestinationSettings,
            ("embeddedDestinationSettings" Core..=)
              Core.<$> embeddedDestinationSettings,
            ("embeddedPlusScte20DestinationSettings" Core..=)
              Core.<$> embeddedPlusScte20DestinationSettings,
            ("rtmpCaptionInfoDestinationSettings" Core..=)
              Core.<$> rtmpCaptionInfoDestinationSettings,
            ("scte20PlusEmbeddedDestinationSettings" Core..=)
              Core.<$> scte20PlusEmbeddedDestinationSettings,
            ("scte27DestinationSettings" Core..=)
              Core.<$> scte27DestinationSettings,
            ("smpteTtDestinationSettings" Core..=)
              Core.<$> smpteTtDestinationSettings,
            ("teletextDestinationSettings" Core..=)
              Core.<$> teletextDestinationSettings,
            ("ttmlDestinationSettings" Core..=)
              Core.<$> ttmlDestinationSettings,
            ("webvttDestinationSettings" Core..=)
              Core.<$> webvttDestinationSettings
          ]
      )

instance Core.FromJSON CaptionDestinationSettings where
  parseJSON =
    Core.withObject "CaptionDestinationSettings" Core.$
      \x ->
        CaptionDestinationSettings'
          Core.<$> (x Core..:? "aribDestinationSettings")
          Core.<*> (x Core..:? "burnInDestinationSettings")
          Core.<*> (x Core..:? "dvbSubDestinationSettings")
          Core.<*> (x Core..:? "ebuTtDDestinationSettings")
          Core.<*> (x Core..:? "embeddedDestinationSettings")
          Core.<*> (x Core..:? "embeddedPlusScte20DestinationSettings")
          Core.<*> (x Core..:? "rtmpCaptionInfoDestinationSettings")
          Core.<*> (x Core..:? "scte20PlusEmbeddedDestinationSettings")
          Core.<*> (x Core..:? "scte27DestinationSettings")
          Core.<*> (x Core..:? "smpteTtDestinationSettings")
          Core.<*> (x Core..:? "teletextDestinationSettings")
          Core.<*> (x Core..:? "ttmlDestinationSettings")
          Core.<*> (x Core..:? "webvttDestinationSettings")
