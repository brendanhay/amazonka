{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDestinationSettings
  ( CaptionDestinationSettings (..),

    -- * Smart constructor
    mkCaptionDestinationSettings,

    -- * Lenses
    cdsBurninDestinationSettings,
    cdsDestinationType,
    cdsDvbSubDestinationSettings,
    cdsEmbeddedDestinationSettings,
    cdsImscDestinationSettings,
    cdsSccDestinationSettings,
    cdsTeletextDestinationSettings,
    cdsTtmlDestinationSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.BurninDestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.CaptionDestinationType as Types
import qualified Network.AWS.MediaConvert.Types.DvbSubDestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.ImscDestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.SccDestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.TeletextDestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.TtmlDestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
--
-- /See:/ 'mkCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { -- | Burn-In Destination Settings.
    burninDestinationSettings :: Core.Maybe Types.BurninDestinationSettings,
    -- | Specify the format for this set of captions on this output. The default format is embedded without SCTE-20. Other options are embedded with SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT. If you are using SCTE-20, choose SCTE-20 plus embedded (SCTE20_PLUS_EMBEDDED) to create an output that complies with the SCTE-43 spec. To create a non-compliant output where the embedded captions come first, choose Embedded plus SCTE-20 (EMBEDDED_PLUS_SCTE20).
    destinationType :: Core.Maybe Types.CaptionDestinationType,
    -- | DVB-Sub Destination Settings
    dvbSubDestinationSettings :: Core.Maybe Types.DvbSubDestinationSettings,
    -- | Settings specific to embedded/ancillary caption outputs, including 608/708 Channel destination number.
    embeddedDestinationSettings :: Core.Maybe Types.EmbeddedDestinationSettings,
    -- | Settings specific to IMSC caption outputs.
    imscDestinationSettings :: Core.Maybe Types.ImscDestinationSettings,
    -- | Settings for SCC caption output.
    sccDestinationSettings :: Core.Maybe Types.SccDestinationSettings,
    -- | Settings for Teletext caption output
    teletextDestinationSettings :: Core.Maybe Types.TeletextDestinationSettings,
    -- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
    ttmlDestinationSettings :: Core.Maybe Types.TtmlDestinationSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionDestinationSettings' value with any optional fields omitted.
mkCaptionDestinationSettings ::
  CaptionDestinationSettings
mkCaptionDestinationSettings =
  CaptionDestinationSettings'
    { burninDestinationSettings =
        Core.Nothing,
      destinationType = Core.Nothing,
      dvbSubDestinationSettings = Core.Nothing,
      embeddedDestinationSettings = Core.Nothing,
      imscDestinationSettings = Core.Nothing,
      sccDestinationSettings = Core.Nothing,
      teletextDestinationSettings = Core.Nothing,
      ttmlDestinationSettings = Core.Nothing
    }

-- | Burn-In Destination Settings.
--
-- /Note:/ Consider using 'burninDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsBurninDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.BurninDestinationSettings)
cdsBurninDestinationSettings = Lens.field @"burninDestinationSettings"
{-# DEPRECATED cdsBurninDestinationSettings "Use generic-lens or generic-optics with 'burninDestinationSettings' instead." #-}

-- | Specify the format for this set of captions on this output. The default format is embedded without SCTE-20. Other options are embedded with SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT. If you are using SCTE-20, choose SCTE-20 plus embedded (SCTE20_PLUS_EMBEDDED) to create an output that complies with the SCTE-43 spec. To create a non-compliant output where the embedded captions come first, choose Embedded plus SCTE-20 (EMBEDDED_PLUS_SCTE20).
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDestinationType :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.CaptionDestinationType)
cdsDestinationType = Lens.field @"destinationType"
{-# DEPRECATED cdsDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | DVB-Sub Destination Settings
--
-- /Note:/ Consider using 'dvbSubDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.DvbSubDestinationSettings)
cdsDvbSubDestinationSettings = Lens.field @"dvbSubDestinationSettings"
{-# DEPRECATED cdsDvbSubDestinationSettings "Use generic-lens or generic-optics with 'dvbSubDestinationSettings' instead." #-}

-- | Settings specific to embedded/ancillary caption outputs, including 608/708 Channel destination number.
--
-- /Note:/ Consider using 'embeddedDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.EmbeddedDestinationSettings)
cdsEmbeddedDestinationSettings = Lens.field @"embeddedDestinationSettings"
{-# DEPRECATED cdsEmbeddedDestinationSettings "Use generic-lens or generic-optics with 'embeddedDestinationSettings' instead." #-}

-- | Settings specific to IMSC caption outputs.
--
-- /Note:/ Consider using 'imscDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsImscDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.ImscDestinationSettings)
cdsImscDestinationSettings = Lens.field @"imscDestinationSettings"
{-# DEPRECATED cdsImscDestinationSettings "Use generic-lens or generic-optics with 'imscDestinationSettings' instead." #-}

-- | Settings for SCC caption output.
--
-- /Note:/ Consider using 'sccDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsSccDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.SccDestinationSettings)
cdsSccDestinationSettings = Lens.field @"sccDestinationSettings"
{-# DEPRECATED cdsSccDestinationSettings "Use generic-lens or generic-optics with 'sccDestinationSettings' instead." #-}

-- | Settings for Teletext caption output
--
-- /Note:/ Consider using 'teletextDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTeletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.TeletextDestinationSettings)
cdsTeletextDestinationSettings = Lens.field @"teletextDestinationSettings"
{-# DEPRECATED cdsTeletextDestinationSettings "Use generic-lens or generic-optics with 'teletextDestinationSettings' instead." #-}

-- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
--
-- /Note:/ Consider using 'ttmlDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTtmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Types.TtmlDestinationSettings)
cdsTtmlDestinationSettings = Lens.field @"ttmlDestinationSettings"
{-# DEPRECATED cdsTtmlDestinationSettings "Use generic-lens or generic-optics with 'ttmlDestinationSettings' instead." #-}

instance Core.FromJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("burninDestinationSettings" Core..=)
              Core.<$> burninDestinationSettings,
            ("destinationType" Core..=) Core.<$> destinationType,
            ("dvbSubDestinationSettings" Core..=)
              Core.<$> dvbSubDestinationSettings,
            ("embeddedDestinationSettings" Core..=)
              Core.<$> embeddedDestinationSettings,
            ("imscDestinationSettings" Core..=)
              Core.<$> imscDestinationSettings,
            ("sccDestinationSettings" Core..=) Core.<$> sccDestinationSettings,
            ("teletextDestinationSettings" Core..=)
              Core.<$> teletextDestinationSettings,
            ("ttmlDestinationSettings" Core..=)
              Core.<$> ttmlDestinationSettings
          ]
      )

instance Core.FromJSON CaptionDestinationSettings where
  parseJSON =
    Core.withObject "CaptionDestinationSettings" Core.$
      \x ->
        CaptionDestinationSettings'
          Core.<$> (x Core..:? "burninDestinationSettings")
          Core.<*> (x Core..:? "destinationType")
          Core.<*> (x Core..:? "dvbSubDestinationSettings")
          Core.<*> (x Core..:? "embeddedDestinationSettings")
          Core.<*> (x Core..:? "imscDestinationSettings")
          Core.<*> (x Core..:? "sccDestinationSettings")
          Core.<*> (x Core..:? "teletextDestinationSettings")
          Core.<*> (x Core..:? "ttmlDestinationSettings")
