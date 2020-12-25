{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSourceSettings
  ( CaptionSourceSettings (..),

    -- * Smart constructor
    mkCaptionSourceSettings,

    -- * Lenses
    cssAncillarySourceSettings,
    cssDvbSubSourceSettings,
    cssEmbeddedSourceSettings,
    cssFileSourceSettings,
    cssSourceType,
    cssTeletextSourceSettings,
    cssTrackSourceSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AncillarySourceSettings as Types
import qualified Network.AWS.MediaConvert.Types.CaptionSourceType as Types
import qualified Network.AWS.MediaConvert.Types.DvbSubSourceSettings as Types
import qualified Network.AWS.MediaConvert.Types.EmbeddedSourceSettings as Types
import qualified Network.AWS.MediaConvert.Types.FileSourceSettings as Types
import qualified Network.AWS.MediaConvert.Types.TeletextSourceSettings as Types
import qualified Network.AWS.MediaConvert.Types.TrackSourceSettings as Types
import qualified Network.AWS.Prelude as Core

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml file, specify the URI of the input captions source file. If your input captions are IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /See:/ 'mkCaptionSourceSettings' smart constructor.
data CaptionSourceSettings = CaptionSourceSettings'
  { -- | Settings for ancillary captions source.
    ancillarySourceSettings :: Core.Maybe Types.AncillarySourceSettings,
    -- | DVB Sub Source Settings
    dvbSubSourceSettings :: Core.Maybe Types.DvbSubSourceSettings,
    -- | Settings for embedded captions Source
    embeddedSourceSettings :: Core.Maybe Types.EmbeddedSourceSettings,
    -- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
    fileSourceSettings :: Core.Maybe Types.FileSourceSettings,
    -- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
    sourceType :: Core.Maybe Types.CaptionSourceType,
    -- | Settings specific to Teletext caption sources, including Page number.
    teletextSourceSettings :: Core.Maybe Types.TeletextSourceSettings,
    -- | Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
    trackSourceSettings :: Core.Maybe Types.TrackSourceSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionSourceSettings' value with any optional fields omitted.
mkCaptionSourceSettings ::
  CaptionSourceSettings
mkCaptionSourceSettings =
  CaptionSourceSettings'
    { ancillarySourceSettings = Core.Nothing,
      dvbSubSourceSettings = Core.Nothing,
      embeddedSourceSettings = Core.Nothing,
      fileSourceSettings = Core.Nothing,
      sourceType = Core.Nothing,
      teletextSourceSettings = Core.Nothing,
      trackSourceSettings = Core.Nothing
    }

-- | Settings for ancillary captions source.
--
-- /Note:/ Consider using 'ancillarySourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAncillarySourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.AncillarySourceSettings)
cssAncillarySourceSettings = Lens.field @"ancillarySourceSettings"
{-# DEPRECATED cssAncillarySourceSettings "Use generic-lens or generic-optics with 'ancillarySourceSettings' instead." #-}

-- | DVB Sub Source Settings
--
-- /Note:/ Consider using 'dvbSubSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDvbSubSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.DvbSubSourceSettings)
cssDvbSubSourceSettings = Lens.field @"dvbSubSourceSettings"
{-# DEPRECATED cssDvbSubSourceSettings "Use generic-lens or generic-optics with 'dvbSubSourceSettings' instead." #-}

-- | Settings for embedded captions Source
--
-- /Note:/ Consider using 'embeddedSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssEmbeddedSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.EmbeddedSourceSettings)
cssEmbeddedSourceSettings = Lens.field @"embeddedSourceSettings"
{-# DEPRECATED cssEmbeddedSourceSettings "Use generic-lens or generic-optics with 'embeddedSourceSettings' instead." #-}

-- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /Note:/ Consider using 'fileSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssFileSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.FileSourceSettings)
cssFileSourceSettings = Lens.field @"fileSourceSettings"
{-# DEPRECATED cssFileSourceSettings "Use generic-lens or generic-optics with 'fileSourceSettings' instead." #-}

-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssSourceType :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.CaptionSourceType)
cssSourceType = Lens.field @"sourceType"
{-# DEPRECATED cssSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | Settings specific to Teletext caption sources, including Page number.
--
-- /Note:/ Consider using 'teletextSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTeletextSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.TeletextSourceSettings)
cssTeletextSourceSettings = Lens.field @"teletextSourceSettings"
{-# DEPRECATED cssTeletextSourceSettings "Use generic-lens or generic-optics with 'teletextSourceSettings' instead." #-}

-- | Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
--
-- /Note:/ Consider using 'trackSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTrackSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.TrackSourceSettings)
cssTrackSourceSettings = Lens.field @"trackSourceSettings"
{-# DEPRECATED cssTrackSourceSettings "Use generic-lens or generic-optics with 'trackSourceSettings' instead." #-}

instance Core.FromJSON CaptionSourceSettings where
  toJSON CaptionSourceSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("ancillarySourceSettings" Core..=)
              Core.<$> ancillarySourceSettings,
            ("dvbSubSourceSettings" Core..=) Core.<$> dvbSubSourceSettings,
            ("embeddedSourceSettings" Core..=) Core.<$> embeddedSourceSettings,
            ("fileSourceSettings" Core..=) Core.<$> fileSourceSettings,
            ("sourceType" Core..=) Core.<$> sourceType,
            ("teletextSourceSettings" Core..=) Core.<$> teletextSourceSettings,
            ("trackSourceSettings" Core..=) Core.<$> trackSourceSettings
          ]
      )

instance Core.FromJSON CaptionSourceSettings where
  parseJSON =
    Core.withObject "CaptionSourceSettings" Core.$
      \x ->
        CaptionSourceSettings'
          Core.<$> (x Core..:? "ancillarySourceSettings")
          Core.<*> (x Core..:? "dvbSubSourceSettings")
          Core.<*> (x Core..:? "embeddedSourceSettings")
          Core.<*> (x Core..:? "fileSourceSettings")
          Core.<*> (x Core..:? "sourceType")
          Core.<*> (x Core..:? "teletextSourceSettings")
          Core.<*> (x Core..:? "trackSourceSettings")
