{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CaptionSourceSettings
  ( CaptionSourceSettings (..)
  -- * Smart constructor
  , mkCaptionSourceSettings
  -- * Lenses
  , cssAncillarySourceSettings
  , cssDvbSubSourceSettings
  , cssEmbeddedSourceSettings
  , cssFileSourceSettings
  , cssSourceType
  , cssTeletextSourceSettings
  , cssTrackSourceSettings
  ) where

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
  { ancillarySourceSettings :: Core.Maybe Types.AncillarySourceSettings
    -- ^ Settings for ancillary captions source.
  , dvbSubSourceSettings :: Core.Maybe Types.DvbSubSourceSettings
    -- ^ DVB Sub Source Settings
  , embeddedSourceSettings :: Core.Maybe Types.EmbeddedSourceSettings
    -- ^ Settings for embedded captions Source
  , fileSourceSettings :: Core.Maybe Types.FileSourceSettings
    -- ^ If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
  , sourceType :: Core.Maybe Types.CaptionSourceType
    -- ^ Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
  , teletextSourceSettings :: Core.Maybe Types.TeletextSourceSettings
    -- ^ Settings specific to Teletext caption sources, including Page number.
  , trackSourceSettings :: Core.Maybe Types.TrackSourceSettings
    -- ^ Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionSourceSettings' value with any optional fields omitted.
mkCaptionSourceSettings
    :: CaptionSourceSettings
mkCaptionSourceSettings
  = CaptionSourceSettings'{ancillarySourceSettings = Core.Nothing,
                           dvbSubSourceSettings = Core.Nothing,
                           embeddedSourceSettings = Core.Nothing,
                           fileSourceSettings = Core.Nothing, sourceType = Core.Nothing,
                           teletextSourceSettings = Core.Nothing,
                           trackSourceSettings = Core.Nothing}

-- | Settings for ancillary captions source.
--
-- /Note:/ Consider using 'ancillarySourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAncillarySourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.AncillarySourceSettings)
cssAncillarySourceSettings = Lens.field @"ancillarySourceSettings"
{-# INLINEABLE cssAncillarySourceSettings #-}
{-# DEPRECATED ancillarySourceSettings "Use generic-lens or generic-optics with 'ancillarySourceSettings' instead"  #-}

-- | DVB Sub Source Settings
--
-- /Note:/ Consider using 'dvbSubSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDvbSubSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.DvbSubSourceSettings)
cssDvbSubSourceSettings = Lens.field @"dvbSubSourceSettings"
{-# INLINEABLE cssDvbSubSourceSettings #-}
{-# DEPRECATED dvbSubSourceSettings "Use generic-lens or generic-optics with 'dvbSubSourceSettings' instead"  #-}

-- | Settings for embedded captions Source
--
-- /Note:/ Consider using 'embeddedSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssEmbeddedSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.EmbeddedSourceSettings)
cssEmbeddedSourceSettings = Lens.field @"embeddedSourceSettings"
{-# INLINEABLE cssEmbeddedSourceSettings #-}
{-# DEPRECATED embeddedSourceSettings "Use generic-lens or generic-optics with 'embeddedSourceSettings' instead"  #-}

-- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /Note:/ Consider using 'fileSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssFileSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.FileSourceSettings)
cssFileSourceSettings = Lens.field @"fileSourceSettings"
{-# INLINEABLE cssFileSourceSettings #-}
{-# DEPRECATED fileSourceSettings "Use generic-lens or generic-optics with 'fileSourceSettings' instead"  #-}

-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssSourceType :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.CaptionSourceType)
cssSourceType = Lens.field @"sourceType"
{-# INLINEABLE cssSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

-- | Settings specific to Teletext caption sources, including Page number.
--
-- /Note:/ Consider using 'teletextSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTeletextSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.TeletextSourceSettings)
cssTeletextSourceSettings = Lens.field @"teletextSourceSettings"
{-# INLINEABLE cssTeletextSourceSettings #-}
{-# DEPRECATED teletextSourceSettings "Use generic-lens or generic-optics with 'teletextSourceSettings' instead"  #-}

-- | Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
--
-- /Note:/ Consider using 'trackSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTrackSourceSettings :: Lens.Lens' CaptionSourceSettings (Core.Maybe Types.TrackSourceSettings)
cssTrackSourceSettings = Lens.field @"trackSourceSettings"
{-# INLINEABLE cssTrackSourceSettings #-}
{-# DEPRECATED trackSourceSettings "Use generic-lens or generic-optics with 'trackSourceSettings' instead"  #-}

instance Core.FromJSON CaptionSourceSettings where
        toJSON CaptionSourceSettings{..}
          = Core.object
              (Core.catMaybes
                 [("ancillarySourceSettings" Core..=) Core.<$>
                    ancillarySourceSettings,
                  ("dvbSubSourceSettings" Core..=) Core.<$> dvbSubSourceSettings,
                  ("embeddedSourceSettings" Core..=) Core.<$> embeddedSourceSettings,
                  ("fileSourceSettings" Core..=) Core.<$> fileSourceSettings,
                  ("sourceType" Core..=) Core.<$> sourceType,
                  ("teletextSourceSettings" Core..=) Core.<$> teletextSourceSettings,
                  ("trackSourceSettings" Core..=) Core.<$> trackSourceSettings])

instance Core.FromJSON CaptionSourceSettings where
        parseJSON
          = Core.withObject "CaptionSourceSettings" Core.$
              \ x ->
                CaptionSourceSettings' Core.<$>
                  (x Core..:? "ancillarySourceSettings") Core.<*>
                    x Core..:? "dvbSubSourceSettings"
                    Core.<*> x Core..:? "embeddedSourceSettings"
                    Core.<*> x Core..:? "fileSourceSettings"
                    Core.<*> x Core..:? "sourceType"
                    Core.<*> x Core..:? "teletextSourceSettings"
                    Core.<*> x Core..:? "trackSourceSettings"
