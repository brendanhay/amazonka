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
    cssTeletextSourceSettings,
    cssSourceType,
    cssFileSourceSettings,
    cssDvbSubSourceSettings,
    cssTrackSourceSettings,
    cssAncillarySourceSettings,
    cssEmbeddedSourceSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AncillarySourceSettings
import Network.AWS.MediaConvert.Types.CaptionSourceType
import Network.AWS.MediaConvert.Types.DvbSubSourceSettings
import Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
import Network.AWS.MediaConvert.Types.FileSourceSettings
import Network.AWS.MediaConvert.Types.TeletextSourceSettings
import Network.AWS.MediaConvert.Types.TrackSourceSettings
import qualified Network.AWS.Prelude as Lude

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml file, specify the URI of the input captions source file. If your input captions are IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /See:/ 'mkCaptionSourceSettings' smart constructor.
data CaptionSourceSettings = CaptionSourceSettings'
  { -- | Settings specific to Teletext caption sources, including Page number.
    teletextSourceSettings :: Lude.Maybe TeletextSourceSettings,
    -- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
    sourceType :: Lude.Maybe CaptionSourceType,
    -- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
    fileSourceSettings :: Lude.Maybe FileSourceSettings,
    -- | DVB Sub Source Settings
    dvbSubSourceSettings :: Lude.Maybe DvbSubSourceSettings,
    -- | Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
    trackSourceSettings :: Lude.Maybe TrackSourceSettings,
    -- | Settings for ancillary captions source.
    ancillarySourceSettings :: Lude.Maybe AncillarySourceSettings,
    -- | Settings for embedded captions Source
    embeddedSourceSettings :: Lude.Maybe EmbeddedSourceSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionSourceSettings' with the minimum fields required to make a request.
--
-- * 'teletextSourceSettings' - Settings specific to Teletext caption sources, including Page number.
-- * 'sourceType' - Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
-- * 'fileSourceSettings' - If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
-- * 'dvbSubSourceSettings' - DVB Sub Source Settings
-- * 'trackSourceSettings' - Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
-- * 'ancillarySourceSettings' - Settings for ancillary captions source.
-- * 'embeddedSourceSettings' - Settings for embedded captions Source
mkCaptionSourceSettings ::
  CaptionSourceSettings
mkCaptionSourceSettings =
  CaptionSourceSettings'
    { teletextSourceSettings = Lude.Nothing,
      sourceType = Lude.Nothing,
      fileSourceSettings = Lude.Nothing,
      dvbSubSourceSettings = Lude.Nothing,
      trackSourceSettings = Lude.Nothing,
      ancillarySourceSettings = Lude.Nothing,
      embeddedSourceSettings = Lude.Nothing
    }

-- | Settings specific to Teletext caption sources, including Page number.
--
-- /Note:/ Consider using 'teletextSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTeletextSourceSettings :: Lens.Lens' CaptionSourceSettings (Lude.Maybe TeletextSourceSettings)
cssTeletextSourceSettings = Lens.lens (teletextSourceSettings :: CaptionSourceSettings -> Lude.Maybe TeletextSourceSettings) (\s a -> s {teletextSourceSettings = a} :: CaptionSourceSettings)
{-# DEPRECATED cssTeletextSourceSettings "Use generic-lens or generic-optics with 'teletextSourceSettings' instead." #-}

-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssSourceType :: Lens.Lens' CaptionSourceSettings (Lude.Maybe CaptionSourceType)
cssSourceType = Lens.lens (sourceType :: CaptionSourceSettings -> Lude.Maybe CaptionSourceType) (\s a -> s {sourceType = a} :: CaptionSourceSettings)
{-# DEPRECATED cssSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /Note:/ Consider using 'fileSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssFileSourceSettings :: Lens.Lens' CaptionSourceSettings (Lude.Maybe FileSourceSettings)
cssFileSourceSettings = Lens.lens (fileSourceSettings :: CaptionSourceSettings -> Lude.Maybe FileSourceSettings) (\s a -> s {fileSourceSettings = a} :: CaptionSourceSettings)
{-# DEPRECATED cssFileSourceSettings "Use generic-lens or generic-optics with 'fileSourceSettings' instead." #-}

-- | DVB Sub Source Settings
--
-- /Note:/ Consider using 'dvbSubSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDvbSubSourceSettings :: Lens.Lens' CaptionSourceSettings (Lude.Maybe DvbSubSourceSettings)
cssDvbSubSourceSettings = Lens.lens (dvbSubSourceSettings :: CaptionSourceSettings -> Lude.Maybe DvbSubSourceSettings) (\s a -> s {dvbSubSourceSettings = a} :: CaptionSourceSettings)
{-# DEPRECATED cssDvbSubSourceSettings "Use generic-lens or generic-optics with 'dvbSubSourceSettings' instead." #-}

-- | Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
--
-- /Note:/ Consider using 'trackSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTrackSourceSettings :: Lens.Lens' CaptionSourceSettings (Lude.Maybe TrackSourceSettings)
cssTrackSourceSettings = Lens.lens (trackSourceSettings :: CaptionSourceSettings -> Lude.Maybe TrackSourceSettings) (\s a -> s {trackSourceSettings = a} :: CaptionSourceSettings)
{-# DEPRECATED cssTrackSourceSettings "Use generic-lens or generic-optics with 'trackSourceSettings' instead." #-}

-- | Settings for ancillary captions source.
--
-- /Note:/ Consider using 'ancillarySourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAncillarySourceSettings :: Lens.Lens' CaptionSourceSettings (Lude.Maybe AncillarySourceSettings)
cssAncillarySourceSettings = Lens.lens (ancillarySourceSettings :: CaptionSourceSettings -> Lude.Maybe AncillarySourceSettings) (\s a -> s {ancillarySourceSettings = a} :: CaptionSourceSettings)
{-# DEPRECATED cssAncillarySourceSettings "Use generic-lens or generic-optics with 'ancillarySourceSettings' instead." #-}

-- | Settings for embedded captions Source
--
-- /Note:/ Consider using 'embeddedSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssEmbeddedSourceSettings :: Lens.Lens' CaptionSourceSettings (Lude.Maybe EmbeddedSourceSettings)
cssEmbeddedSourceSettings = Lens.lens (embeddedSourceSettings :: CaptionSourceSettings -> Lude.Maybe EmbeddedSourceSettings) (\s a -> s {embeddedSourceSettings = a} :: CaptionSourceSettings)
{-# DEPRECATED cssEmbeddedSourceSettings "Use generic-lens or generic-optics with 'embeddedSourceSettings' instead." #-}

instance Lude.FromJSON CaptionSourceSettings where
  parseJSON =
    Lude.withObject
      "CaptionSourceSettings"
      ( \x ->
          CaptionSourceSettings'
            Lude.<$> (x Lude..:? "teletextSourceSettings")
            Lude.<*> (x Lude..:? "sourceType")
            Lude.<*> (x Lude..:? "fileSourceSettings")
            Lude.<*> (x Lude..:? "dvbSubSourceSettings")
            Lude.<*> (x Lude..:? "trackSourceSettings")
            Lude.<*> (x Lude..:? "ancillarySourceSettings")
            Lude.<*> (x Lude..:? "embeddedSourceSettings")
      )

instance Lude.ToJSON CaptionSourceSettings where
  toJSON CaptionSourceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("teletextSourceSettings" Lude..=)
              Lude.<$> teletextSourceSettings,
            ("sourceType" Lude..=) Lude.<$> sourceType,
            ("fileSourceSettings" Lude..=) Lude.<$> fileSourceSettings,
            ("dvbSubSourceSettings" Lude..=) Lude.<$> dvbSubSourceSettings,
            ("trackSourceSettings" Lude..=) Lude.<$> trackSourceSettings,
            ("ancillarySourceSettings" Lude..=)
              Lude.<$> ancillarySourceSettings,
            ("embeddedSourceSettings" Lude..=)
              Lude.<$> embeddedSourceSettings
          ]
      )
