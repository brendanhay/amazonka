{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionSourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionSourceSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AncillarySourceSettings
import Network.AWS.MediaConvert.Types.CaptionSourceType
import Network.AWS.MediaConvert.Types.DvbSubSourceSettings
import Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
import Network.AWS.MediaConvert.Types.FileSourceSettings
import Network.AWS.MediaConvert.Types.TeletextSourceSettings
import Network.AWS.MediaConvert.Types.TrackSourceSettings
import qualified Network.AWS.Prelude as Prelude

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml
-- file, specify the URI of the input captions source file. If your input
-- captions are IMSC in an IMF package, use TrackSourceSettings instead of
-- FileSoureSettings.
--
-- /See:/ 'newCaptionSourceSettings' smart constructor.
data CaptionSourceSettings = CaptionSourceSettings'
  { -- | Settings for ancillary captions source.
    ancillarySourceSettings :: Prelude.Maybe AncillarySourceSettings,
    -- | Settings specific to caption sources that are specified by track number.
    -- Currently, this is only IMSC captions in an IMF package. If your caption
    -- source is IMSC 1.1 in a separate xml file, use FileSourceSettings
    -- instead of TrackSourceSettings.
    trackSourceSettings :: Prelude.Maybe TrackSourceSettings,
    -- | Settings for embedded captions Source
    embeddedSourceSettings :: Prelude.Maybe EmbeddedSourceSettings,
    -- | DVB Sub Source Settings
    dvbSubSourceSettings :: Prelude.Maybe DvbSubSourceSettings,
    -- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an
    -- xml file, specify the URI of the input caption source file. If your
    -- caption source is IMSC in an IMF package, use TrackSourceSettings
    -- instead of FileSoureSettings.
    fileSourceSettings :: Prelude.Maybe FileSourceSettings,
    -- | Settings specific to Teletext caption sources, including Page number.
    teletextSourceSettings :: Prelude.Maybe TeletextSourceSettings,
    -- | Use Source (SourceType) to identify the format of your input captions.
    -- The service cannot auto-detect caption format.
    sourceType :: Prelude.Maybe CaptionSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CaptionSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ancillarySourceSettings', 'captionSourceSettings_ancillarySourceSettings' - Settings for ancillary captions source.
--
-- 'trackSourceSettings', 'captionSourceSettings_trackSourceSettings' - Settings specific to caption sources that are specified by track number.
-- Currently, this is only IMSC captions in an IMF package. If your caption
-- source is IMSC 1.1 in a separate xml file, use FileSourceSettings
-- instead of TrackSourceSettings.
--
-- 'embeddedSourceSettings', 'captionSourceSettings_embeddedSourceSettings' - Settings for embedded captions Source
--
-- 'dvbSubSourceSettings', 'captionSourceSettings_dvbSubSourceSettings' - DVB Sub Source Settings
--
-- 'fileSourceSettings', 'captionSourceSettings_fileSourceSettings' - If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an
-- xml file, specify the URI of the input caption source file. If your
-- caption source is IMSC in an IMF package, use TrackSourceSettings
-- instead of FileSoureSettings.
--
-- 'teletextSourceSettings', 'captionSourceSettings_teletextSourceSettings' - Settings specific to Teletext caption sources, including Page number.
--
-- 'sourceType', 'captionSourceSettings_sourceType' - Use Source (SourceType) to identify the format of your input captions.
-- The service cannot auto-detect caption format.
newCaptionSourceSettings ::
  CaptionSourceSettings
newCaptionSourceSettings =
  CaptionSourceSettings'
    { ancillarySourceSettings =
        Prelude.Nothing,
      trackSourceSettings = Prelude.Nothing,
      embeddedSourceSettings = Prelude.Nothing,
      dvbSubSourceSettings = Prelude.Nothing,
      fileSourceSettings = Prelude.Nothing,
      teletextSourceSettings = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | Settings for ancillary captions source.
captionSourceSettings_ancillarySourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe AncillarySourceSettings)
captionSourceSettings_ancillarySourceSettings = Lens.lens (\CaptionSourceSettings' {ancillarySourceSettings} -> ancillarySourceSettings) (\s@CaptionSourceSettings' {} a -> s {ancillarySourceSettings = a} :: CaptionSourceSettings)

-- | Settings specific to caption sources that are specified by track number.
-- Currently, this is only IMSC captions in an IMF package. If your caption
-- source is IMSC 1.1 in a separate xml file, use FileSourceSettings
-- instead of TrackSourceSettings.
captionSourceSettings_trackSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe TrackSourceSettings)
captionSourceSettings_trackSourceSettings = Lens.lens (\CaptionSourceSettings' {trackSourceSettings} -> trackSourceSettings) (\s@CaptionSourceSettings' {} a -> s {trackSourceSettings = a} :: CaptionSourceSettings)

-- | Settings for embedded captions Source
captionSourceSettings_embeddedSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe EmbeddedSourceSettings)
captionSourceSettings_embeddedSourceSettings = Lens.lens (\CaptionSourceSettings' {embeddedSourceSettings} -> embeddedSourceSettings) (\s@CaptionSourceSettings' {} a -> s {embeddedSourceSettings = a} :: CaptionSourceSettings)

-- | DVB Sub Source Settings
captionSourceSettings_dvbSubSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe DvbSubSourceSettings)
captionSourceSettings_dvbSubSourceSettings = Lens.lens (\CaptionSourceSettings' {dvbSubSourceSettings} -> dvbSubSourceSettings) (\s@CaptionSourceSettings' {} a -> s {dvbSubSourceSettings = a} :: CaptionSourceSettings)

-- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an
-- xml file, specify the URI of the input caption source file. If your
-- caption source is IMSC in an IMF package, use TrackSourceSettings
-- instead of FileSoureSettings.
captionSourceSettings_fileSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe FileSourceSettings)
captionSourceSettings_fileSourceSettings = Lens.lens (\CaptionSourceSettings' {fileSourceSettings} -> fileSourceSettings) (\s@CaptionSourceSettings' {} a -> s {fileSourceSettings = a} :: CaptionSourceSettings)

-- | Settings specific to Teletext caption sources, including Page number.
captionSourceSettings_teletextSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe TeletextSourceSettings)
captionSourceSettings_teletextSourceSettings = Lens.lens (\CaptionSourceSettings' {teletextSourceSettings} -> teletextSourceSettings) (\s@CaptionSourceSettings' {} a -> s {teletextSourceSettings = a} :: CaptionSourceSettings)

-- | Use Source (SourceType) to identify the format of your input captions.
-- The service cannot auto-detect caption format.
captionSourceSettings_sourceType :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe CaptionSourceType)
captionSourceSettings_sourceType = Lens.lens (\CaptionSourceSettings' {sourceType} -> sourceType) (\s@CaptionSourceSettings' {} a -> s {sourceType = a} :: CaptionSourceSettings)

instance Prelude.FromJSON CaptionSourceSettings where
  parseJSON =
    Prelude.withObject
      "CaptionSourceSettings"
      ( \x ->
          CaptionSourceSettings'
            Prelude.<$> (x Prelude..:? "ancillarySourceSettings")
            Prelude.<*> (x Prelude..:? "trackSourceSettings")
            Prelude.<*> (x Prelude..:? "embeddedSourceSettings")
            Prelude.<*> (x Prelude..:? "dvbSubSourceSettings")
            Prelude.<*> (x Prelude..:? "fileSourceSettings")
            Prelude.<*> (x Prelude..:? "teletextSourceSettings")
            Prelude.<*> (x Prelude..:? "sourceType")
      )

instance Prelude.Hashable CaptionSourceSettings

instance Prelude.NFData CaptionSourceSettings

instance Prelude.ToJSON CaptionSourceSettings where
  toJSON CaptionSourceSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ancillarySourceSettings" Prelude..=)
              Prelude.<$> ancillarySourceSettings,
            ("trackSourceSettings" Prelude..=)
              Prelude.<$> trackSourceSettings,
            ("embeddedSourceSettings" Prelude..=)
              Prelude.<$> embeddedSourceSettings,
            ("dvbSubSourceSettings" Prelude..=)
              Prelude.<$> dvbSubSourceSettings,
            ("fileSourceSettings" Prelude..=)
              Prelude.<$> fileSourceSettings,
            ("teletextSourceSettings" Prelude..=)
              Prelude.<$> teletextSourceSettings,
            ("sourceType" Prelude..=) Prelude.<$> sourceType
          ]
      )
