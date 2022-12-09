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
-- Module      : Amazonka.MediaConvert.Types.CaptionSourceSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CaptionSourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AncillarySourceSettings
import Amazonka.MediaConvert.Types.CaptionSourceType
import Amazonka.MediaConvert.Types.DvbSubSourceSettings
import Amazonka.MediaConvert.Types.EmbeddedSourceSettings
import Amazonka.MediaConvert.Types.FileSourceSettings
import Amazonka.MediaConvert.Types.TeletextSourceSettings
import Amazonka.MediaConvert.Types.TrackSourceSettings
import Amazonka.MediaConvert.Types.WebvttHlsSourceSettings
import qualified Amazonka.Prelude as Prelude

-- | If your input captions are SCC, TTML, STL, SMI, SRT, or IMSC in an xml
-- file, specify the URI of the input captions source file. If your input
-- captions are IMSC in an IMF package, use TrackSourceSettings instead of
-- FileSoureSettings.
--
-- /See:/ 'newCaptionSourceSettings' smart constructor.
data CaptionSourceSettings = CaptionSourceSettings'
  { -- | Settings for ancillary captions source.
    ancillarySourceSettings :: Prelude.Maybe AncillarySourceSettings,
    -- | DVB Sub Source Settings
    dvbSubSourceSettings :: Prelude.Maybe DvbSubSourceSettings,
    -- | Settings for embedded captions Source
    embeddedSourceSettings :: Prelude.Maybe EmbeddedSourceSettings,
    -- | If your input captions are SCC, SMI, SRT, STL, TTML, WebVTT, or IMSC 1.1
    -- in an xml file, specify the URI of the input caption source file. If
    -- your caption source is IMSC in an IMF package, use TrackSourceSettings
    -- instead of FileSoureSettings.
    fileSourceSettings :: Prelude.Maybe FileSourceSettings,
    -- | Use Source (SourceType) to identify the format of your input captions.
    -- The service cannot auto-detect caption format.
    sourceType :: Prelude.Maybe CaptionSourceType,
    -- | Settings specific to Teletext caption sources, including Page number.
    teletextSourceSettings :: Prelude.Maybe TeletextSourceSettings,
    -- | Settings specific to caption sources that are specified by track number.
    -- Currently, this is only IMSC captions in an IMF package. If your caption
    -- source is IMSC 1.1 in a separate xml file, use FileSourceSettings
    -- instead of TrackSourceSettings.
    trackSourceSettings :: Prelude.Maybe TrackSourceSettings,
    -- | Settings specific to WebVTT sources in HLS alternative rendition group.
    -- Specify the properties (renditionGroupId, renditionName or
    -- renditionLanguageCode) to identify the unique subtitle track among the
    -- alternative rendition groups present in the HLS manifest. If no unique
    -- track is found, or multiple tracks match the specified properties, the
    -- job fails. If there is only one subtitle track in the rendition group,
    -- the settings can be left empty and the default subtitle track will be
    -- chosen. If your caption source is a sidecar file, use FileSourceSettings
    -- instead of WebvttHlsSourceSettings.
    webvttHlsSourceSettings :: Prelude.Maybe WebvttHlsSourceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'dvbSubSourceSettings', 'captionSourceSettings_dvbSubSourceSettings' - DVB Sub Source Settings
--
-- 'embeddedSourceSettings', 'captionSourceSettings_embeddedSourceSettings' - Settings for embedded captions Source
--
-- 'fileSourceSettings', 'captionSourceSettings_fileSourceSettings' - If your input captions are SCC, SMI, SRT, STL, TTML, WebVTT, or IMSC 1.1
-- in an xml file, specify the URI of the input caption source file. If
-- your caption source is IMSC in an IMF package, use TrackSourceSettings
-- instead of FileSoureSettings.
--
-- 'sourceType', 'captionSourceSettings_sourceType' - Use Source (SourceType) to identify the format of your input captions.
-- The service cannot auto-detect caption format.
--
-- 'teletextSourceSettings', 'captionSourceSettings_teletextSourceSettings' - Settings specific to Teletext caption sources, including Page number.
--
-- 'trackSourceSettings', 'captionSourceSettings_trackSourceSettings' - Settings specific to caption sources that are specified by track number.
-- Currently, this is only IMSC captions in an IMF package. If your caption
-- source is IMSC 1.1 in a separate xml file, use FileSourceSettings
-- instead of TrackSourceSettings.
--
-- 'webvttHlsSourceSettings', 'captionSourceSettings_webvttHlsSourceSettings' - Settings specific to WebVTT sources in HLS alternative rendition group.
-- Specify the properties (renditionGroupId, renditionName or
-- renditionLanguageCode) to identify the unique subtitle track among the
-- alternative rendition groups present in the HLS manifest. If no unique
-- track is found, or multiple tracks match the specified properties, the
-- job fails. If there is only one subtitle track in the rendition group,
-- the settings can be left empty and the default subtitle track will be
-- chosen. If your caption source is a sidecar file, use FileSourceSettings
-- instead of WebvttHlsSourceSettings.
newCaptionSourceSettings ::
  CaptionSourceSettings
newCaptionSourceSettings =
  CaptionSourceSettings'
    { ancillarySourceSettings =
        Prelude.Nothing,
      dvbSubSourceSettings = Prelude.Nothing,
      embeddedSourceSettings = Prelude.Nothing,
      fileSourceSettings = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      teletextSourceSettings = Prelude.Nothing,
      trackSourceSettings = Prelude.Nothing,
      webvttHlsSourceSettings = Prelude.Nothing
    }

-- | Settings for ancillary captions source.
captionSourceSettings_ancillarySourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe AncillarySourceSettings)
captionSourceSettings_ancillarySourceSettings = Lens.lens (\CaptionSourceSettings' {ancillarySourceSettings} -> ancillarySourceSettings) (\s@CaptionSourceSettings' {} a -> s {ancillarySourceSettings = a} :: CaptionSourceSettings)

-- | DVB Sub Source Settings
captionSourceSettings_dvbSubSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe DvbSubSourceSettings)
captionSourceSettings_dvbSubSourceSettings = Lens.lens (\CaptionSourceSettings' {dvbSubSourceSettings} -> dvbSubSourceSettings) (\s@CaptionSourceSettings' {} a -> s {dvbSubSourceSettings = a} :: CaptionSourceSettings)

-- | Settings for embedded captions Source
captionSourceSettings_embeddedSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe EmbeddedSourceSettings)
captionSourceSettings_embeddedSourceSettings = Lens.lens (\CaptionSourceSettings' {embeddedSourceSettings} -> embeddedSourceSettings) (\s@CaptionSourceSettings' {} a -> s {embeddedSourceSettings = a} :: CaptionSourceSettings)

-- | If your input captions are SCC, SMI, SRT, STL, TTML, WebVTT, or IMSC 1.1
-- in an xml file, specify the URI of the input caption source file. If
-- your caption source is IMSC in an IMF package, use TrackSourceSettings
-- instead of FileSoureSettings.
captionSourceSettings_fileSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe FileSourceSettings)
captionSourceSettings_fileSourceSettings = Lens.lens (\CaptionSourceSettings' {fileSourceSettings} -> fileSourceSettings) (\s@CaptionSourceSettings' {} a -> s {fileSourceSettings = a} :: CaptionSourceSettings)

-- | Use Source (SourceType) to identify the format of your input captions.
-- The service cannot auto-detect caption format.
captionSourceSettings_sourceType :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe CaptionSourceType)
captionSourceSettings_sourceType = Lens.lens (\CaptionSourceSettings' {sourceType} -> sourceType) (\s@CaptionSourceSettings' {} a -> s {sourceType = a} :: CaptionSourceSettings)

-- | Settings specific to Teletext caption sources, including Page number.
captionSourceSettings_teletextSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe TeletextSourceSettings)
captionSourceSettings_teletextSourceSettings = Lens.lens (\CaptionSourceSettings' {teletextSourceSettings} -> teletextSourceSettings) (\s@CaptionSourceSettings' {} a -> s {teletextSourceSettings = a} :: CaptionSourceSettings)

-- | Settings specific to caption sources that are specified by track number.
-- Currently, this is only IMSC captions in an IMF package. If your caption
-- source is IMSC 1.1 in a separate xml file, use FileSourceSettings
-- instead of TrackSourceSettings.
captionSourceSettings_trackSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe TrackSourceSettings)
captionSourceSettings_trackSourceSettings = Lens.lens (\CaptionSourceSettings' {trackSourceSettings} -> trackSourceSettings) (\s@CaptionSourceSettings' {} a -> s {trackSourceSettings = a} :: CaptionSourceSettings)

-- | Settings specific to WebVTT sources in HLS alternative rendition group.
-- Specify the properties (renditionGroupId, renditionName or
-- renditionLanguageCode) to identify the unique subtitle track among the
-- alternative rendition groups present in the HLS manifest. If no unique
-- track is found, or multiple tracks match the specified properties, the
-- job fails. If there is only one subtitle track in the rendition group,
-- the settings can be left empty and the default subtitle track will be
-- chosen. If your caption source is a sidecar file, use FileSourceSettings
-- instead of WebvttHlsSourceSettings.
captionSourceSettings_webvttHlsSourceSettings :: Lens.Lens' CaptionSourceSettings (Prelude.Maybe WebvttHlsSourceSettings)
captionSourceSettings_webvttHlsSourceSettings = Lens.lens (\CaptionSourceSettings' {webvttHlsSourceSettings} -> webvttHlsSourceSettings) (\s@CaptionSourceSettings' {} a -> s {webvttHlsSourceSettings = a} :: CaptionSourceSettings)

instance Data.FromJSON CaptionSourceSettings where
  parseJSON =
    Data.withObject
      "CaptionSourceSettings"
      ( \x ->
          CaptionSourceSettings'
            Prelude.<$> (x Data..:? "ancillarySourceSettings")
            Prelude.<*> (x Data..:? "dvbSubSourceSettings")
            Prelude.<*> (x Data..:? "embeddedSourceSettings")
            Prelude.<*> (x Data..:? "fileSourceSettings")
            Prelude.<*> (x Data..:? "sourceType")
            Prelude.<*> (x Data..:? "teletextSourceSettings")
            Prelude.<*> (x Data..:? "trackSourceSettings")
            Prelude.<*> (x Data..:? "webvttHlsSourceSettings")
      )

instance Prelude.Hashable CaptionSourceSettings where
  hashWithSalt _salt CaptionSourceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` ancillarySourceSettings
      `Prelude.hashWithSalt` dvbSubSourceSettings
      `Prelude.hashWithSalt` embeddedSourceSettings
      `Prelude.hashWithSalt` fileSourceSettings
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` teletextSourceSettings
      `Prelude.hashWithSalt` trackSourceSettings
      `Prelude.hashWithSalt` webvttHlsSourceSettings

instance Prelude.NFData CaptionSourceSettings where
  rnf CaptionSourceSettings' {..} =
    Prelude.rnf ancillarySourceSettings
      `Prelude.seq` Prelude.rnf dvbSubSourceSettings
      `Prelude.seq` Prelude.rnf embeddedSourceSettings
      `Prelude.seq` Prelude.rnf fileSourceSettings
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf teletextSourceSettings
      `Prelude.seq` Prelude.rnf trackSourceSettings
      `Prelude.seq` Prelude.rnf webvttHlsSourceSettings

instance Data.ToJSON CaptionSourceSettings where
  toJSON CaptionSourceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ancillarySourceSettings" Data..=)
              Prelude.<$> ancillarySourceSettings,
            ("dvbSubSourceSettings" Data..=)
              Prelude.<$> dvbSubSourceSettings,
            ("embeddedSourceSettings" Data..=)
              Prelude.<$> embeddedSourceSettings,
            ("fileSourceSettings" Data..=)
              Prelude.<$> fileSourceSettings,
            ("sourceType" Data..=) Prelude.<$> sourceType,
            ("teletextSourceSettings" Data..=)
              Prelude.<$> teletextSourceSettings,
            ("trackSourceSettings" Data..=)
              Prelude.<$> trackSourceSettings,
            ("webvttHlsSourceSettings" Data..=)
              Prelude.<$> webvttHlsSourceSettings
          ]
      )
