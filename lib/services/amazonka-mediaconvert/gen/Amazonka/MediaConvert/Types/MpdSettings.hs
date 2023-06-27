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
-- Module      : Amazonka.MediaConvert.Types.MpdSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MpdSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.MpdAccessibilityCaptionHints
import Amazonka.MediaConvert.Types.MpdAudioDuration
import Amazonka.MediaConvert.Types.MpdCaptionContainerType
import Amazonka.MediaConvert.Types.MpdKlvMetadata
import Amazonka.MediaConvert.Types.MpdManifestMetadataSignaling
import Amazonka.MediaConvert.Types.MpdScte35Esam
import Amazonka.MediaConvert.Types.MpdScte35Source
import Amazonka.MediaConvert.Types.MpdTimedMetadata
import Amazonka.MediaConvert.Types.MpdTimedMetadataBoxVersion
import qualified Amazonka.Prelude as Prelude

-- | These settings relate to the fragmented MP4 container for the segments
-- in your DASH outputs.
--
-- /See:/ 'newMpdSettings' smart constructor.
data MpdSettings = MpdSettings'
  { -- | Optional. Choose Include (INCLUDE) to have MediaConvert mark up your
    -- DASH manifest with elements for embedded 608 captions. This markup
    -- isn\'t generally required, but some video players require it to discover
    -- and play embedded 608 captions. Keep the default value, Exclude
    -- (EXCLUDE), to leave these elements out. When you enable this setting,
    -- this is the markup that MediaConvert includes in your manifest:
    accessibilityCaptionHints :: Prelude.Maybe MpdAccessibilityCaptionHints,
    -- | Specify this setting only when your output will be consumed by a
    -- downstream repackaging workflow that is sensitive to very small duration
    -- differences between video and audio. For this situation, choose Match
    -- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
    -- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
    -- choose Match video duration, MediaConvert pads the output audio streams
    -- with silence or trims them to ensure that the total duration of each
    -- audio stream is at least as long as the total duration of the video
    -- stream. After padding or trimming, the audio stream duration is no more
    -- than one frame longer than the video stream. MediaConvert applies audio
    -- padding or trimming only to the end of the last segment of the output.
    -- For unsegmented outputs, MediaConvert adds padding only to the end of
    -- the file. When you keep the default value, any minor discrepancies
    -- between audio and video duration will depend on your output audio codec.
    audioDuration :: Prelude.Maybe MpdAudioDuration,
    -- | Use this setting only in DASH output groups that include sidecar TTML or
    -- IMSC captions. You specify sidecar captions in a separate output from
    -- your audio and video. Choose Raw (RAW) for captions in a single XML file
    -- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
    -- captions in XML format contained within fragmented MP4 files. This set
    -- of fragmented MP4 files is separate from your video and audio fragmented
    -- MP4 files.
    captionContainerType :: Prelude.Maybe MpdCaptionContainerType,
    -- | To include key-length-value metadata in this output: Set KLV metadata
    -- insertion to Passthrough. MediaConvert reads KLV metadata present in
    -- your input and writes each instance to a separate event message box in
    -- the output, according to MISB ST1910.1. To exclude this KLV metadata:
    -- Set KLV metadata insertion to None or leave blank.
    klvMetadata :: Prelude.Maybe MpdKlvMetadata,
    -- | To add an InbandEventStream element in your output MPD manifest for each
    -- type of event message, set Manifest metadata signaling to Enabled. For
    -- ID3 event messages, the InbandEventStream element schemeIdUri will be
    -- same value that you specify for ID3 metadata scheme ID URI. For SCTE35
    -- event messages, the InbandEventStream element schemeIdUri will be
    -- \"urn:scte:scte35:2013:bin\". To leave these elements out of your output
    -- MPD manifest, set Manifest metadata signaling to Disabled. To enable
    -- Manifest metadata signaling, you must also set SCTE-35 source to
    -- Passthrough, ESAM SCTE-35 to insert, or ID3 metadata (TimedMetadata) to
    -- Passthrough.
    manifestMetadataSignaling :: Prelude.Maybe MpdManifestMetadataSignaling,
    -- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
    -- INSERT to put SCTE-35 markers in this output at the insertion points
    -- that you specify in an ESAM XML document. Provide the document in the
    -- setting SCC XML (sccXml).
    scte35Esam :: Prelude.Maybe MpdScte35Esam,
    -- | Ignore this setting unless you have SCTE-35 markers in your input video
    -- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
    -- appear in your input to also appear in this output. Choose None (NONE)
    -- if you don\'t want those SCTE-35 markers in this output.
    scte35Source :: Prelude.Maybe MpdScte35Source,
    -- | To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
    -- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
    -- metadata inserter (timedMetadataInsertion). MediaConvert writes each
    -- instance of ID3 metadata in a separate Event Message (eMSG) box. To
    -- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
    -- blank.
    timedMetadata :: Prelude.Maybe MpdTimedMetadata,
    -- | Specify the event message box (eMSG) version for ID3 timed metadata in
    -- your output. For more information, see ISO\/IEC 23009-1:2022 section
    -- 5.10.3.3.3 Syntax. Leave blank to use the default value Version 0. When
    -- you specify Version 1, you must also set ID3 metadata (timedMetadata) to
    -- Passthrough.
    timedMetadataBoxVersion :: Prelude.Maybe MpdTimedMetadataBoxVersion,
    -- | Specify the event message box (eMSG) scheme ID URI (scheme_id_uri) for
    -- ID3 timed metadata in your output. For more information, see ISO\/IEC
    -- 23009-1:2022 section 5.10.3.3.4 Semantics. Leave blank to use the
    -- default value: https:\/\/aomedia.org\/emsg\/ID3 When you specify a value
    -- for ID3 metadata scheme ID URI, you must also set ID3 metadata
    -- (timedMetadata) to Passthrough.
    timedMetadataSchemeIdUri :: Prelude.Maybe Prelude.Text,
    -- | Specify the event message box (eMSG) value for ID3 timed metadata in
    -- your output. For more information, see ISO\/IEC 23009-1:2022 section
    -- 5.10.3.3.4 Semantics. When you specify a value for ID3 Metadata Value,
    -- you must also set ID3 metadata (timedMetadata) to Passthrough.
    timedMetadataValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MpdSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessibilityCaptionHints', 'mpdSettings_accessibilityCaptionHints' - Optional. Choose Include (INCLUDE) to have MediaConvert mark up your
-- DASH manifest with elements for embedded 608 captions. This markup
-- isn\'t generally required, but some video players require it to discover
-- and play embedded 608 captions. Keep the default value, Exclude
-- (EXCLUDE), to leave these elements out. When you enable this setting,
-- this is the markup that MediaConvert includes in your manifest:
--
-- 'audioDuration', 'mpdSettings_audioDuration' - Specify this setting only when your output will be consumed by a
-- downstream repackaging workflow that is sensitive to very small duration
-- differences between video and audio. For this situation, choose Match
-- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
-- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
-- choose Match video duration, MediaConvert pads the output audio streams
-- with silence or trims them to ensure that the total duration of each
-- audio stream is at least as long as the total duration of the video
-- stream. After padding or trimming, the audio stream duration is no more
-- than one frame longer than the video stream. MediaConvert applies audio
-- padding or trimming only to the end of the last segment of the output.
-- For unsegmented outputs, MediaConvert adds padding only to the end of
-- the file. When you keep the default value, any minor discrepancies
-- between audio and video duration will depend on your output audio codec.
--
-- 'captionContainerType', 'mpdSettings_captionContainerType' - Use this setting only in DASH output groups that include sidecar TTML or
-- IMSC captions. You specify sidecar captions in a separate output from
-- your audio and video. Choose Raw (RAW) for captions in a single XML file
-- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
-- captions in XML format contained within fragmented MP4 files. This set
-- of fragmented MP4 files is separate from your video and audio fragmented
-- MP4 files.
--
-- 'klvMetadata', 'mpdSettings_klvMetadata' - To include key-length-value metadata in this output: Set KLV metadata
-- insertion to Passthrough. MediaConvert reads KLV metadata present in
-- your input and writes each instance to a separate event message box in
-- the output, according to MISB ST1910.1. To exclude this KLV metadata:
-- Set KLV metadata insertion to None or leave blank.
--
-- 'manifestMetadataSignaling', 'mpdSettings_manifestMetadataSignaling' - To add an InbandEventStream element in your output MPD manifest for each
-- type of event message, set Manifest metadata signaling to Enabled. For
-- ID3 event messages, the InbandEventStream element schemeIdUri will be
-- same value that you specify for ID3 metadata scheme ID URI. For SCTE35
-- event messages, the InbandEventStream element schemeIdUri will be
-- \"urn:scte:scte35:2013:bin\". To leave these elements out of your output
-- MPD manifest, set Manifest metadata signaling to Disabled. To enable
-- Manifest metadata signaling, you must also set SCTE-35 source to
-- Passthrough, ESAM SCTE-35 to insert, or ID3 metadata (TimedMetadata) to
-- Passthrough.
--
-- 'scte35Esam', 'mpdSettings_scte35Esam' - Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
--
-- 'scte35Source', 'mpdSettings_scte35Source' - Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
--
-- 'timedMetadata', 'mpdSettings_timedMetadata' - To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
-- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
-- metadata inserter (timedMetadataInsertion). MediaConvert writes each
-- instance of ID3 metadata in a separate Event Message (eMSG) box. To
-- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
-- blank.
--
-- 'timedMetadataBoxVersion', 'mpdSettings_timedMetadataBoxVersion' - Specify the event message box (eMSG) version for ID3 timed metadata in
-- your output. For more information, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.3 Syntax. Leave blank to use the default value Version 0. When
-- you specify Version 1, you must also set ID3 metadata (timedMetadata) to
-- Passthrough.
--
-- 'timedMetadataSchemeIdUri', 'mpdSettings_timedMetadataSchemeIdUri' - Specify the event message box (eMSG) scheme ID URI (scheme_id_uri) for
-- ID3 timed metadata in your output. For more information, see ISO\/IEC
-- 23009-1:2022 section 5.10.3.3.4 Semantics. Leave blank to use the
-- default value: https:\/\/aomedia.org\/emsg\/ID3 When you specify a value
-- for ID3 metadata scheme ID URI, you must also set ID3 metadata
-- (timedMetadata) to Passthrough.
--
-- 'timedMetadataValue', 'mpdSettings_timedMetadataValue' - Specify the event message box (eMSG) value for ID3 timed metadata in
-- your output. For more information, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.4 Semantics. When you specify a value for ID3 Metadata Value,
-- you must also set ID3 metadata (timedMetadata) to Passthrough.
newMpdSettings ::
  MpdSettings
newMpdSettings =
  MpdSettings'
    { accessibilityCaptionHints =
        Prelude.Nothing,
      audioDuration = Prelude.Nothing,
      captionContainerType = Prelude.Nothing,
      klvMetadata = Prelude.Nothing,
      manifestMetadataSignaling = Prelude.Nothing,
      scte35Esam = Prelude.Nothing,
      scte35Source = Prelude.Nothing,
      timedMetadata = Prelude.Nothing,
      timedMetadataBoxVersion = Prelude.Nothing,
      timedMetadataSchemeIdUri = Prelude.Nothing,
      timedMetadataValue = Prelude.Nothing
    }

-- | Optional. Choose Include (INCLUDE) to have MediaConvert mark up your
-- DASH manifest with elements for embedded 608 captions. This markup
-- isn\'t generally required, but some video players require it to discover
-- and play embedded 608 captions. Keep the default value, Exclude
-- (EXCLUDE), to leave these elements out. When you enable this setting,
-- this is the markup that MediaConvert includes in your manifest:
mpdSettings_accessibilityCaptionHints :: Lens.Lens' MpdSettings (Prelude.Maybe MpdAccessibilityCaptionHints)
mpdSettings_accessibilityCaptionHints = Lens.lens (\MpdSettings' {accessibilityCaptionHints} -> accessibilityCaptionHints) (\s@MpdSettings' {} a -> s {accessibilityCaptionHints = a} :: MpdSettings)

-- | Specify this setting only when your output will be consumed by a
-- downstream repackaging workflow that is sensitive to very small duration
-- differences between video and audio. For this situation, choose Match
-- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
-- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
-- choose Match video duration, MediaConvert pads the output audio streams
-- with silence or trims them to ensure that the total duration of each
-- audio stream is at least as long as the total duration of the video
-- stream. After padding or trimming, the audio stream duration is no more
-- than one frame longer than the video stream. MediaConvert applies audio
-- padding or trimming only to the end of the last segment of the output.
-- For unsegmented outputs, MediaConvert adds padding only to the end of
-- the file. When you keep the default value, any minor discrepancies
-- between audio and video duration will depend on your output audio codec.
mpdSettings_audioDuration :: Lens.Lens' MpdSettings (Prelude.Maybe MpdAudioDuration)
mpdSettings_audioDuration = Lens.lens (\MpdSettings' {audioDuration} -> audioDuration) (\s@MpdSettings' {} a -> s {audioDuration = a} :: MpdSettings)

-- | Use this setting only in DASH output groups that include sidecar TTML or
-- IMSC captions. You specify sidecar captions in a separate output from
-- your audio and video. Choose Raw (RAW) for captions in a single XML file
-- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
-- captions in XML format contained within fragmented MP4 files. This set
-- of fragmented MP4 files is separate from your video and audio fragmented
-- MP4 files.
mpdSettings_captionContainerType :: Lens.Lens' MpdSettings (Prelude.Maybe MpdCaptionContainerType)
mpdSettings_captionContainerType = Lens.lens (\MpdSettings' {captionContainerType} -> captionContainerType) (\s@MpdSettings' {} a -> s {captionContainerType = a} :: MpdSettings)

-- | To include key-length-value metadata in this output: Set KLV metadata
-- insertion to Passthrough. MediaConvert reads KLV metadata present in
-- your input and writes each instance to a separate event message box in
-- the output, according to MISB ST1910.1. To exclude this KLV metadata:
-- Set KLV metadata insertion to None or leave blank.
mpdSettings_klvMetadata :: Lens.Lens' MpdSettings (Prelude.Maybe MpdKlvMetadata)
mpdSettings_klvMetadata = Lens.lens (\MpdSettings' {klvMetadata} -> klvMetadata) (\s@MpdSettings' {} a -> s {klvMetadata = a} :: MpdSettings)

-- | To add an InbandEventStream element in your output MPD manifest for each
-- type of event message, set Manifest metadata signaling to Enabled. For
-- ID3 event messages, the InbandEventStream element schemeIdUri will be
-- same value that you specify for ID3 metadata scheme ID URI. For SCTE35
-- event messages, the InbandEventStream element schemeIdUri will be
-- \"urn:scte:scte35:2013:bin\". To leave these elements out of your output
-- MPD manifest, set Manifest metadata signaling to Disabled. To enable
-- Manifest metadata signaling, you must also set SCTE-35 source to
-- Passthrough, ESAM SCTE-35 to insert, or ID3 metadata (TimedMetadata) to
-- Passthrough.
mpdSettings_manifestMetadataSignaling :: Lens.Lens' MpdSettings (Prelude.Maybe MpdManifestMetadataSignaling)
mpdSettings_manifestMetadataSignaling = Lens.lens (\MpdSettings' {manifestMetadataSignaling} -> manifestMetadataSignaling) (\s@MpdSettings' {} a -> s {manifestMetadataSignaling = a} :: MpdSettings)

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
mpdSettings_scte35Esam :: Lens.Lens' MpdSettings (Prelude.Maybe MpdScte35Esam)
mpdSettings_scte35Esam = Lens.lens (\MpdSettings' {scte35Esam} -> scte35Esam) (\s@MpdSettings' {} a -> s {scte35Esam = a} :: MpdSettings)

-- | Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
mpdSettings_scte35Source :: Lens.Lens' MpdSettings (Prelude.Maybe MpdScte35Source)
mpdSettings_scte35Source = Lens.lens (\MpdSettings' {scte35Source} -> scte35Source) (\s@MpdSettings' {} a -> s {scte35Source = a} :: MpdSettings)

-- | To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
-- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
-- metadata inserter (timedMetadataInsertion). MediaConvert writes each
-- instance of ID3 metadata in a separate Event Message (eMSG) box. To
-- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
-- blank.
mpdSettings_timedMetadata :: Lens.Lens' MpdSettings (Prelude.Maybe MpdTimedMetadata)
mpdSettings_timedMetadata = Lens.lens (\MpdSettings' {timedMetadata} -> timedMetadata) (\s@MpdSettings' {} a -> s {timedMetadata = a} :: MpdSettings)

-- | Specify the event message box (eMSG) version for ID3 timed metadata in
-- your output. For more information, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.3 Syntax. Leave blank to use the default value Version 0. When
-- you specify Version 1, you must also set ID3 metadata (timedMetadata) to
-- Passthrough.
mpdSettings_timedMetadataBoxVersion :: Lens.Lens' MpdSettings (Prelude.Maybe MpdTimedMetadataBoxVersion)
mpdSettings_timedMetadataBoxVersion = Lens.lens (\MpdSettings' {timedMetadataBoxVersion} -> timedMetadataBoxVersion) (\s@MpdSettings' {} a -> s {timedMetadataBoxVersion = a} :: MpdSettings)

-- | Specify the event message box (eMSG) scheme ID URI (scheme_id_uri) for
-- ID3 timed metadata in your output. For more information, see ISO\/IEC
-- 23009-1:2022 section 5.10.3.3.4 Semantics. Leave blank to use the
-- default value: https:\/\/aomedia.org\/emsg\/ID3 When you specify a value
-- for ID3 metadata scheme ID URI, you must also set ID3 metadata
-- (timedMetadata) to Passthrough.
mpdSettings_timedMetadataSchemeIdUri :: Lens.Lens' MpdSettings (Prelude.Maybe Prelude.Text)
mpdSettings_timedMetadataSchemeIdUri = Lens.lens (\MpdSettings' {timedMetadataSchemeIdUri} -> timedMetadataSchemeIdUri) (\s@MpdSettings' {} a -> s {timedMetadataSchemeIdUri = a} :: MpdSettings)

-- | Specify the event message box (eMSG) value for ID3 timed metadata in
-- your output. For more information, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.4 Semantics. When you specify a value for ID3 Metadata Value,
-- you must also set ID3 metadata (timedMetadata) to Passthrough.
mpdSettings_timedMetadataValue :: Lens.Lens' MpdSettings (Prelude.Maybe Prelude.Text)
mpdSettings_timedMetadataValue = Lens.lens (\MpdSettings' {timedMetadataValue} -> timedMetadataValue) (\s@MpdSettings' {} a -> s {timedMetadataValue = a} :: MpdSettings)

instance Data.FromJSON MpdSettings where
  parseJSON =
    Data.withObject
      "MpdSettings"
      ( \x ->
          MpdSettings'
            Prelude.<$> (x Data..:? "accessibilityCaptionHints")
            Prelude.<*> (x Data..:? "audioDuration")
            Prelude.<*> (x Data..:? "captionContainerType")
            Prelude.<*> (x Data..:? "klvMetadata")
            Prelude.<*> (x Data..:? "manifestMetadataSignaling")
            Prelude.<*> (x Data..:? "scte35Esam")
            Prelude.<*> (x Data..:? "scte35Source")
            Prelude.<*> (x Data..:? "timedMetadata")
            Prelude.<*> (x Data..:? "timedMetadataBoxVersion")
            Prelude.<*> (x Data..:? "timedMetadataSchemeIdUri")
            Prelude.<*> (x Data..:? "timedMetadataValue")
      )

instance Prelude.Hashable MpdSettings where
  hashWithSalt _salt MpdSettings' {..} =
    _salt
      `Prelude.hashWithSalt` accessibilityCaptionHints
      `Prelude.hashWithSalt` audioDuration
      `Prelude.hashWithSalt` captionContainerType
      `Prelude.hashWithSalt` klvMetadata
      `Prelude.hashWithSalt` manifestMetadataSignaling
      `Prelude.hashWithSalt` scte35Esam
      `Prelude.hashWithSalt` scte35Source
      `Prelude.hashWithSalt` timedMetadata
      `Prelude.hashWithSalt` timedMetadataBoxVersion
      `Prelude.hashWithSalt` timedMetadataSchemeIdUri
      `Prelude.hashWithSalt` timedMetadataValue

instance Prelude.NFData MpdSettings where
  rnf MpdSettings' {..} =
    Prelude.rnf accessibilityCaptionHints
      `Prelude.seq` Prelude.rnf audioDuration
      `Prelude.seq` Prelude.rnf captionContainerType
      `Prelude.seq` Prelude.rnf klvMetadata
      `Prelude.seq` Prelude.rnf manifestMetadataSignaling
      `Prelude.seq` Prelude.rnf scte35Esam
      `Prelude.seq` Prelude.rnf scte35Source
      `Prelude.seq` Prelude.rnf timedMetadata
      `Prelude.seq` Prelude.rnf timedMetadataBoxVersion
      `Prelude.seq` Prelude.rnf timedMetadataSchemeIdUri
      `Prelude.seq` Prelude.rnf timedMetadataValue

instance Data.ToJSON MpdSettings where
  toJSON MpdSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessibilityCaptionHints" Data..=)
              Prelude.<$> accessibilityCaptionHints,
            ("audioDuration" Data..=) Prelude.<$> audioDuration,
            ("captionContainerType" Data..=)
              Prelude.<$> captionContainerType,
            ("klvMetadata" Data..=) Prelude.<$> klvMetadata,
            ("manifestMetadataSignaling" Data..=)
              Prelude.<$> manifestMetadataSignaling,
            ("scte35Esam" Data..=) Prelude.<$> scte35Esam,
            ("scte35Source" Data..=) Prelude.<$> scte35Source,
            ("timedMetadata" Data..=) Prelude.<$> timedMetadata,
            ("timedMetadataBoxVersion" Data..=)
              Prelude.<$> timedMetadataBoxVersion,
            ("timedMetadataSchemeIdUri" Data..=)
              Prelude.<$> timedMetadataSchemeIdUri,
            ("timedMetadataValue" Data..=)
              Prelude.<$> timedMetadataValue
          ]
      )
