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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MpdSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.MpdAccessibilityCaptionHints
import Amazonka.MediaConvert.Types.MpdAudioDuration
import Amazonka.MediaConvert.Types.MpdCaptionContainerType
import Amazonka.MediaConvert.Types.MpdKlvMetadata
import Amazonka.MediaConvert.Types.MpdScte35Esam
import Amazonka.MediaConvert.Types.MpdScte35Source
import Amazonka.MediaConvert.Types.MpdTimedMetadata
import qualified Amazonka.Prelude as Prelude

-- | These settings relate to the fragmented MP4 container for the segments
-- in your DASH outputs.
--
-- /See:/ 'newMpdSettings' smart constructor.
data MpdSettings = MpdSettings'
  { -- | Specify this setting only when your output will be consumed by a
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
    -- | To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
    -- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
    -- metadata inserter (timedMetadataInsertion). MediaConvert writes each
    -- instance of ID3 metadata in a separate Event Message (eMSG) box. To
    -- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
    -- blank.
    timedMetadata :: Prelude.Maybe MpdTimedMetadata,
    -- | To include key-length-value metadata in this output: Set KLV metadata
    -- insertion to Passthrough. MediaConvert reads KLV metadata present in
    -- your input and writes each instance to a separate event message box in
    -- the output, according to MISB ST1910.1. To exclude this KLV metadata:
    -- Set KLV metadata insertion to None or leave blank.
    klvMetadata :: Prelude.Maybe MpdKlvMetadata,
    -- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
    -- INSERT to put SCTE-35 markers in this output at the insertion points
    -- that you specify in an ESAM XML document. Provide the document in the
    -- setting SCC XML (sccXml).
    scte35Esam :: Prelude.Maybe MpdScte35Esam,
    -- | Optional. Choose Include (INCLUDE) to have MediaConvert mark up your
    -- DASH manifest with elements for embedded 608 captions. This markup
    -- isn\'t generally required, but some video players require it to discover
    -- and play embedded 608 captions. Keep the default value, Exclude
    -- (EXCLUDE), to leave these elements out. When you enable this setting,
    -- this is the markup that MediaConvert includes in your manifest:
    accessibilityCaptionHints :: Prelude.Maybe MpdAccessibilityCaptionHints,
    -- | Use this setting only in DASH output groups that include sidecar TTML or
    -- IMSC captions. You specify sidecar captions in a separate output from
    -- your audio and video. Choose Raw (RAW) for captions in a single XML file
    -- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
    -- captions in XML format contained within fragmented MP4 files. This set
    -- of fragmented MP4 files is separate from your video and audio fragmented
    -- MP4 files.
    captionContainerType :: Prelude.Maybe MpdCaptionContainerType,
    -- | Ignore this setting unless you have SCTE-35 markers in your input video
    -- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
    -- appear in your input to also appear in this output. Choose None (NONE)
    -- if you don\'t want those SCTE-35 markers in this output.
    scte35Source :: Prelude.Maybe MpdScte35Source
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
-- 'timedMetadata', 'mpdSettings_timedMetadata' - To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
-- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
-- metadata inserter (timedMetadataInsertion). MediaConvert writes each
-- instance of ID3 metadata in a separate Event Message (eMSG) box. To
-- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
-- blank.
--
-- 'klvMetadata', 'mpdSettings_klvMetadata' - To include key-length-value metadata in this output: Set KLV metadata
-- insertion to Passthrough. MediaConvert reads KLV metadata present in
-- your input and writes each instance to a separate event message box in
-- the output, according to MISB ST1910.1. To exclude this KLV metadata:
-- Set KLV metadata insertion to None or leave blank.
--
-- 'scte35Esam', 'mpdSettings_scte35Esam' - Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
--
-- 'accessibilityCaptionHints', 'mpdSettings_accessibilityCaptionHints' - Optional. Choose Include (INCLUDE) to have MediaConvert mark up your
-- DASH manifest with elements for embedded 608 captions. This markup
-- isn\'t generally required, but some video players require it to discover
-- and play embedded 608 captions. Keep the default value, Exclude
-- (EXCLUDE), to leave these elements out. When you enable this setting,
-- this is the markup that MediaConvert includes in your manifest:
--
-- 'captionContainerType', 'mpdSettings_captionContainerType' - Use this setting only in DASH output groups that include sidecar TTML or
-- IMSC captions. You specify sidecar captions in a separate output from
-- your audio and video. Choose Raw (RAW) for captions in a single XML file
-- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
-- captions in XML format contained within fragmented MP4 files. This set
-- of fragmented MP4 files is separate from your video and audio fragmented
-- MP4 files.
--
-- 'scte35Source', 'mpdSettings_scte35Source' - Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
newMpdSettings ::
  MpdSettings
newMpdSettings =
  MpdSettings'
    { audioDuration = Prelude.Nothing,
      timedMetadata = Prelude.Nothing,
      klvMetadata = Prelude.Nothing,
      scte35Esam = Prelude.Nothing,
      accessibilityCaptionHints = Prelude.Nothing,
      captionContainerType = Prelude.Nothing,
      scte35Source = Prelude.Nothing
    }

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

-- | To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
-- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
-- metadata inserter (timedMetadataInsertion). MediaConvert writes each
-- instance of ID3 metadata in a separate Event Message (eMSG) box. To
-- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
-- blank.
mpdSettings_timedMetadata :: Lens.Lens' MpdSettings (Prelude.Maybe MpdTimedMetadata)
mpdSettings_timedMetadata = Lens.lens (\MpdSettings' {timedMetadata} -> timedMetadata) (\s@MpdSettings' {} a -> s {timedMetadata = a} :: MpdSettings)

-- | To include key-length-value metadata in this output: Set KLV metadata
-- insertion to Passthrough. MediaConvert reads KLV metadata present in
-- your input and writes each instance to a separate event message box in
-- the output, according to MISB ST1910.1. To exclude this KLV metadata:
-- Set KLV metadata insertion to None or leave blank.
mpdSettings_klvMetadata :: Lens.Lens' MpdSettings (Prelude.Maybe MpdKlvMetadata)
mpdSettings_klvMetadata = Lens.lens (\MpdSettings' {klvMetadata} -> klvMetadata) (\s@MpdSettings' {} a -> s {klvMetadata = a} :: MpdSettings)

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
mpdSettings_scte35Esam :: Lens.Lens' MpdSettings (Prelude.Maybe MpdScte35Esam)
mpdSettings_scte35Esam = Lens.lens (\MpdSettings' {scte35Esam} -> scte35Esam) (\s@MpdSettings' {} a -> s {scte35Esam = a} :: MpdSettings)

-- | Optional. Choose Include (INCLUDE) to have MediaConvert mark up your
-- DASH manifest with elements for embedded 608 captions. This markup
-- isn\'t generally required, but some video players require it to discover
-- and play embedded 608 captions. Keep the default value, Exclude
-- (EXCLUDE), to leave these elements out. When you enable this setting,
-- this is the markup that MediaConvert includes in your manifest:
mpdSettings_accessibilityCaptionHints :: Lens.Lens' MpdSettings (Prelude.Maybe MpdAccessibilityCaptionHints)
mpdSettings_accessibilityCaptionHints = Lens.lens (\MpdSettings' {accessibilityCaptionHints} -> accessibilityCaptionHints) (\s@MpdSettings' {} a -> s {accessibilityCaptionHints = a} :: MpdSettings)

-- | Use this setting only in DASH output groups that include sidecar TTML or
-- IMSC captions. You specify sidecar captions in a separate output from
-- your audio and video. Choose Raw (RAW) for captions in a single XML file
-- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
-- captions in XML format contained within fragmented MP4 files. This set
-- of fragmented MP4 files is separate from your video and audio fragmented
-- MP4 files.
mpdSettings_captionContainerType :: Lens.Lens' MpdSettings (Prelude.Maybe MpdCaptionContainerType)
mpdSettings_captionContainerType = Lens.lens (\MpdSettings' {captionContainerType} -> captionContainerType) (\s@MpdSettings' {} a -> s {captionContainerType = a} :: MpdSettings)

-- | Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
mpdSettings_scte35Source :: Lens.Lens' MpdSettings (Prelude.Maybe MpdScte35Source)
mpdSettings_scte35Source = Lens.lens (\MpdSettings' {scte35Source} -> scte35Source) (\s@MpdSettings' {} a -> s {scte35Source = a} :: MpdSettings)

instance Core.FromJSON MpdSettings where
  parseJSON =
    Core.withObject
      "MpdSettings"
      ( \x ->
          MpdSettings'
            Prelude.<$> (x Core..:? "audioDuration")
            Prelude.<*> (x Core..:? "timedMetadata")
            Prelude.<*> (x Core..:? "klvMetadata")
            Prelude.<*> (x Core..:? "scte35Esam")
            Prelude.<*> (x Core..:? "accessibilityCaptionHints")
            Prelude.<*> (x Core..:? "captionContainerType")
            Prelude.<*> (x Core..:? "scte35Source")
      )

instance Prelude.Hashable MpdSettings where
  hashWithSalt _salt MpdSettings' {..} =
    _salt `Prelude.hashWithSalt` audioDuration
      `Prelude.hashWithSalt` timedMetadata
      `Prelude.hashWithSalt` klvMetadata
      `Prelude.hashWithSalt` scte35Esam
      `Prelude.hashWithSalt` accessibilityCaptionHints
      `Prelude.hashWithSalt` captionContainerType
      `Prelude.hashWithSalt` scte35Source

instance Prelude.NFData MpdSettings where
  rnf MpdSettings' {..} =
    Prelude.rnf audioDuration
      `Prelude.seq` Prelude.rnf timedMetadata
      `Prelude.seq` Prelude.rnf klvMetadata
      `Prelude.seq` Prelude.rnf scte35Esam
      `Prelude.seq` Prelude.rnf accessibilityCaptionHints
      `Prelude.seq` Prelude.rnf captionContainerType
      `Prelude.seq` Prelude.rnf scte35Source

instance Core.ToJSON MpdSettings where
  toJSON MpdSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("audioDuration" Core..=) Prelude.<$> audioDuration,
            ("timedMetadata" Core..=) Prelude.<$> timedMetadata,
            ("klvMetadata" Core..=) Prelude.<$> klvMetadata,
            ("scte35Esam" Core..=) Prelude.<$> scte35Esam,
            ("accessibilityCaptionHints" Core..=)
              Prelude.<$> accessibilityCaptionHints,
            ("captionContainerType" Core..=)
              Prelude.<$> captionContainerType,
            ("scte35Source" Core..=) Prelude.<$> scte35Source
          ]
      )
