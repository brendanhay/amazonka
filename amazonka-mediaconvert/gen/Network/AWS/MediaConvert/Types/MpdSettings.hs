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
-- Module      : Network.AWS.MediaConvert.Types.MpdSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
import Network.AWS.MediaConvert.Types.MpdAudioDuration
import Network.AWS.MediaConvert.Types.MpdCaptionContainerType
import Network.AWS.MediaConvert.Types.MpdScte35Esam
import Network.AWS.MediaConvert.Types.MpdScte35Source
import qualified Network.AWS.Prelude as Prelude

-- | Settings for MP4 segments in DASH
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
    -- | Use this setting only in DASH output groups that include sidecar TTML or
    -- IMSC captions. You specify sidecar captions in a separate output from
    -- your audio and video. Choose Raw (RAW) for captions in a single XML file
    -- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
    -- captions in XML format contained within fragmented MP4 files. This set
    -- of fragmented MP4 files is separate from your video and audio fragmented
    -- MP4 files.
    captionContainerType :: Prelude.Maybe MpdCaptionContainerType,
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
    -- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
    -- INSERT to put SCTE-35 markers in this output at the insertion points
    -- that you specify in an ESAM XML document. Provide the document in the
    -- setting SCC XML (sccXml).
    scte35Esam :: Prelude.Maybe MpdScte35Esam,
    -- | Ignore this setting unless you have SCTE-35 markers in your input video
    -- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
    -- appear in your input to also appear in this output. Choose None (NONE)
    -- if you don\'t want those SCTE-35 markers in this output.
    scte35Source :: Prelude.Maybe MpdScte35Source
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'captionContainerType', 'mpdSettings_captionContainerType' - Use this setting only in DASH output groups that include sidecar TTML or
-- IMSC captions. You specify sidecar captions in a separate output from
-- your audio and video. Choose Raw (RAW) for captions in a single XML file
-- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
-- captions in XML format contained within fragmented MP4 files. This set
-- of fragmented MP4 files is separate from your video and audio fragmented
-- MP4 files.
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
-- 'scte35Esam', 'mpdSettings_scte35Esam' - Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
--
-- 'scte35Source', 'mpdSettings_scte35Source' - Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
newMpdSettings ::
  MpdSettings
newMpdSettings =
  MpdSettings'
    { accessibilityCaptionHints =
        Prelude.Nothing,
      captionContainerType = Prelude.Nothing,
      audioDuration = Prelude.Nothing,
      scte35Esam = Prelude.Nothing,
      scte35Source = Prelude.Nothing
    }

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

instance Prelude.FromJSON MpdSettings where
  parseJSON =
    Prelude.withObject
      "MpdSettings"
      ( \x ->
          MpdSettings'
            Prelude.<$> (x Prelude..:? "accessibilityCaptionHints")
            Prelude.<*> (x Prelude..:? "captionContainerType")
            Prelude.<*> (x Prelude..:? "audioDuration")
            Prelude.<*> (x Prelude..:? "scte35Esam")
            Prelude.<*> (x Prelude..:? "scte35Source")
      )

instance Prelude.Hashable MpdSettings

instance Prelude.NFData MpdSettings

instance Prelude.ToJSON MpdSettings where
  toJSON MpdSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("accessibilityCaptionHints" Prelude..=)
              Prelude.<$> accessibilityCaptionHints,
            ("captionContainerType" Prelude..=)
              Prelude.<$> captionContainerType,
            ("audioDuration" Prelude..=)
              Prelude.<$> audioDuration,
            ("scte35Esam" Prelude..=) Prelude.<$> scte35Esam,
            ("scte35Source" Prelude..=)
              Prelude.<$> scte35Source
          ]
      )
