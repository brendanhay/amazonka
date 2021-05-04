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
-- Module      : Network.AWS.MediaConvert.Types.CmfcSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmfcSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmfcAudioDuration
import Network.AWS.MediaConvert.Types.CmfcIFrameOnlyManifest
import Network.AWS.MediaConvert.Types.CmfcScte35Esam
import Network.AWS.MediaConvert.Types.CmfcScte35Source
import qualified Network.AWS.Prelude as Prelude

-- | Settings for MP4 segments in CMAF
--
-- /See:/ 'newCmfcSettings' smart constructor.
data CmfcSettings = CmfcSettings'
  { -- | Choose Include (INCLUDE) to have MediaConvert generate an HLS child
    -- manifest that lists only the I-frames for this rendition, in addition to
    -- your regular manifest for this rendition. You might use this manifest as
    -- part of a workflow that creates preview functions for your video.
    -- MediaConvert adds both the I-frame only child manifest and the regular
    -- child manifest to the parent manifest. When you don\'t need the I-frame
    -- only child manifest, keep the default value Exclude (EXCLUDE).
    iFrameOnlyManifest :: Prelude.Maybe CmfcIFrameOnlyManifest,
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
    audioDuration :: Prelude.Maybe CmfcAudioDuration,
    -- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
    -- INSERT to put SCTE-35 markers in this output at the insertion points
    -- that you specify in an ESAM XML document. Provide the document in the
    -- setting SCC XML (sccXml).
    scte35Esam :: Prelude.Maybe CmfcScte35Esam,
    -- | Ignore this setting unless you have SCTE-35 markers in your input video
    -- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
    -- appear in your input to also appear in this output. Choose None (NONE)
    -- if you don\'t want those SCTE-35 markers in this output.
    scte35Source :: Prelude.Maybe CmfcScte35Source
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CmfcSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iFrameOnlyManifest', 'cmfcSettings_iFrameOnlyManifest' - Choose Include (INCLUDE) to have MediaConvert generate an HLS child
-- manifest that lists only the I-frames for this rendition, in addition to
-- your regular manifest for this rendition. You might use this manifest as
-- part of a workflow that creates preview functions for your video.
-- MediaConvert adds both the I-frame only child manifest and the regular
-- child manifest to the parent manifest. When you don\'t need the I-frame
-- only child manifest, keep the default value Exclude (EXCLUDE).
--
-- 'audioDuration', 'cmfcSettings_audioDuration' - Specify this setting only when your output will be consumed by a
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
-- 'scte35Esam', 'cmfcSettings_scte35Esam' - Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
--
-- 'scte35Source', 'cmfcSettings_scte35Source' - Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
newCmfcSettings ::
  CmfcSettings
newCmfcSettings =
  CmfcSettings'
    { iFrameOnlyManifest = Prelude.Nothing,
      audioDuration = Prelude.Nothing,
      scte35Esam = Prelude.Nothing,
      scte35Source = Prelude.Nothing
    }

-- | Choose Include (INCLUDE) to have MediaConvert generate an HLS child
-- manifest that lists only the I-frames for this rendition, in addition to
-- your regular manifest for this rendition. You might use this manifest as
-- part of a workflow that creates preview functions for your video.
-- MediaConvert adds both the I-frame only child manifest and the regular
-- child manifest to the parent manifest. When you don\'t need the I-frame
-- only child manifest, keep the default value Exclude (EXCLUDE).
cmfcSettings_iFrameOnlyManifest :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcIFrameOnlyManifest)
cmfcSettings_iFrameOnlyManifest = Lens.lens (\CmfcSettings' {iFrameOnlyManifest} -> iFrameOnlyManifest) (\s@CmfcSettings' {} a -> s {iFrameOnlyManifest = a} :: CmfcSettings)

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
cmfcSettings_audioDuration :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcAudioDuration)
cmfcSettings_audioDuration = Lens.lens (\CmfcSettings' {audioDuration} -> audioDuration) (\s@CmfcSettings' {} a -> s {audioDuration = a} :: CmfcSettings)

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
cmfcSettings_scte35Esam :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcScte35Esam)
cmfcSettings_scte35Esam = Lens.lens (\CmfcSettings' {scte35Esam} -> scte35Esam) (\s@CmfcSettings' {} a -> s {scte35Esam = a} :: CmfcSettings)

-- | Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
cmfcSettings_scte35Source :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcScte35Source)
cmfcSettings_scte35Source = Lens.lens (\CmfcSettings' {scte35Source} -> scte35Source) (\s@CmfcSettings' {} a -> s {scte35Source = a} :: CmfcSettings)

instance Prelude.FromJSON CmfcSettings where
  parseJSON =
    Prelude.withObject
      "CmfcSettings"
      ( \x ->
          CmfcSettings'
            Prelude.<$> (x Prelude..:? "iFrameOnlyManifest")
            Prelude.<*> (x Prelude..:? "audioDuration")
            Prelude.<*> (x Prelude..:? "scte35Esam")
            Prelude.<*> (x Prelude..:? "scte35Source")
      )

instance Prelude.Hashable CmfcSettings

instance Prelude.NFData CmfcSettings

instance Prelude.ToJSON CmfcSettings where
  toJSON CmfcSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("iFrameOnlyManifest" Prelude..=)
              Prelude.<$> iFrameOnlyManifest,
            ("audioDuration" Prelude..=)
              Prelude.<$> audioDuration,
            ("scte35Esam" Prelude..=) Prelude.<$> scte35Esam,
            ("scte35Source" Prelude..=)
              Prelude.<$> scte35Source
          ]
      )
