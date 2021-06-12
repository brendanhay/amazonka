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
-- Module      : Network.AWS.MediaConvert.Types.Mp4Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmfcAudioDuration
import Network.AWS.MediaConvert.Types.Mp4CslgAtom
import Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
import Network.AWS.MediaConvert.Types.Mp4MoovPlacement

-- | Settings for MP4 container. You can create audio-only AAC outputs with
-- this container.
--
-- /See:/ 'newMp4Settings' smart constructor.
data Mp4Settings = Mp4Settings'
  { -- | When enabled, file composition times will start at zero, composition
    -- times in the \'ctts\' (composition time to sample) box for B-frames will
    -- be negative, and a \'cslg\' (composition shift least greatest) box will
    -- be included per 14496-1 amendment 1. This improves compatibility with
    -- Apple players and tools.
    cslgAtom :: Core.Maybe Mp4CslgAtom,
    -- | Overrides the \"Major Brand\" field in the output file. Usually not
    -- necessary to specify.
    mp4MajorBrand :: Core.Maybe Core.Text,
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
    audioDuration :: Core.Maybe CmfcAudioDuration,
    -- | Inserts a free-space box immediately after the moov box.
    freeSpaceBox :: Core.Maybe Mp4FreeSpaceBox,
    -- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
    -- beginning of the archive as required for progressive downloading.
    -- Otherwise it is placed normally at the end.
    moovPlacement :: Core.Maybe Mp4MoovPlacement,
    -- | Ignore this setting unless compliance to the CTTS box version
    -- specification matters in your workflow. Specify a value of 1 to set your
    -- CTTS box version to 1 and make your output compliant with the
    -- specification. When you specify a value of 1, you must also set CSLG
    -- atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set
    -- your CTTS box version to 0. This can provide backward compatibility for
    -- some players and packagers.
    cttsVersion :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Mp4Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cslgAtom', 'mp4Settings_cslgAtom' - When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
--
-- 'mp4MajorBrand', 'mp4Settings_mp4MajorBrand' - Overrides the \"Major Brand\" field in the output file. Usually not
-- necessary to specify.
--
-- 'audioDuration', 'mp4Settings_audioDuration' - Specify this setting only when your output will be consumed by a
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
-- 'freeSpaceBox', 'mp4Settings_freeSpaceBox' - Inserts a free-space box immediately after the moov box.
--
-- 'moovPlacement', 'mp4Settings_moovPlacement' - If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
--
-- 'cttsVersion', 'mp4Settings_cttsVersion' - Ignore this setting unless compliance to the CTTS box version
-- specification matters in your workflow. Specify a value of 1 to set your
-- CTTS box version to 1 and make your output compliant with the
-- specification. When you specify a value of 1, you must also set CSLG
-- atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set
-- your CTTS box version to 0. This can provide backward compatibility for
-- some players and packagers.
newMp4Settings ::
  Mp4Settings
newMp4Settings =
  Mp4Settings'
    { cslgAtom = Core.Nothing,
      mp4MajorBrand = Core.Nothing,
      audioDuration = Core.Nothing,
      freeSpaceBox = Core.Nothing,
      moovPlacement = Core.Nothing,
      cttsVersion = Core.Nothing
    }

-- | When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
mp4Settings_cslgAtom :: Lens.Lens' Mp4Settings (Core.Maybe Mp4CslgAtom)
mp4Settings_cslgAtom = Lens.lens (\Mp4Settings' {cslgAtom} -> cslgAtom) (\s@Mp4Settings' {} a -> s {cslgAtom = a} :: Mp4Settings)

-- | Overrides the \"Major Brand\" field in the output file. Usually not
-- necessary to specify.
mp4Settings_mp4MajorBrand :: Lens.Lens' Mp4Settings (Core.Maybe Core.Text)
mp4Settings_mp4MajorBrand = Lens.lens (\Mp4Settings' {mp4MajorBrand} -> mp4MajorBrand) (\s@Mp4Settings' {} a -> s {mp4MajorBrand = a} :: Mp4Settings)

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
mp4Settings_audioDuration :: Lens.Lens' Mp4Settings (Core.Maybe CmfcAudioDuration)
mp4Settings_audioDuration = Lens.lens (\Mp4Settings' {audioDuration} -> audioDuration) (\s@Mp4Settings' {} a -> s {audioDuration = a} :: Mp4Settings)

-- | Inserts a free-space box immediately after the moov box.
mp4Settings_freeSpaceBox :: Lens.Lens' Mp4Settings (Core.Maybe Mp4FreeSpaceBox)
mp4Settings_freeSpaceBox = Lens.lens (\Mp4Settings' {freeSpaceBox} -> freeSpaceBox) (\s@Mp4Settings' {} a -> s {freeSpaceBox = a} :: Mp4Settings)

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
mp4Settings_moovPlacement :: Lens.Lens' Mp4Settings (Core.Maybe Mp4MoovPlacement)
mp4Settings_moovPlacement = Lens.lens (\Mp4Settings' {moovPlacement} -> moovPlacement) (\s@Mp4Settings' {} a -> s {moovPlacement = a} :: Mp4Settings)

-- | Ignore this setting unless compliance to the CTTS box version
-- specification matters in your workflow. Specify a value of 1 to set your
-- CTTS box version to 1 and make your output compliant with the
-- specification. When you specify a value of 1, you must also set CSLG
-- atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set
-- your CTTS box version to 0. This can provide backward compatibility for
-- some players and packagers.
mp4Settings_cttsVersion :: Lens.Lens' Mp4Settings (Core.Maybe Core.Natural)
mp4Settings_cttsVersion = Lens.lens (\Mp4Settings' {cttsVersion} -> cttsVersion) (\s@Mp4Settings' {} a -> s {cttsVersion = a} :: Mp4Settings)

instance Core.FromJSON Mp4Settings where
  parseJSON =
    Core.withObject
      "Mp4Settings"
      ( \x ->
          Mp4Settings'
            Core.<$> (x Core..:? "cslgAtom")
            Core.<*> (x Core..:? "mp4MajorBrand")
            Core.<*> (x Core..:? "audioDuration")
            Core.<*> (x Core..:? "freeSpaceBox")
            Core.<*> (x Core..:? "moovPlacement")
            Core.<*> (x Core..:? "cttsVersion")
      )

instance Core.Hashable Mp4Settings

instance Core.NFData Mp4Settings

instance Core.ToJSON Mp4Settings where
  toJSON Mp4Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("cslgAtom" Core..=) Core.<$> cslgAtom,
            ("mp4MajorBrand" Core..=) Core.<$> mp4MajorBrand,
            ("audioDuration" Core..=) Core.<$> audioDuration,
            ("freeSpaceBox" Core..=) Core.<$> freeSpaceBox,
            ("moovPlacement" Core..=) Core.<$> moovPlacement,
            ("cttsVersion" Core..=) Core.<$> cttsVersion
          ]
      )
