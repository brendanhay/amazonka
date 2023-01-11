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
-- Module      : Amazonka.MediaConvert.Types.Mp4Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mp4Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.CmfcAudioDuration
import Amazonka.MediaConvert.Types.Mp4CslgAtom
import Amazonka.MediaConvert.Types.Mp4FreeSpaceBox
import Amazonka.MediaConvert.Types.Mp4MoovPlacement
import qualified Amazonka.Prelude as Prelude

-- | These settings relate to your MP4 output container. You can create audio
-- only outputs with this container. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/supported-codecs-containers-audio-only.html#output-codecs-and-containers-supported-for-audio-only.
--
-- /See:/ 'newMp4Settings' smart constructor.
data Mp4Settings = Mp4Settings'
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
    audioDuration :: Prelude.Maybe CmfcAudioDuration,
    -- | When enabled, file composition times will start at zero, composition
    -- times in the \'ctts\' (composition time to sample) box for B-frames will
    -- be negative, and a \'cslg\' (composition shift least greatest) box will
    -- be included per 14496-1 amendment 1. This improves compatibility with
    -- Apple players and tools.
    cslgAtom :: Prelude.Maybe Mp4CslgAtom,
    -- | Ignore this setting unless compliance to the CTTS box version
    -- specification matters in your workflow. Specify a value of 1 to set your
    -- CTTS box version to 1 and make your output compliant with the
    -- specification. When you specify a value of 1, you must also set CSLG
    -- atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set
    -- your CTTS box version to 0. This can provide backward compatibility for
    -- some players and packagers.
    cttsVersion :: Prelude.Maybe Prelude.Natural,
    -- | Inserts a free-space box immediately after the moov box.
    freeSpaceBox :: Prelude.Maybe Mp4FreeSpaceBox,
    -- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
    -- beginning of the archive as required for progressive downloading.
    -- Otherwise it is placed normally at the end.
    moovPlacement :: Prelude.Maybe Mp4MoovPlacement,
    -- | Overrides the \"Major Brand\" field in the output file. Usually not
    -- necessary to specify.
    mp4MajorBrand :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Mp4Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'cslgAtom', 'mp4Settings_cslgAtom' - When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
--
-- 'cttsVersion', 'mp4Settings_cttsVersion' - Ignore this setting unless compliance to the CTTS box version
-- specification matters in your workflow. Specify a value of 1 to set your
-- CTTS box version to 1 and make your output compliant with the
-- specification. When you specify a value of 1, you must also set CSLG
-- atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set
-- your CTTS box version to 0. This can provide backward compatibility for
-- some players and packagers.
--
-- 'freeSpaceBox', 'mp4Settings_freeSpaceBox' - Inserts a free-space box immediately after the moov box.
--
-- 'moovPlacement', 'mp4Settings_moovPlacement' - If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
--
-- 'mp4MajorBrand', 'mp4Settings_mp4MajorBrand' - Overrides the \"Major Brand\" field in the output file. Usually not
-- necessary to specify.
newMp4Settings ::
  Mp4Settings
newMp4Settings =
  Mp4Settings'
    { audioDuration = Prelude.Nothing,
      cslgAtom = Prelude.Nothing,
      cttsVersion = Prelude.Nothing,
      freeSpaceBox = Prelude.Nothing,
      moovPlacement = Prelude.Nothing,
      mp4MajorBrand = Prelude.Nothing
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
mp4Settings_audioDuration :: Lens.Lens' Mp4Settings (Prelude.Maybe CmfcAudioDuration)
mp4Settings_audioDuration = Lens.lens (\Mp4Settings' {audioDuration} -> audioDuration) (\s@Mp4Settings' {} a -> s {audioDuration = a} :: Mp4Settings)

-- | When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
mp4Settings_cslgAtom :: Lens.Lens' Mp4Settings (Prelude.Maybe Mp4CslgAtom)
mp4Settings_cslgAtom = Lens.lens (\Mp4Settings' {cslgAtom} -> cslgAtom) (\s@Mp4Settings' {} a -> s {cslgAtom = a} :: Mp4Settings)

-- | Ignore this setting unless compliance to the CTTS box version
-- specification matters in your workflow. Specify a value of 1 to set your
-- CTTS box version to 1 and make your output compliant with the
-- specification. When you specify a value of 1, you must also set CSLG
-- atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set
-- your CTTS box version to 0. This can provide backward compatibility for
-- some players and packagers.
mp4Settings_cttsVersion :: Lens.Lens' Mp4Settings (Prelude.Maybe Prelude.Natural)
mp4Settings_cttsVersion = Lens.lens (\Mp4Settings' {cttsVersion} -> cttsVersion) (\s@Mp4Settings' {} a -> s {cttsVersion = a} :: Mp4Settings)

-- | Inserts a free-space box immediately after the moov box.
mp4Settings_freeSpaceBox :: Lens.Lens' Mp4Settings (Prelude.Maybe Mp4FreeSpaceBox)
mp4Settings_freeSpaceBox = Lens.lens (\Mp4Settings' {freeSpaceBox} -> freeSpaceBox) (\s@Mp4Settings' {} a -> s {freeSpaceBox = a} :: Mp4Settings)

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
mp4Settings_moovPlacement :: Lens.Lens' Mp4Settings (Prelude.Maybe Mp4MoovPlacement)
mp4Settings_moovPlacement = Lens.lens (\Mp4Settings' {moovPlacement} -> moovPlacement) (\s@Mp4Settings' {} a -> s {moovPlacement = a} :: Mp4Settings)

-- | Overrides the \"Major Brand\" field in the output file. Usually not
-- necessary to specify.
mp4Settings_mp4MajorBrand :: Lens.Lens' Mp4Settings (Prelude.Maybe Prelude.Text)
mp4Settings_mp4MajorBrand = Lens.lens (\Mp4Settings' {mp4MajorBrand} -> mp4MajorBrand) (\s@Mp4Settings' {} a -> s {mp4MajorBrand = a} :: Mp4Settings)

instance Data.FromJSON Mp4Settings where
  parseJSON =
    Data.withObject
      "Mp4Settings"
      ( \x ->
          Mp4Settings'
            Prelude.<$> (x Data..:? "audioDuration")
            Prelude.<*> (x Data..:? "cslgAtom")
            Prelude.<*> (x Data..:? "cttsVersion")
            Prelude.<*> (x Data..:? "freeSpaceBox")
            Prelude.<*> (x Data..:? "moovPlacement")
            Prelude.<*> (x Data..:? "mp4MajorBrand")
      )

instance Prelude.Hashable Mp4Settings where
  hashWithSalt _salt Mp4Settings' {..} =
    _salt `Prelude.hashWithSalt` audioDuration
      `Prelude.hashWithSalt` cslgAtom
      `Prelude.hashWithSalt` cttsVersion
      `Prelude.hashWithSalt` freeSpaceBox
      `Prelude.hashWithSalt` moovPlacement
      `Prelude.hashWithSalt` mp4MajorBrand

instance Prelude.NFData Mp4Settings where
  rnf Mp4Settings' {..} =
    Prelude.rnf audioDuration
      `Prelude.seq` Prelude.rnf cslgAtom
      `Prelude.seq` Prelude.rnf cttsVersion
      `Prelude.seq` Prelude.rnf freeSpaceBox
      `Prelude.seq` Prelude.rnf moovPlacement
      `Prelude.seq` Prelude.rnf mp4MajorBrand

instance Data.ToJSON Mp4Settings where
  toJSON Mp4Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioDuration" Data..=) Prelude.<$> audioDuration,
            ("cslgAtom" Data..=) Prelude.<$> cslgAtom,
            ("cttsVersion" Data..=) Prelude.<$> cttsVersion,
            ("freeSpaceBox" Data..=) Prelude.<$> freeSpaceBox,
            ("moovPlacement" Data..=) Prelude.<$> moovPlacement,
            ("mp4MajorBrand" Data..=) Prelude.<$> mp4MajorBrand
          ]
      )
