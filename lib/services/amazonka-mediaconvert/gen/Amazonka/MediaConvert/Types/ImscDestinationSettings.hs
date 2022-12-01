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
-- Module      : Amazonka.MediaConvert.Types.ImscDestinationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ImscDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.ImscAccessibilitySubs
import Amazonka.MediaConvert.Types.ImscStylePassthrough
import qualified Amazonka.Prelude as Prelude

-- | Settings related to IMSC captions. IMSC is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to IMSC.
--
-- /See:/ 'newImscDestinationSettings' smart constructor.
data ImscDestinationSettings = ImscDestinationSettings'
  { -- | Keep this setting enabled to have MediaConvert use the font style and
    -- position information from the captions source in the output. This option
    -- is available only when your input captions are IMSC, SMPTE-TT, or TTML.
    -- Disable this setting for simplified output captions.
    stylePassthrough :: Prelude.Maybe ImscStylePassthrough,
    -- | Set Accessibility subtitles to Enabled if the ISMC or WebVTT captions
    -- track is intended to provide accessibility for people who are deaf or
    -- hard of hearing. When you enable this feature, MediaConvert adds the
    -- following attributes under EXT-X-MEDIA in the HLS or CMAF manifest for
    -- this track:
    -- CHARACTERISTICS=\"public.accessibility.describes-spoken-dialog,public.accessibility.describes-music-and-sound\"
    -- and AUTOSELECT=\"YES\". Keep the default value, Disabled, if the
    -- captions track is not intended to provide such accessibility.
    -- MediaConvert will not add the above attributes.
    accessibility :: Prelude.Maybe ImscAccessibilitySubs
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImscDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stylePassthrough', 'imscDestinationSettings_stylePassthrough' - Keep this setting enabled to have MediaConvert use the font style and
-- position information from the captions source in the output. This option
-- is available only when your input captions are IMSC, SMPTE-TT, or TTML.
-- Disable this setting for simplified output captions.
--
-- 'accessibility', 'imscDestinationSettings_accessibility' - Set Accessibility subtitles to Enabled if the ISMC or WebVTT captions
-- track is intended to provide accessibility for people who are deaf or
-- hard of hearing. When you enable this feature, MediaConvert adds the
-- following attributes under EXT-X-MEDIA in the HLS or CMAF manifest for
-- this track:
-- CHARACTERISTICS=\"public.accessibility.describes-spoken-dialog,public.accessibility.describes-music-and-sound\"
-- and AUTOSELECT=\"YES\". Keep the default value, Disabled, if the
-- captions track is not intended to provide such accessibility.
-- MediaConvert will not add the above attributes.
newImscDestinationSettings ::
  ImscDestinationSettings
newImscDestinationSettings =
  ImscDestinationSettings'
    { stylePassthrough =
        Prelude.Nothing,
      accessibility = Prelude.Nothing
    }

-- | Keep this setting enabled to have MediaConvert use the font style and
-- position information from the captions source in the output. This option
-- is available only when your input captions are IMSC, SMPTE-TT, or TTML.
-- Disable this setting for simplified output captions.
imscDestinationSettings_stylePassthrough :: Lens.Lens' ImscDestinationSettings (Prelude.Maybe ImscStylePassthrough)
imscDestinationSettings_stylePassthrough = Lens.lens (\ImscDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@ImscDestinationSettings' {} a -> s {stylePassthrough = a} :: ImscDestinationSettings)

-- | Set Accessibility subtitles to Enabled if the ISMC or WebVTT captions
-- track is intended to provide accessibility for people who are deaf or
-- hard of hearing. When you enable this feature, MediaConvert adds the
-- following attributes under EXT-X-MEDIA in the HLS or CMAF manifest for
-- this track:
-- CHARACTERISTICS=\"public.accessibility.describes-spoken-dialog,public.accessibility.describes-music-and-sound\"
-- and AUTOSELECT=\"YES\". Keep the default value, Disabled, if the
-- captions track is not intended to provide such accessibility.
-- MediaConvert will not add the above attributes.
imscDestinationSettings_accessibility :: Lens.Lens' ImscDestinationSettings (Prelude.Maybe ImscAccessibilitySubs)
imscDestinationSettings_accessibility = Lens.lens (\ImscDestinationSettings' {accessibility} -> accessibility) (\s@ImscDestinationSettings' {} a -> s {accessibility = a} :: ImscDestinationSettings)

instance Core.FromJSON ImscDestinationSettings where
  parseJSON =
    Core.withObject
      "ImscDestinationSettings"
      ( \x ->
          ImscDestinationSettings'
            Prelude.<$> (x Core..:? "stylePassthrough")
            Prelude.<*> (x Core..:? "accessibility")
      )

instance Prelude.Hashable ImscDestinationSettings where
  hashWithSalt _salt ImscDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` stylePassthrough
      `Prelude.hashWithSalt` accessibility

instance Prelude.NFData ImscDestinationSettings where
  rnf ImscDestinationSettings' {..} =
    Prelude.rnf stylePassthrough
      `Prelude.seq` Prelude.rnf accessibility

instance Core.ToJSON ImscDestinationSettings where
  toJSON ImscDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stylePassthrough" Core..=)
              Prelude.<$> stylePassthrough,
            ("accessibility" Core..=) Prelude.<$> accessibility
          ]
      )
