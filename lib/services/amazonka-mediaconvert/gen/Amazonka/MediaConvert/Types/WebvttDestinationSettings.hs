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
-- Module      : Amazonka.MediaConvert.Types.WebvttDestinationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.WebvttDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.WebvttAccessibilitySubs
import Amazonka.MediaConvert.Types.WebvttStylePassthrough
import qualified Amazonka.Prelude as Prelude

-- | Settings related to WebVTT captions. WebVTT is a sidecar format that
-- holds captions in a file that is separate from the video container. Set
-- up sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to WebVTT.
--
-- /See:/ 'newWebvttDestinationSettings' smart constructor.
data WebvttDestinationSettings = WebvttDestinationSettings'
  { -- | To use the available style, color, and position information from your
    -- input captions: Set Style passthrough (stylePassthrough) to Enabled
    -- (ENABLED). MediaConvert uses default settings when style and position
    -- information is missing from your input captions. To recreate the input
    -- captions exactly: Set Style passthrough to Strict (STRICT). MediaConvert
    -- automatically applies timing adjustments, including adjustments for
    -- frame rate conversion, ad avails, and input clipping. Your input
    -- captions format must be WebVTT. To ignore the style and position
    -- information from your input captions and use simplified output captions:
    -- Set Style passthrough to Disabled (DISABLED), or leave blank.
    stylePassthrough :: Prelude.Maybe WebvttStylePassthrough,
    -- | Set Accessibility subtitles to Enabled if the ISMC or WebVTT captions
    -- track is intended to provide accessibility for people who are deaf or
    -- hard of hearing. When you enable this feature, MediaConvert adds the
    -- following attributes under EXT-X-MEDIA in the HLS or CMAF manifest for
    -- this track:
    -- CHARACTERISTICS=\"public.accessibility.describes-spoken-dialog,public.accessibility.describes-music-and-sound\"
    -- and AUTOSELECT=\"YES\". Keep the default value, Disabled, if the
    -- captions track is not intended to provide such accessibility.
    -- MediaConvert will not add the above attributes.
    accessibility :: Prelude.Maybe WebvttAccessibilitySubs
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebvttDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stylePassthrough', 'webvttDestinationSettings_stylePassthrough' - To use the available style, color, and position information from your
-- input captions: Set Style passthrough (stylePassthrough) to Enabled
-- (ENABLED). MediaConvert uses default settings when style and position
-- information is missing from your input captions. To recreate the input
-- captions exactly: Set Style passthrough to Strict (STRICT). MediaConvert
-- automatically applies timing adjustments, including adjustments for
-- frame rate conversion, ad avails, and input clipping. Your input
-- captions format must be WebVTT. To ignore the style and position
-- information from your input captions and use simplified output captions:
-- Set Style passthrough to Disabled (DISABLED), or leave blank.
--
-- 'accessibility', 'webvttDestinationSettings_accessibility' - Set Accessibility subtitles to Enabled if the ISMC or WebVTT captions
-- track is intended to provide accessibility for people who are deaf or
-- hard of hearing. When you enable this feature, MediaConvert adds the
-- following attributes under EXT-X-MEDIA in the HLS or CMAF manifest for
-- this track:
-- CHARACTERISTICS=\"public.accessibility.describes-spoken-dialog,public.accessibility.describes-music-and-sound\"
-- and AUTOSELECT=\"YES\". Keep the default value, Disabled, if the
-- captions track is not intended to provide such accessibility.
-- MediaConvert will not add the above attributes.
newWebvttDestinationSettings ::
  WebvttDestinationSettings
newWebvttDestinationSettings =
  WebvttDestinationSettings'
    { stylePassthrough =
        Prelude.Nothing,
      accessibility = Prelude.Nothing
    }

-- | To use the available style, color, and position information from your
-- input captions: Set Style passthrough (stylePassthrough) to Enabled
-- (ENABLED). MediaConvert uses default settings when style and position
-- information is missing from your input captions. To recreate the input
-- captions exactly: Set Style passthrough to Strict (STRICT). MediaConvert
-- automatically applies timing adjustments, including adjustments for
-- frame rate conversion, ad avails, and input clipping. Your input
-- captions format must be WebVTT. To ignore the style and position
-- information from your input captions and use simplified output captions:
-- Set Style passthrough to Disabled (DISABLED), or leave blank.
webvttDestinationSettings_stylePassthrough :: Lens.Lens' WebvttDestinationSettings (Prelude.Maybe WebvttStylePassthrough)
webvttDestinationSettings_stylePassthrough = Lens.lens (\WebvttDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@WebvttDestinationSettings' {} a -> s {stylePassthrough = a} :: WebvttDestinationSettings)

-- | Set Accessibility subtitles to Enabled if the ISMC or WebVTT captions
-- track is intended to provide accessibility for people who are deaf or
-- hard of hearing. When you enable this feature, MediaConvert adds the
-- following attributes under EXT-X-MEDIA in the HLS or CMAF manifest for
-- this track:
-- CHARACTERISTICS=\"public.accessibility.describes-spoken-dialog,public.accessibility.describes-music-and-sound\"
-- and AUTOSELECT=\"YES\". Keep the default value, Disabled, if the
-- captions track is not intended to provide such accessibility.
-- MediaConvert will not add the above attributes.
webvttDestinationSettings_accessibility :: Lens.Lens' WebvttDestinationSettings (Prelude.Maybe WebvttAccessibilitySubs)
webvttDestinationSettings_accessibility = Lens.lens (\WebvttDestinationSettings' {accessibility} -> accessibility) (\s@WebvttDestinationSettings' {} a -> s {accessibility = a} :: WebvttDestinationSettings)

instance Data.FromJSON WebvttDestinationSettings where
  parseJSON =
    Data.withObject
      "WebvttDestinationSettings"
      ( \x ->
          WebvttDestinationSettings'
            Prelude.<$> (x Data..:? "stylePassthrough")
            Prelude.<*> (x Data..:? "accessibility")
      )

instance Prelude.Hashable WebvttDestinationSettings where
  hashWithSalt _salt WebvttDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` stylePassthrough
      `Prelude.hashWithSalt` accessibility

instance Prelude.NFData WebvttDestinationSettings where
  rnf WebvttDestinationSettings' {..} =
    Prelude.rnf stylePassthrough
      `Prelude.seq` Prelude.rnf accessibility

instance Data.ToJSON WebvttDestinationSettings where
  toJSON WebvttDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("stylePassthrough" Data..=)
              Prelude.<$> stylePassthrough,
            ("accessibility" Data..=) Prelude.<$> accessibility
          ]
      )
