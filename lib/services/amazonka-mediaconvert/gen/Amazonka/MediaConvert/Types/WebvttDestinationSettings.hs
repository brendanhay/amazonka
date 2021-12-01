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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.WebvttDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  { -- | Set Style passthrough (StylePassthrough) to ENABLED to use the available
    -- style, color, and position information from your input captions.
    -- MediaConvert uses default settings for any missing style and position
    -- information in your input captions. Set Style passthrough to DISABLED,
    -- or leave blank, to ignore the style and position information from your
    -- input captions and use simplified output captions.
    stylePassthrough :: Prelude.Maybe WebvttStylePassthrough
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
-- 'stylePassthrough', 'webvttDestinationSettings_stylePassthrough' - Set Style passthrough (StylePassthrough) to ENABLED to use the available
-- style, color, and position information from your input captions.
-- MediaConvert uses default settings for any missing style and position
-- information in your input captions. Set Style passthrough to DISABLED,
-- or leave blank, to ignore the style and position information from your
-- input captions and use simplified output captions.
newWebvttDestinationSettings ::
  WebvttDestinationSettings
newWebvttDestinationSettings =
  WebvttDestinationSettings'
    { stylePassthrough =
        Prelude.Nothing
    }

-- | Set Style passthrough (StylePassthrough) to ENABLED to use the available
-- style, color, and position information from your input captions.
-- MediaConvert uses default settings for any missing style and position
-- information in your input captions. Set Style passthrough to DISABLED,
-- or leave blank, to ignore the style and position information from your
-- input captions and use simplified output captions.
webvttDestinationSettings_stylePassthrough :: Lens.Lens' WebvttDestinationSettings (Prelude.Maybe WebvttStylePassthrough)
webvttDestinationSettings_stylePassthrough = Lens.lens (\WebvttDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@WebvttDestinationSettings' {} a -> s {stylePassthrough = a} :: WebvttDestinationSettings)

instance Core.FromJSON WebvttDestinationSettings where
  parseJSON =
    Core.withObject
      "WebvttDestinationSettings"
      ( \x ->
          WebvttDestinationSettings'
            Prelude.<$> (x Core..:? "stylePassthrough")
      )

instance Prelude.Hashable WebvttDestinationSettings where
  hashWithSalt salt' WebvttDestinationSettings' {..} =
    salt' `Prelude.hashWithSalt` stylePassthrough

instance Prelude.NFData WebvttDestinationSettings where
  rnf WebvttDestinationSettings' {..} =
    Prelude.rnf stylePassthrough

instance Core.ToJSON WebvttDestinationSettings where
  toJSON WebvttDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stylePassthrough" Core..=)
              Prelude.<$> stylePassthrough
          ]
      )
