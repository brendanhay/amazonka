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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ImscDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    stylePassthrough :: Prelude.Maybe ImscStylePassthrough
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
newImscDestinationSettings ::
  ImscDestinationSettings
newImscDestinationSettings =
  ImscDestinationSettings'
    { stylePassthrough =
        Prelude.Nothing
    }

-- | Keep this setting enabled to have MediaConvert use the font style and
-- position information from the captions source in the output. This option
-- is available only when your input captions are IMSC, SMPTE-TT, or TTML.
-- Disable this setting for simplified output captions.
imscDestinationSettings_stylePassthrough :: Lens.Lens' ImscDestinationSettings (Prelude.Maybe ImscStylePassthrough)
imscDestinationSettings_stylePassthrough = Lens.lens (\ImscDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@ImscDestinationSettings' {} a -> s {stylePassthrough = a} :: ImscDestinationSettings)

instance Core.FromJSON ImscDestinationSettings where
  parseJSON =
    Core.withObject
      "ImscDestinationSettings"
      ( \x ->
          ImscDestinationSettings'
            Prelude.<$> (x Core..:? "stylePassthrough")
      )

instance Prelude.Hashable ImscDestinationSettings where
  hashWithSalt _salt ImscDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` stylePassthrough

instance Prelude.NFData ImscDestinationSettings where
  rnf ImscDestinationSettings' {..} =
    Prelude.rnf stylePassthrough

instance Core.ToJSON ImscDestinationSettings where
  toJSON ImscDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stylePassthrough" Core..=)
              Prelude.<$> stylePassthrough
          ]
      )
