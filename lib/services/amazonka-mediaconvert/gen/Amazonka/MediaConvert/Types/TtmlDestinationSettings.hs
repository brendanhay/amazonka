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
-- Module      : Amazonka.MediaConvert.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.TtmlDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.TtmlStylePassthrough
import qualified Amazonka.Prelude as Prelude

-- | Settings related to TTML captions. TTML is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/ttml-and-webvtt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to TTML.
--
-- /See:/ 'newTtmlDestinationSettings' smart constructor.
data TtmlDestinationSettings = TtmlDestinationSettings'
  { -- | Pass through style and position information from a TTML-like input
    -- source (TTML, IMSC, SMPTE-TT) to the TTML output.
    stylePassthrough :: Prelude.Maybe TtmlStylePassthrough
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TtmlDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stylePassthrough', 'ttmlDestinationSettings_stylePassthrough' - Pass through style and position information from a TTML-like input
-- source (TTML, IMSC, SMPTE-TT) to the TTML output.
newTtmlDestinationSettings ::
  TtmlDestinationSettings
newTtmlDestinationSettings =
  TtmlDestinationSettings'
    { stylePassthrough =
        Prelude.Nothing
    }

-- | Pass through style and position information from a TTML-like input
-- source (TTML, IMSC, SMPTE-TT) to the TTML output.
ttmlDestinationSettings_stylePassthrough :: Lens.Lens' TtmlDestinationSettings (Prelude.Maybe TtmlStylePassthrough)
ttmlDestinationSettings_stylePassthrough = Lens.lens (\TtmlDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@TtmlDestinationSettings' {} a -> s {stylePassthrough = a} :: TtmlDestinationSettings)

instance Data.FromJSON TtmlDestinationSettings where
  parseJSON =
    Data.withObject
      "TtmlDestinationSettings"
      ( \x ->
          TtmlDestinationSettings'
            Prelude.<$> (x Data..:? "stylePassthrough")
      )

instance Prelude.Hashable TtmlDestinationSettings where
  hashWithSalt _salt TtmlDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` stylePassthrough

instance Prelude.NFData TtmlDestinationSettings where
  rnf TtmlDestinationSettings' {..} =
    Prelude.rnf stylePassthrough

instance Data.ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("stylePassthrough" Data..=)
              Prelude.<$> stylePassthrough
          ]
      )
