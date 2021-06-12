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
-- Module      : Network.AWS.MediaConvert.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TtmlDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TtmlStylePassthrough

-- | Settings specific to TTML caption outputs, including Pass style
-- information (TtmlStylePassthrough).
--
-- /See:/ 'newTtmlDestinationSettings' smart constructor.
data TtmlDestinationSettings = TtmlDestinationSettings'
  { -- | Pass through style and position information from a TTML-like input
    -- source (TTML, SMPTE-TT) to the TTML output.
    stylePassthrough :: Core.Maybe TtmlStylePassthrough
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TtmlDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stylePassthrough', 'ttmlDestinationSettings_stylePassthrough' - Pass through style and position information from a TTML-like input
-- source (TTML, SMPTE-TT) to the TTML output.
newTtmlDestinationSettings ::
  TtmlDestinationSettings
newTtmlDestinationSettings =
  TtmlDestinationSettings'
    { stylePassthrough =
        Core.Nothing
    }

-- | Pass through style and position information from a TTML-like input
-- source (TTML, SMPTE-TT) to the TTML output.
ttmlDestinationSettings_stylePassthrough :: Lens.Lens' TtmlDestinationSettings (Core.Maybe TtmlStylePassthrough)
ttmlDestinationSettings_stylePassthrough = Lens.lens (\TtmlDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@TtmlDestinationSettings' {} a -> s {stylePassthrough = a} :: TtmlDestinationSettings)

instance Core.FromJSON TtmlDestinationSettings where
  parseJSON =
    Core.withObject
      "TtmlDestinationSettings"
      ( \x ->
          TtmlDestinationSettings'
            Core.<$> (x Core..:? "stylePassthrough")
      )

instance Core.Hashable TtmlDestinationSettings

instance Core.NFData TtmlDestinationSettings

instance Core.ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("stylePassthrough" Core..=)
              Core.<$> stylePassthrough
          ]
      )
