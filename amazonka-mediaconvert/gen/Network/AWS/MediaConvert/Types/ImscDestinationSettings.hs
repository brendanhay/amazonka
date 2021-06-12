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
-- Module      : Network.AWS.MediaConvert.Types.ImscDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ImscDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.ImscStylePassthrough

-- | Settings specific to IMSC caption outputs.
--
-- /See:/ 'newImscDestinationSettings' smart constructor.
data ImscDestinationSettings = ImscDestinationSettings'
  { -- | Keep this setting enabled to have MediaConvert use the font style and
    -- position information from the captions source in the output. This option
    -- is available only when your input captions are IMSC, SMPTE-TT, or TTML.
    -- Disable this setting for simplified output captions.
    stylePassthrough :: Core.Maybe ImscStylePassthrough
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | Keep this setting enabled to have MediaConvert use the font style and
-- position information from the captions source in the output. This option
-- is available only when your input captions are IMSC, SMPTE-TT, or TTML.
-- Disable this setting for simplified output captions.
imscDestinationSettings_stylePassthrough :: Lens.Lens' ImscDestinationSettings (Core.Maybe ImscStylePassthrough)
imscDestinationSettings_stylePassthrough = Lens.lens (\ImscDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@ImscDestinationSettings' {} a -> s {stylePassthrough = a} :: ImscDestinationSettings)

instance Core.FromJSON ImscDestinationSettings where
  parseJSON =
    Core.withObject
      "ImscDestinationSettings"
      ( \x ->
          ImscDestinationSettings'
            Core.<$> (x Core..:? "stylePassthrough")
      )

instance Core.Hashable ImscDestinationSettings

instance Core.NFData ImscDestinationSettings

instance Core.ToJSON ImscDestinationSettings where
  toJSON ImscDestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("stylePassthrough" Core..=)
              Core.<$> stylePassthrough
          ]
      )
