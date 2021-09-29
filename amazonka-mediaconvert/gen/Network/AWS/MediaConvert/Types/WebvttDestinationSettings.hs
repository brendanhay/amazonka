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
-- Module      : Network.AWS.MediaConvert.Types.WebvttDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WebvttDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.WebvttStylePassthrough
import qualified Network.AWS.Prelude as Prelude

-- | WEBVTT Destination Settings
--
-- /See:/ 'newWebvttDestinationSettings' smart constructor.
data WebvttDestinationSettings = WebvttDestinationSettings'
  { -- | Choose Enabled (ENABLED) to have MediaConvert use the font style, color,
    -- and position information from the captions source in the input. Keep the
    -- default value, Disabled (DISABLED), for simplified output captions.
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
-- 'stylePassthrough', 'webvttDestinationSettings_stylePassthrough' - Choose Enabled (ENABLED) to have MediaConvert use the font style, color,
-- and position information from the captions source in the input. Keep the
-- default value, Disabled (DISABLED), for simplified output captions.
newWebvttDestinationSettings ::
  WebvttDestinationSettings
newWebvttDestinationSettings =
  WebvttDestinationSettings'
    { stylePassthrough =
        Prelude.Nothing
    }

-- | Choose Enabled (ENABLED) to have MediaConvert use the font style, color,
-- and position information from the captions source in the input. Keep the
-- default value, Disabled (DISABLED), for simplified output captions.
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

instance Prelude.Hashable WebvttDestinationSettings

instance Prelude.NFData WebvttDestinationSettings

instance Core.ToJSON WebvttDestinationSettings where
  toJSON WebvttDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stylePassthrough" Core..=)
              Prelude.<$> stylePassthrough
          ]
      )
