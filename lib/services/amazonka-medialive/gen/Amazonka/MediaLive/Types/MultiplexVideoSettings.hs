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
-- Module      : Amazonka.MediaLive.Types.MultiplexVideoSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexVideoSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.MultiplexStatmuxVideoSettings
import qualified Amazonka.Prelude as Prelude

-- | The video configuration for each program in a multiplex.
--
-- /See:/ 'newMultiplexVideoSettings' smart constructor.
data MultiplexVideoSettings = MultiplexVideoSettings'
  { -- | Statmux rate control settings. When this field is defined,
    -- ConstantBitrate must be undefined.
    statmuxSettings :: Prelude.Maybe MultiplexStatmuxVideoSettings,
    -- | The constant bitrate configuration for the video encode. When this field
    -- is defined, StatmuxSettings must be undefined.
    constantBitrate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexVideoSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statmuxSettings', 'multiplexVideoSettings_statmuxSettings' - Statmux rate control settings. When this field is defined,
-- ConstantBitrate must be undefined.
--
-- 'constantBitrate', 'multiplexVideoSettings_constantBitrate' - The constant bitrate configuration for the video encode. When this field
-- is defined, StatmuxSettings must be undefined.
newMultiplexVideoSettings ::
  MultiplexVideoSettings
newMultiplexVideoSettings =
  MultiplexVideoSettings'
    { statmuxSettings =
        Prelude.Nothing,
      constantBitrate = Prelude.Nothing
    }

-- | Statmux rate control settings. When this field is defined,
-- ConstantBitrate must be undefined.
multiplexVideoSettings_statmuxSettings :: Lens.Lens' MultiplexVideoSettings (Prelude.Maybe MultiplexStatmuxVideoSettings)
multiplexVideoSettings_statmuxSettings = Lens.lens (\MultiplexVideoSettings' {statmuxSettings} -> statmuxSettings) (\s@MultiplexVideoSettings' {} a -> s {statmuxSettings = a} :: MultiplexVideoSettings)

-- | The constant bitrate configuration for the video encode. When this field
-- is defined, StatmuxSettings must be undefined.
multiplexVideoSettings_constantBitrate :: Lens.Lens' MultiplexVideoSettings (Prelude.Maybe Prelude.Natural)
multiplexVideoSettings_constantBitrate = Lens.lens (\MultiplexVideoSettings' {constantBitrate} -> constantBitrate) (\s@MultiplexVideoSettings' {} a -> s {constantBitrate = a} :: MultiplexVideoSettings)

instance Core.FromJSON MultiplexVideoSettings where
  parseJSON =
    Core.withObject
      "MultiplexVideoSettings"
      ( \x ->
          MultiplexVideoSettings'
            Prelude.<$> (x Core..:? "statmuxSettings")
            Prelude.<*> (x Core..:? "constantBitrate")
      )

instance Prelude.Hashable MultiplexVideoSettings

instance Prelude.NFData MultiplexVideoSettings

instance Core.ToJSON MultiplexVideoSettings where
  toJSON MultiplexVideoSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("statmuxSettings" Core..=)
              Prelude.<$> statmuxSettings,
            ("constantBitrate" Core..=)
              Prelude.<$> constantBitrate
          ]
      )
