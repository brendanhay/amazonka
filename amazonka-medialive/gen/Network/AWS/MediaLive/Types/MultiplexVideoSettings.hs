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
-- Module      : Network.AWS.MediaLive.Types.MultiplexVideoSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexVideoSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings

-- | The video configuration for each program in a multiplex.
--
-- /See:/ 'newMultiplexVideoSettings' smart constructor.
data MultiplexVideoSettings = MultiplexVideoSettings'
  { -- | The constant bitrate configuration for the video encode. When this field
    -- is defined, StatmuxSettings must be undefined.
    constantBitrate :: Core.Maybe Core.Natural,
    -- | Statmux rate control settings. When this field is defined,
    -- ConstantBitrate must be undefined.
    statmuxSettings :: Core.Maybe MultiplexStatmuxVideoSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiplexVideoSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constantBitrate', 'multiplexVideoSettings_constantBitrate' - The constant bitrate configuration for the video encode. When this field
-- is defined, StatmuxSettings must be undefined.
--
-- 'statmuxSettings', 'multiplexVideoSettings_statmuxSettings' - Statmux rate control settings. When this field is defined,
-- ConstantBitrate must be undefined.
newMultiplexVideoSettings ::
  MultiplexVideoSettings
newMultiplexVideoSettings =
  MultiplexVideoSettings'
    { constantBitrate =
        Core.Nothing,
      statmuxSettings = Core.Nothing
    }

-- | The constant bitrate configuration for the video encode. When this field
-- is defined, StatmuxSettings must be undefined.
multiplexVideoSettings_constantBitrate :: Lens.Lens' MultiplexVideoSettings (Core.Maybe Core.Natural)
multiplexVideoSettings_constantBitrate = Lens.lens (\MultiplexVideoSettings' {constantBitrate} -> constantBitrate) (\s@MultiplexVideoSettings' {} a -> s {constantBitrate = a} :: MultiplexVideoSettings)

-- | Statmux rate control settings. When this field is defined,
-- ConstantBitrate must be undefined.
multiplexVideoSettings_statmuxSettings :: Lens.Lens' MultiplexVideoSettings (Core.Maybe MultiplexStatmuxVideoSettings)
multiplexVideoSettings_statmuxSettings = Lens.lens (\MultiplexVideoSettings' {statmuxSettings} -> statmuxSettings) (\s@MultiplexVideoSettings' {} a -> s {statmuxSettings = a} :: MultiplexVideoSettings)

instance Core.FromJSON MultiplexVideoSettings where
  parseJSON =
    Core.withObject
      "MultiplexVideoSettings"
      ( \x ->
          MultiplexVideoSettings'
            Core.<$> (x Core..:? "constantBitrate")
            Core.<*> (x Core..:? "statmuxSettings")
      )

instance Core.Hashable MultiplexVideoSettings

instance Core.NFData MultiplexVideoSettings

instance Core.ToJSON MultiplexVideoSettings where
  toJSON MultiplexVideoSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("constantBitrate" Core..=)
              Core.<$> constantBitrate,
            ("statmuxSettings" Core..=)
              Core.<$> statmuxSettings
          ]
      )
