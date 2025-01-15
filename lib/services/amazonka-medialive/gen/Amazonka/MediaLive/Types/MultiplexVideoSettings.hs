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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexVideoSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.MultiplexStatmuxVideoSettings
import qualified Amazonka.Prelude as Prelude

-- | The video configuration for each program in a multiplex.
--
-- /See:/ 'newMultiplexVideoSettings' smart constructor.
data MultiplexVideoSettings = MultiplexVideoSettings'
  { -- | The constant bitrate configuration for the video encode. When this field
    -- is defined, StatmuxSettings must be undefined.
    constantBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Statmux rate control settings. When this field is defined,
    -- ConstantBitrate must be undefined.
    statmuxSettings :: Prelude.Maybe MultiplexStatmuxVideoSettings
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
        Prelude.Nothing,
      statmuxSettings = Prelude.Nothing
    }

-- | The constant bitrate configuration for the video encode. When this field
-- is defined, StatmuxSettings must be undefined.
multiplexVideoSettings_constantBitrate :: Lens.Lens' MultiplexVideoSettings (Prelude.Maybe Prelude.Natural)
multiplexVideoSettings_constantBitrate = Lens.lens (\MultiplexVideoSettings' {constantBitrate} -> constantBitrate) (\s@MultiplexVideoSettings' {} a -> s {constantBitrate = a} :: MultiplexVideoSettings)

-- | Statmux rate control settings. When this field is defined,
-- ConstantBitrate must be undefined.
multiplexVideoSettings_statmuxSettings :: Lens.Lens' MultiplexVideoSettings (Prelude.Maybe MultiplexStatmuxVideoSettings)
multiplexVideoSettings_statmuxSettings = Lens.lens (\MultiplexVideoSettings' {statmuxSettings} -> statmuxSettings) (\s@MultiplexVideoSettings' {} a -> s {statmuxSettings = a} :: MultiplexVideoSettings)

instance Data.FromJSON MultiplexVideoSettings where
  parseJSON =
    Data.withObject
      "MultiplexVideoSettings"
      ( \x ->
          MultiplexVideoSettings'
            Prelude.<$> (x Data..:? "constantBitrate")
            Prelude.<*> (x Data..:? "statmuxSettings")
      )

instance Prelude.Hashable MultiplexVideoSettings where
  hashWithSalt _salt MultiplexVideoSettings' {..} =
    _salt
      `Prelude.hashWithSalt` constantBitrate
      `Prelude.hashWithSalt` statmuxSettings

instance Prelude.NFData MultiplexVideoSettings where
  rnf MultiplexVideoSettings' {..} =
    Prelude.rnf constantBitrate `Prelude.seq`
      Prelude.rnf statmuxSettings

instance Data.ToJSON MultiplexVideoSettings where
  toJSON MultiplexVideoSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("constantBitrate" Data..=)
              Prelude.<$> constantBitrate,
            ("statmuxSettings" Data..=)
              Prelude.<$> statmuxSettings
          ]
      )
