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
-- Module      : Amazonka.MediaLive.Types.Mp2Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Mp2Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Mp2CodingMode
import qualified Amazonka.Prelude as Prelude

-- | Mp2 Settings
--
-- /See:/ 'newMp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { -- | Average bitrate in bits\/second.
    bitrate :: Prelude.Maybe Prelude.Double,
    -- | The MPEG2 Audio coding mode. Valid values are codingMode10 (for mono) or
    -- codingMode20 (for stereo).
    codingMode :: Prelude.Maybe Mp2CodingMode,
    -- | Sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Mp2Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitrate', 'mp2Settings_bitrate' - Average bitrate in bits\/second.
--
-- 'codingMode', 'mp2Settings_codingMode' - The MPEG2 Audio coding mode. Valid values are codingMode10 (for mono) or
-- codingMode20 (for stereo).
--
-- 'sampleRate', 'mp2Settings_sampleRate' - Sample rate in Hz.
newMp2Settings ::
  Mp2Settings
newMp2Settings =
  Mp2Settings'
    { bitrate = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | Average bitrate in bits\/second.
mp2Settings_bitrate :: Lens.Lens' Mp2Settings (Prelude.Maybe Prelude.Double)
mp2Settings_bitrate = Lens.lens (\Mp2Settings' {bitrate} -> bitrate) (\s@Mp2Settings' {} a -> s {bitrate = a} :: Mp2Settings)

-- | The MPEG2 Audio coding mode. Valid values are codingMode10 (for mono) or
-- codingMode20 (for stereo).
mp2Settings_codingMode :: Lens.Lens' Mp2Settings (Prelude.Maybe Mp2CodingMode)
mp2Settings_codingMode = Lens.lens (\Mp2Settings' {codingMode} -> codingMode) (\s@Mp2Settings' {} a -> s {codingMode = a} :: Mp2Settings)

-- | Sample rate in Hz.
mp2Settings_sampleRate :: Lens.Lens' Mp2Settings (Prelude.Maybe Prelude.Double)
mp2Settings_sampleRate = Lens.lens (\Mp2Settings' {sampleRate} -> sampleRate) (\s@Mp2Settings' {} a -> s {sampleRate = a} :: Mp2Settings)

instance Data.FromJSON Mp2Settings where
  parseJSON =
    Data.withObject
      "Mp2Settings"
      ( \x ->
          Mp2Settings'
            Prelude.<$> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "codingMode")
            Prelude.<*> (x Data..:? "sampleRate")
      )

instance Prelude.Hashable Mp2Settings where
  hashWithSalt _salt Mp2Settings' {..} =
    _salt `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` codingMode
      `Prelude.hashWithSalt` sampleRate

instance Prelude.NFData Mp2Settings where
  rnf Mp2Settings' {..} =
    Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf codingMode
      `Prelude.seq` Prelude.rnf sampleRate

instance Data.ToJSON Mp2Settings where
  toJSON Mp2Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrate" Data..=) Prelude.<$> bitrate,
            ("codingMode" Data..=) Prelude.<$> codingMode,
            ("sampleRate" Data..=) Prelude.<$> sampleRate
          ]
      )
