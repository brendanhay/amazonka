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
-- Module      : Amazonka.MediaLive.Types.Eac3AtmosSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3AtmosSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Eac3AtmosCodingMode
import Amazonka.MediaLive.Types.Eac3AtmosDrcLine
import Amazonka.MediaLive.Types.Eac3AtmosDrcRf
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Atmos Settings
--
-- /See:/ 'newEac3AtmosSettings' smart constructor.
data Eac3AtmosSettings = Eac3AtmosSettings'
  { -- | Average bitrate in bits\/second. Valid bitrates depend on the coding
    -- mode. \/\/ * \@affectsRightSizing true
    bitrate :: Prelude.Maybe Prelude.Double,
    -- | Dolby Digital Plus with Dolby Atmos coding mode. Determines number of
    -- channels.
    codingMode :: Prelude.Maybe Eac3AtmosCodingMode,
    -- | Sets the dialnorm for the output. Default 23.
    dialnorm :: Prelude.Maybe Prelude.Natural,
    -- | Sets the Dolby dynamic range compression profile.
    drcLine :: Prelude.Maybe Eac3AtmosDrcLine,
    -- | Sets the profile for heavy Dolby dynamic range compression, ensures that
    -- the instantaneous signal peaks do not exceed specified levels.
    drcRf :: Prelude.Maybe Eac3AtmosDrcRf,
    -- | Height dimensional trim. Sets the maximum amount to attenuate the height
    -- channels when the downstream player isn??t configured to handle Dolby
    -- Digital Plus with Dolby Atmos and must remix the channels.
    heightTrim :: Prelude.Maybe Prelude.Double,
    -- | Surround dimensional trim. Sets the maximum amount to attenuate the
    -- surround channels when the downstream player isn\'t configured to handle
    -- Dolby Digital Plus with Dolby Atmos and must remix the channels.
    surroundTrim :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Eac3AtmosSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitrate', 'eac3AtmosSettings_bitrate' - Average bitrate in bits\/second. Valid bitrates depend on the coding
-- mode. \/\/ * \@affectsRightSizing true
--
-- 'codingMode', 'eac3AtmosSettings_codingMode' - Dolby Digital Plus with Dolby Atmos coding mode. Determines number of
-- channels.
--
-- 'dialnorm', 'eac3AtmosSettings_dialnorm' - Sets the dialnorm for the output. Default 23.
--
-- 'drcLine', 'eac3AtmosSettings_drcLine' - Sets the Dolby dynamic range compression profile.
--
-- 'drcRf', 'eac3AtmosSettings_drcRf' - Sets the profile for heavy Dolby dynamic range compression, ensures that
-- the instantaneous signal peaks do not exceed specified levels.
--
-- 'heightTrim', 'eac3AtmosSettings_heightTrim' - Height dimensional trim. Sets the maximum amount to attenuate the height
-- channels when the downstream player isn??t configured to handle Dolby
-- Digital Plus with Dolby Atmos and must remix the channels.
--
-- 'surroundTrim', 'eac3AtmosSettings_surroundTrim' - Surround dimensional trim. Sets the maximum amount to attenuate the
-- surround channels when the downstream player isn\'t configured to handle
-- Dolby Digital Plus with Dolby Atmos and must remix the channels.
newEac3AtmosSettings ::
  Eac3AtmosSettings
newEac3AtmosSettings =
  Eac3AtmosSettings'
    { bitrate = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      dialnorm = Prelude.Nothing,
      drcLine = Prelude.Nothing,
      drcRf = Prelude.Nothing,
      heightTrim = Prelude.Nothing,
      surroundTrim = Prelude.Nothing
    }

-- | Average bitrate in bits\/second. Valid bitrates depend on the coding
-- mode. \/\/ * \@affectsRightSizing true
eac3AtmosSettings_bitrate :: Lens.Lens' Eac3AtmosSettings (Prelude.Maybe Prelude.Double)
eac3AtmosSettings_bitrate = Lens.lens (\Eac3AtmosSettings' {bitrate} -> bitrate) (\s@Eac3AtmosSettings' {} a -> s {bitrate = a} :: Eac3AtmosSettings)

-- | Dolby Digital Plus with Dolby Atmos coding mode. Determines number of
-- channels.
eac3AtmosSettings_codingMode :: Lens.Lens' Eac3AtmosSettings (Prelude.Maybe Eac3AtmosCodingMode)
eac3AtmosSettings_codingMode = Lens.lens (\Eac3AtmosSettings' {codingMode} -> codingMode) (\s@Eac3AtmosSettings' {} a -> s {codingMode = a} :: Eac3AtmosSettings)

-- | Sets the dialnorm for the output. Default 23.
eac3AtmosSettings_dialnorm :: Lens.Lens' Eac3AtmosSettings (Prelude.Maybe Prelude.Natural)
eac3AtmosSettings_dialnorm = Lens.lens (\Eac3AtmosSettings' {dialnorm} -> dialnorm) (\s@Eac3AtmosSettings' {} a -> s {dialnorm = a} :: Eac3AtmosSettings)

-- | Sets the Dolby dynamic range compression profile.
eac3AtmosSettings_drcLine :: Lens.Lens' Eac3AtmosSettings (Prelude.Maybe Eac3AtmosDrcLine)
eac3AtmosSettings_drcLine = Lens.lens (\Eac3AtmosSettings' {drcLine} -> drcLine) (\s@Eac3AtmosSettings' {} a -> s {drcLine = a} :: Eac3AtmosSettings)

-- | Sets the profile for heavy Dolby dynamic range compression, ensures that
-- the instantaneous signal peaks do not exceed specified levels.
eac3AtmosSettings_drcRf :: Lens.Lens' Eac3AtmosSettings (Prelude.Maybe Eac3AtmosDrcRf)
eac3AtmosSettings_drcRf = Lens.lens (\Eac3AtmosSettings' {drcRf} -> drcRf) (\s@Eac3AtmosSettings' {} a -> s {drcRf = a} :: Eac3AtmosSettings)

-- | Height dimensional trim. Sets the maximum amount to attenuate the height
-- channels when the downstream player isn??t configured to handle Dolby
-- Digital Plus with Dolby Atmos and must remix the channels.
eac3AtmosSettings_heightTrim :: Lens.Lens' Eac3AtmosSettings (Prelude.Maybe Prelude.Double)
eac3AtmosSettings_heightTrim = Lens.lens (\Eac3AtmosSettings' {heightTrim} -> heightTrim) (\s@Eac3AtmosSettings' {} a -> s {heightTrim = a} :: Eac3AtmosSettings)

-- | Surround dimensional trim. Sets the maximum amount to attenuate the
-- surround channels when the downstream player isn\'t configured to handle
-- Dolby Digital Plus with Dolby Atmos and must remix the channels.
eac3AtmosSettings_surroundTrim :: Lens.Lens' Eac3AtmosSettings (Prelude.Maybe Prelude.Double)
eac3AtmosSettings_surroundTrim = Lens.lens (\Eac3AtmosSettings' {surroundTrim} -> surroundTrim) (\s@Eac3AtmosSettings' {} a -> s {surroundTrim = a} :: Eac3AtmosSettings)

instance Data.FromJSON Eac3AtmosSettings where
  parseJSON =
    Data.withObject
      "Eac3AtmosSettings"
      ( \x ->
          Eac3AtmosSettings'
            Prelude.<$> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "codingMode")
            Prelude.<*> (x Data..:? "dialnorm")
            Prelude.<*> (x Data..:? "drcLine")
            Prelude.<*> (x Data..:? "drcRf")
            Prelude.<*> (x Data..:? "heightTrim")
            Prelude.<*> (x Data..:? "surroundTrim")
      )

instance Prelude.Hashable Eac3AtmosSettings where
  hashWithSalt _salt Eac3AtmosSettings' {..} =
    _salt `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` codingMode
      `Prelude.hashWithSalt` dialnorm
      `Prelude.hashWithSalt` drcLine
      `Prelude.hashWithSalt` drcRf
      `Prelude.hashWithSalt` heightTrim
      `Prelude.hashWithSalt` surroundTrim

instance Prelude.NFData Eac3AtmosSettings where
  rnf Eac3AtmosSettings' {..} =
    Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf codingMode
      `Prelude.seq` Prelude.rnf dialnorm
      `Prelude.seq` Prelude.rnf drcLine
      `Prelude.seq` Prelude.rnf drcRf
      `Prelude.seq` Prelude.rnf heightTrim
      `Prelude.seq` Prelude.rnf surroundTrim

instance Data.ToJSON Eac3AtmosSettings where
  toJSON Eac3AtmosSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrate" Data..=) Prelude.<$> bitrate,
            ("codingMode" Data..=) Prelude.<$> codingMode,
            ("dialnorm" Data..=) Prelude.<$> dialnorm,
            ("drcLine" Data..=) Prelude.<$> drcLine,
            ("drcRf" Data..=) Prelude.<$> drcRf,
            ("heightTrim" Data..=) Prelude.<$> heightTrim,
            ("surroundTrim" Data..=) Prelude.<$> surroundTrim
          ]
      )
