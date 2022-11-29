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
-- Module      : Amazonka.MediaLive.Types.Hdr10Settings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Hdr10Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Hdr10 Settings
--
-- /See:/ 'newHdr10Settings' smart constructor.
data Hdr10Settings = Hdr10Settings'
  { -- | Maximum Content Light Level An integer metadata value defining the
    -- maximum light level, in nits, of any single pixel within an encoded HDR
    -- video stream or file.
    maxCll :: Prelude.Maybe Prelude.Natural,
    -- | Maximum Frame Average Light Level An integer metadata value defining the
    -- maximum average light level, in nits, for any single frame within an
    -- encoded HDR video stream or file.
    maxFall :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Hdr10Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCll', 'hdr10Settings_maxCll' - Maximum Content Light Level An integer metadata value defining the
-- maximum light level, in nits, of any single pixel within an encoded HDR
-- video stream or file.
--
-- 'maxFall', 'hdr10Settings_maxFall' - Maximum Frame Average Light Level An integer metadata value defining the
-- maximum average light level, in nits, for any single frame within an
-- encoded HDR video stream or file.
newHdr10Settings ::
  Hdr10Settings
newHdr10Settings =
  Hdr10Settings'
    { maxCll = Prelude.Nothing,
      maxFall = Prelude.Nothing
    }

-- | Maximum Content Light Level An integer metadata value defining the
-- maximum light level, in nits, of any single pixel within an encoded HDR
-- video stream or file.
hdr10Settings_maxCll :: Lens.Lens' Hdr10Settings (Prelude.Maybe Prelude.Natural)
hdr10Settings_maxCll = Lens.lens (\Hdr10Settings' {maxCll} -> maxCll) (\s@Hdr10Settings' {} a -> s {maxCll = a} :: Hdr10Settings)

-- | Maximum Frame Average Light Level An integer metadata value defining the
-- maximum average light level, in nits, for any single frame within an
-- encoded HDR video stream or file.
hdr10Settings_maxFall :: Lens.Lens' Hdr10Settings (Prelude.Maybe Prelude.Natural)
hdr10Settings_maxFall = Lens.lens (\Hdr10Settings' {maxFall} -> maxFall) (\s@Hdr10Settings' {} a -> s {maxFall = a} :: Hdr10Settings)

instance Core.FromJSON Hdr10Settings where
  parseJSON =
    Core.withObject
      "Hdr10Settings"
      ( \x ->
          Hdr10Settings'
            Prelude.<$> (x Core..:? "maxCll")
            Prelude.<*> (x Core..:? "maxFall")
      )

instance Prelude.Hashable Hdr10Settings where
  hashWithSalt _salt Hdr10Settings' {..} =
    _salt `Prelude.hashWithSalt` maxCll
      `Prelude.hashWithSalt` maxFall

instance Prelude.NFData Hdr10Settings where
  rnf Hdr10Settings' {..} =
    Prelude.rnf maxCll
      `Prelude.seq` Prelude.rnf maxFall

instance Core.ToJSON Hdr10Settings where
  toJSON Hdr10Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxCll" Core..=) Prelude.<$> maxCll,
            ("maxFall" Core..=) Prelude.<$> maxFall
          ]
      )
