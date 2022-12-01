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
-- Module      : Amazonka.MediaLive.Types.FecOutputSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FecOutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.FecOutputIncludeFec
import qualified Amazonka.Prelude as Prelude

-- | Fec Output Settings
--
-- /See:/ 'newFecOutputSettings' smart constructor.
data FecOutputSettings = FecOutputSettings'
  { -- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.
    -- Must be between 1 and 20, inclusive. If only Column FEC is used, then
    -- larger values increase robustness. If Row FEC is used, then this is the
    -- number of transport stream packets per row error correction packet, and
    -- the value must be between 4 and 20, inclusive, if includeFec is
    -- columnAndRow. If includeFec is column, this value must be 1 to 20,
    -- inclusive.
    rowLength :: Prelude.Maybe Prelude.Natural,
    -- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.
    -- The number of transport stream packets per column error correction
    -- packet. Must be between 4 and 20, inclusive.
    columnDepth :: Prelude.Maybe Prelude.Natural,
    -- | Enables column only or column and row based FEC
    includeFec :: Prelude.Maybe FecOutputIncludeFec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FecOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rowLength', 'fecOutputSettings_rowLength' - Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.
-- Must be between 1 and 20, inclusive. If only Column FEC is used, then
-- larger values increase robustness. If Row FEC is used, then this is the
-- number of transport stream packets per row error correction packet, and
-- the value must be between 4 and 20, inclusive, if includeFec is
-- columnAndRow. If includeFec is column, this value must be 1 to 20,
-- inclusive.
--
-- 'columnDepth', 'fecOutputSettings_columnDepth' - Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.
-- The number of transport stream packets per column error correction
-- packet. Must be between 4 and 20, inclusive.
--
-- 'includeFec', 'fecOutputSettings_includeFec' - Enables column only or column and row based FEC
newFecOutputSettings ::
  FecOutputSettings
newFecOutputSettings =
  FecOutputSettings'
    { rowLength = Prelude.Nothing,
      columnDepth = Prelude.Nothing,
      includeFec = Prelude.Nothing
    }

-- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.
-- Must be between 1 and 20, inclusive. If only Column FEC is used, then
-- larger values increase robustness. If Row FEC is used, then this is the
-- number of transport stream packets per row error correction packet, and
-- the value must be between 4 and 20, inclusive, if includeFec is
-- columnAndRow. If includeFec is column, this value must be 1 to 20,
-- inclusive.
fecOutputSettings_rowLength :: Lens.Lens' FecOutputSettings (Prelude.Maybe Prelude.Natural)
fecOutputSettings_rowLength = Lens.lens (\FecOutputSettings' {rowLength} -> rowLength) (\s@FecOutputSettings' {} a -> s {rowLength = a} :: FecOutputSettings)

-- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.
-- The number of transport stream packets per column error correction
-- packet. Must be between 4 and 20, inclusive.
fecOutputSettings_columnDepth :: Lens.Lens' FecOutputSettings (Prelude.Maybe Prelude.Natural)
fecOutputSettings_columnDepth = Lens.lens (\FecOutputSettings' {columnDepth} -> columnDepth) (\s@FecOutputSettings' {} a -> s {columnDepth = a} :: FecOutputSettings)

-- | Enables column only or column and row based FEC
fecOutputSettings_includeFec :: Lens.Lens' FecOutputSettings (Prelude.Maybe FecOutputIncludeFec)
fecOutputSettings_includeFec = Lens.lens (\FecOutputSettings' {includeFec} -> includeFec) (\s@FecOutputSettings' {} a -> s {includeFec = a} :: FecOutputSettings)

instance Core.FromJSON FecOutputSettings where
  parseJSON =
    Core.withObject
      "FecOutputSettings"
      ( \x ->
          FecOutputSettings'
            Prelude.<$> (x Core..:? "rowLength")
            Prelude.<*> (x Core..:? "columnDepth")
            Prelude.<*> (x Core..:? "includeFec")
      )

instance Prelude.Hashable FecOutputSettings where
  hashWithSalt _salt FecOutputSettings' {..} =
    _salt `Prelude.hashWithSalt` rowLength
      `Prelude.hashWithSalt` columnDepth
      `Prelude.hashWithSalt` includeFec

instance Prelude.NFData FecOutputSettings where
  rnf FecOutputSettings' {..} =
    Prelude.rnf rowLength
      `Prelude.seq` Prelude.rnf columnDepth
      `Prelude.seq` Prelude.rnf includeFec

instance Core.ToJSON FecOutputSettings where
  toJSON FecOutputSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("rowLength" Core..=) Prelude.<$> rowLength,
            ("columnDepth" Core..=) Prelude.<$> columnDepth,
            ("includeFec" Core..=) Prelude.<$> includeFec
          ]
      )
