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
-- Module      : Network.AWS.MediaLive.Types.FecOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FecOutputSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FecOutputIncludeFec

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
    rowLength :: Core.Maybe Core.Natural,
    -- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.
    -- The number of transport stream packets per column error correction
    -- packet. Must be between 4 and 20, inclusive.
    columnDepth :: Core.Maybe Core.Natural,
    -- | Enables column only or column and row based FEC
    includeFec :: Core.Maybe FecOutputIncludeFec
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { rowLength = Core.Nothing,
      columnDepth = Core.Nothing,
      includeFec = Core.Nothing
    }

-- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.
-- Must be between 1 and 20, inclusive. If only Column FEC is used, then
-- larger values increase robustness. If Row FEC is used, then this is the
-- number of transport stream packets per row error correction packet, and
-- the value must be between 4 and 20, inclusive, if includeFec is
-- columnAndRow. If includeFec is column, this value must be 1 to 20,
-- inclusive.
fecOutputSettings_rowLength :: Lens.Lens' FecOutputSettings (Core.Maybe Core.Natural)
fecOutputSettings_rowLength = Lens.lens (\FecOutputSettings' {rowLength} -> rowLength) (\s@FecOutputSettings' {} a -> s {rowLength = a} :: FecOutputSettings)

-- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.
-- The number of transport stream packets per column error correction
-- packet. Must be between 4 and 20, inclusive.
fecOutputSettings_columnDepth :: Lens.Lens' FecOutputSettings (Core.Maybe Core.Natural)
fecOutputSettings_columnDepth = Lens.lens (\FecOutputSettings' {columnDepth} -> columnDepth) (\s@FecOutputSettings' {} a -> s {columnDepth = a} :: FecOutputSettings)

-- | Enables column only or column and row based FEC
fecOutputSettings_includeFec :: Lens.Lens' FecOutputSettings (Core.Maybe FecOutputIncludeFec)
fecOutputSettings_includeFec = Lens.lens (\FecOutputSettings' {includeFec} -> includeFec) (\s@FecOutputSettings' {} a -> s {includeFec = a} :: FecOutputSettings)

instance Core.FromJSON FecOutputSettings where
  parseJSON =
    Core.withObject
      "FecOutputSettings"
      ( \x ->
          FecOutputSettings'
            Core.<$> (x Core..:? "rowLength")
            Core.<*> (x Core..:? "columnDepth")
            Core.<*> (x Core..:? "includeFec")
      )

instance Core.Hashable FecOutputSettings

instance Core.NFData FecOutputSettings

instance Core.ToJSON FecOutputSettings where
  toJSON FecOutputSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("rowLength" Core..=) Core.<$> rowLength,
            ("columnDepth" Core..=) Core.<$> columnDepth,
            ("includeFec" Core..=) Core.<$> includeFec
          ]
      )
