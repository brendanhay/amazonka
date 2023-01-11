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
-- Module      : Amazonka.IoTWireless.Types.CdmaLocalId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.CdmaLocalId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | CDMA local ID information, which corresponds to the local identification
-- parameters of a CDMA cell.
--
-- /See:/ 'newCdmaLocalId' smart constructor.
data CdmaLocalId = CdmaLocalId'
  { -- | Pseudo-noise offset, which is a characteristic of the signal from a cell
    -- on a radio tower.
    pnOffset :: Prelude.Natural,
    -- | CDMA channel information.
    cdmaChannel :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CdmaLocalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pnOffset', 'cdmaLocalId_pnOffset' - Pseudo-noise offset, which is a characteristic of the signal from a cell
-- on a radio tower.
--
-- 'cdmaChannel', 'cdmaLocalId_cdmaChannel' - CDMA channel information.
newCdmaLocalId ::
  -- | 'pnOffset'
  Prelude.Natural ->
  -- | 'cdmaChannel'
  Prelude.Natural ->
  CdmaLocalId
newCdmaLocalId pPnOffset_ pCdmaChannel_ =
  CdmaLocalId'
    { pnOffset = pPnOffset_,
      cdmaChannel = pCdmaChannel_
    }

-- | Pseudo-noise offset, which is a characteristic of the signal from a cell
-- on a radio tower.
cdmaLocalId_pnOffset :: Lens.Lens' CdmaLocalId Prelude.Natural
cdmaLocalId_pnOffset = Lens.lens (\CdmaLocalId' {pnOffset} -> pnOffset) (\s@CdmaLocalId' {} a -> s {pnOffset = a} :: CdmaLocalId)

-- | CDMA channel information.
cdmaLocalId_cdmaChannel :: Lens.Lens' CdmaLocalId Prelude.Natural
cdmaLocalId_cdmaChannel = Lens.lens (\CdmaLocalId' {cdmaChannel} -> cdmaChannel) (\s@CdmaLocalId' {} a -> s {cdmaChannel = a} :: CdmaLocalId)

instance Prelude.Hashable CdmaLocalId where
  hashWithSalt _salt CdmaLocalId' {..} =
    _salt `Prelude.hashWithSalt` pnOffset
      `Prelude.hashWithSalt` cdmaChannel

instance Prelude.NFData CdmaLocalId where
  rnf CdmaLocalId' {..} =
    Prelude.rnf pnOffset
      `Prelude.seq` Prelude.rnf cdmaChannel

instance Data.ToJSON CdmaLocalId where
  toJSON CdmaLocalId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PnOffset" Data..= pnOffset),
            Prelude.Just ("CdmaChannel" Data..= cdmaChannel)
          ]
      )
