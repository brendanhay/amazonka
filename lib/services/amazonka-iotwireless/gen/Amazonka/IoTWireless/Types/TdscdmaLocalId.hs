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
-- Module      : Amazonka.IoTWireless.Types.TdscdmaLocalId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.TdscdmaLocalId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | TD-SCDMA local identification (local Id) information.
--
-- /See:/ 'newTdscdmaLocalId' smart constructor.
data TdscdmaLocalId = TdscdmaLocalId'
  { -- | TD-SCDMA UTRA (Universal Terrestrial Radio Access Network) absolute RF
    -- channel number (UARFCN).
    uarfcn :: Prelude.Natural,
    -- | Cell parameters for TD-SCDMA.
    cellParams :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TdscdmaLocalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uarfcn', 'tdscdmaLocalId_uarfcn' - TD-SCDMA UTRA (Universal Terrestrial Radio Access Network) absolute RF
-- channel number (UARFCN).
--
-- 'cellParams', 'tdscdmaLocalId_cellParams' - Cell parameters for TD-SCDMA.
newTdscdmaLocalId ::
  -- | 'uarfcn'
  Prelude.Natural ->
  -- | 'cellParams'
  Prelude.Natural ->
  TdscdmaLocalId
newTdscdmaLocalId pUarfcn_ pCellParams_ =
  TdscdmaLocalId'
    { uarfcn = pUarfcn_,
      cellParams = pCellParams_
    }

-- | TD-SCDMA UTRA (Universal Terrestrial Radio Access Network) absolute RF
-- channel number (UARFCN).
tdscdmaLocalId_uarfcn :: Lens.Lens' TdscdmaLocalId Prelude.Natural
tdscdmaLocalId_uarfcn = Lens.lens (\TdscdmaLocalId' {uarfcn} -> uarfcn) (\s@TdscdmaLocalId' {} a -> s {uarfcn = a} :: TdscdmaLocalId)

-- | Cell parameters for TD-SCDMA.
tdscdmaLocalId_cellParams :: Lens.Lens' TdscdmaLocalId Prelude.Natural
tdscdmaLocalId_cellParams = Lens.lens (\TdscdmaLocalId' {cellParams} -> cellParams) (\s@TdscdmaLocalId' {} a -> s {cellParams = a} :: TdscdmaLocalId)

instance Prelude.Hashable TdscdmaLocalId where
  hashWithSalt _salt TdscdmaLocalId' {..} =
    _salt
      `Prelude.hashWithSalt` uarfcn
      `Prelude.hashWithSalt` cellParams

instance Prelude.NFData TdscdmaLocalId where
  rnf TdscdmaLocalId' {..} =
    Prelude.rnf uarfcn
      `Prelude.seq` Prelude.rnf cellParams

instance Data.ToJSON TdscdmaLocalId where
  toJSON TdscdmaLocalId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Uarfcn" Data..= uarfcn),
            Prelude.Just ("CellParams" Data..= cellParams)
          ]
      )
