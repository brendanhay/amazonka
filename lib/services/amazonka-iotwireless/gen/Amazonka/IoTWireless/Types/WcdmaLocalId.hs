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
-- Module      : Amazonka.IoTWireless.Types.WcdmaLocalId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WcdmaLocalId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | WCDMA local identification (local ID) information.
--
-- /See:/ 'newWcdmaLocalId' smart constructor.
data WcdmaLocalId = WcdmaLocalId'
  { -- | WCDMA UTRA Absolute RF Channel Number downlink.
    uarfcndl :: Prelude.Natural,
    -- | Primary Scrambling Code.
    psc :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WcdmaLocalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uarfcndl', 'wcdmaLocalId_uarfcndl' - WCDMA UTRA Absolute RF Channel Number downlink.
--
-- 'psc', 'wcdmaLocalId_psc' - Primary Scrambling Code.
newWcdmaLocalId ::
  -- | 'uarfcndl'
  Prelude.Natural ->
  -- | 'psc'
  Prelude.Natural ->
  WcdmaLocalId
newWcdmaLocalId pUarfcndl_ pPsc_ =
  WcdmaLocalId' {uarfcndl = pUarfcndl_, psc = pPsc_}

-- | WCDMA UTRA Absolute RF Channel Number downlink.
wcdmaLocalId_uarfcndl :: Lens.Lens' WcdmaLocalId Prelude.Natural
wcdmaLocalId_uarfcndl = Lens.lens (\WcdmaLocalId' {uarfcndl} -> uarfcndl) (\s@WcdmaLocalId' {} a -> s {uarfcndl = a} :: WcdmaLocalId)

-- | Primary Scrambling Code.
wcdmaLocalId_psc :: Lens.Lens' WcdmaLocalId Prelude.Natural
wcdmaLocalId_psc = Lens.lens (\WcdmaLocalId' {psc} -> psc) (\s@WcdmaLocalId' {} a -> s {psc = a} :: WcdmaLocalId)

instance Prelude.Hashable WcdmaLocalId where
  hashWithSalt _salt WcdmaLocalId' {..} =
    _salt
      `Prelude.hashWithSalt` uarfcndl
      `Prelude.hashWithSalt` psc

instance Prelude.NFData WcdmaLocalId where
  rnf WcdmaLocalId' {..} =
    Prelude.rnf uarfcndl `Prelude.seq` Prelude.rnf psc

instance Data.ToJSON WcdmaLocalId where
  toJSON WcdmaLocalId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Uarfcndl" Data..= uarfcndl),
            Prelude.Just ("Psc" Data..= psc)
          ]
      )
