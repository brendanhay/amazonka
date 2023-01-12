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
-- Module      : Amazonka.IoTWireless.Types.LteLocalId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LteLocalId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | LTE local identification (local ID) information.
--
-- /See:/ 'newLteLocalId' smart constructor.
data LteLocalId = LteLocalId'
  { -- | Physical cell ID.
    pci :: Prelude.Natural,
    -- | Evolved universal terrestrial radio access (E-UTRA) absolute radio
    -- frequency channel number (FCN).
    earfcn :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LteLocalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pci', 'lteLocalId_pci' - Physical cell ID.
--
-- 'earfcn', 'lteLocalId_earfcn' - Evolved universal terrestrial radio access (E-UTRA) absolute radio
-- frequency channel number (FCN).
newLteLocalId ::
  -- | 'pci'
  Prelude.Natural ->
  -- | 'earfcn'
  Prelude.Natural ->
  LteLocalId
newLteLocalId pPci_ pEarfcn_ =
  LteLocalId' {pci = pPci_, earfcn = pEarfcn_}

-- | Physical cell ID.
lteLocalId_pci :: Lens.Lens' LteLocalId Prelude.Natural
lteLocalId_pci = Lens.lens (\LteLocalId' {pci} -> pci) (\s@LteLocalId' {} a -> s {pci = a} :: LteLocalId)

-- | Evolved universal terrestrial radio access (E-UTRA) absolute radio
-- frequency channel number (FCN).
lteLocalId_earfcn :: Lens.Lens' LteLocalId Prelude.Natural
lteLocalId_earfcn = Lens.lens (\LteLocalId' {earfcn} -> earfcn) (\s@LteLocalId' {} a -> s {earfcn = a} :: LteLocalId)

instance Prelude.Hashable LteLocalId where
  hashWithSalt _salt LteLocalId' {..} =
    _salt `Prelude.hashWithSalt` pci
      `Prelude.hashWithSalt` earfcn

instance Prelude.NFData LteLocalId where
  rnf LteLocalId' {..} =
    Prelude.rnf pci `Prelude.seq` Prelude.rnf earfcn

instance Data.ToJSON LteLocalId where
  toJSON LteLocalId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Pci" Data..= pci),
            Prelude.Just ("Earfcn" Data..= earfcn)
          ]
      )
