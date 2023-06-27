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
-- Module      : Amazonka.IoTWireless.Types.GsmLocalId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.GsmLocalId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | GSM local ID information, which corresponds to the local identification
-- parameters of a GSM cell.
--
-- /See:/ 'newGsmLocalId' smart constructor.
data GsmLocalId = GsmLocalId'
  { -- | GSM base station identity code (BSIC).
    bsic :: Prelude.Natural,
    -- | GSM broadcast control channel.
    bcch :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GsmLocalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bsic', 'gsmLocalId_bsic' - GSM base station identity code (BSIC).
--
-- 'bcch', 'gsmLocalId_bcch' - GSM broadcast control channel.
newGsmLocalId ::
  -- | 'bsic'
  Prelude.Natural ->
  -- | 'bcch'
  Prelude.Natural ->
  GsmLocalId
newGsmLocalId pBsic_ pBcch_ =
  GsmLocalId' {bsic = pBsic_, bcch = pBcch_}

-- | GSM base station identity code (BSIC).
gsmLocalId_bsic :: Lens.Lens' GsmLocalId Prelude.Natural
gsmLocalId_bsic = Lens.lens (\GsmLocalId' {bsic} -> bsic) (\s@GsmLocalId' {} a -> s {bsic = a} :: GsmLocalId)

-- | GSM broadcast control channel.
gsmLocalId_bcch :: Lens.Lens' GsmLocalId Prelude.Natural
gsmLocalId_bcch = Lens.lens (\GsmLocalId' {bcch} -> bcch) (\s@GsmLocalId' {} a -> s {bcch = a} :: GsmLocalId)

instance Prelude.Hashable GsmLocalId where
  hashWithSalt _salt GsmLocalId' {..} =
    _salt
      `Prelude.hashWithSalt` bsic
      `Prelude.hashWithSalt` bcch

instance Prelude.NFData GsmLocalId where
  rnf GsmLocalId' {..} =
    Prelude.rnf bsic `Prelude.seq` Prelude.rnf bcch

instance Data.ToJSON GsmLocalId where
  toJSON GsmLocalId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Bsic" Data..= bsic),
            Prelude.Just ("Bcch" Data..= bcch)
          ]
      )
