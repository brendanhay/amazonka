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
-- Module      : Amazonka.GroundStation.Types.TLEData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.TLEData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.TimeRange
import qualified Amazonka.Prelude as Prelude

-- | Two-line element set (TLE) data.
--
-- /See:/ 'newTLEData' smart constructor.
data TLEData = TLEData'
  { -- | First line of two-line element set (TLE) data.
    tleLine1 :: Prelude.Text,
    -- | Second line of two-line element set (TLE) data.
    tleLine2 :: Prelude.Text,
    -- | The valid time range for the TLE. Gaps or overlap are not permitted.
    validTimeRange :: TimeRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TLEData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tleLine1', 'tLEData_tleLine1' - First line of two-line element set (TLE) data.
--
-- 'tleLine2', 'tLEData_tleLine2' - Second line of two-line element set (TLE) data.
--
-- 'validTimeRange', 'tLEData_validTimeRange' - The valid time range for the TLE. Gaps or overlap are not permitted.
newTLEData ::
  -- | 'tleLine1'
  Prelude.Text ->
  -- | 'tleLine2'
  Prelude.Text ->
  -- | 'validTimeRange'
  TimeRange ->
  TLEData
newTLEData pTleLine1_ pTleLine2_ pValidTimeRange_ =
  TLEData'
    { tleLine1 = pTleLine1_,
      tleLine2 = pTleLine2_,
      validTimeRange = pValidTimeRange_
    }

-- | First line of two-line element set (TLE) data.
tLEData_tleLine1 :: Lens.Lens' TLEData Prelude.Text
tLEData_tleLine1 = Lens.lens (\TLEData' {tleLine1} -> tleLine1) (\s@TLEData' {} a -> s {tleLine1 = a} :: TLEData)

-- | Second line of two-line element set (TLE) data.
tLEData_tleLine2 :: Lens.Lens' TLEData Prelude.Text
tLEData_tleLine2 = Lens.lens (\TLEData' {tleLine2} -> tleLine2) (\s@TLEData' {} a -> s {tleLine2 = a} :: TLEData)

-- | The valid time range for the TLE. Gaps or overlap are not permitted.
tLEData_validTimeRange :: Lens.Lens' TLEData TimeRange
tLEData_validTimeRange = Lens.lens (\TLEData' {validTimeRange} -> validTimeRange) (\s@TLEData' {} a -> s {validTimeRange = a} :: TLEData)

instance Prelude.Hashable TLEData where
  hashWithSalt _salt TLEData' {..} =
    _salt
      `Prelude.hashWithSalt` tleLine1
      `Prelude.hashWithSalt` tleLine2
      `Prelude.hashWithSalt` validTimeRange

instance Prelude.NFData TLEData where
  rnf TLEData' {..} =
    Prelude.rnf tleLine1 `Prelude.seq`
      Prelude.rnf tleLine2 `Prelude.seq`
        Prelude.rnf validTimeRange

instance Data.ToJSON TLEData where
  toJSON TLEData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("tleLine1" Data..= tleLine1),
            Prelude.Just ("tleLine2" Data..= tleLine2),
            Prelude.Just
              ("validTimeRange" Data..= validTimeRange)
          ]
      )
