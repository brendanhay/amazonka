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
-- Module      : Amazonka.MemoryDb.Types.RecurringCharge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.RecurringCharge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The recurring charge to run this reserved node.
--
-- /See:/ 'newRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The amount of the recurring charge to run this reserved node.
    recurringChargeAmount :: Prelude.Maybe Prelude.Double,
    -- | The frequency of the recurring price charged to run this reserved node.
    recurringChargeFrequency :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecurringCharge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recurringChargeAmount', 'recurringCharge_recurringChargeAmount' - The amount of the recurring charge to run this reserved node.
--
-- 'recurringChargeFrequency', 'recurringCharge_recurringChargeFrequency' - The frequency of the recurring price charged to run this reserved node.
newRecurringCharge ::
  RecurringCharge
newRecurringCharge =
  RecurringCharge'
    { recurringChargeAmount =
        Prelude.Nothing,
      recurringChargeFrequency = Prelude.Nothing
    }

-- | The amount of the recurring charge to run this reserved node.
recurringCharge_recurringChargeAmount :: Lens.Lens' RecurringCharge (Prelude.Maybe Prelude.Double)
recurringCharge_recurringChargeAmount = Lens.lens (\RecurringCharge' {recurringChargeAmount} -> recurringChargeAmount) (\s@RecurringCharge' {} a -> s {recurringChargeAmount = a} :: RecurringCharge)

-- | The frequency of the recurring price charged to run this reserved node.
recurringCharge_recurringChargeFrequency :: Lens.Lens' RecurringCharge (Prelude.Maybe Prelude.Text)
recurringCharge_recurringChargeFrequency = Lens.lens (\RecurringCharge' {recurringChargeFrequency} -> recurringChargeFrequency) (\s@RecurringCharge' {} a -> s {recurringChargeFrequency = a} :: RecurringCharge)

instance Data.FromJSON RecurringCharge where
  parseJSON =
    Data.withObject
      "RecurringCharge"
      ( \x ->
          RecurringCharge'
            Prelude.<$> (x Data..:? "RecurringChargeAmount")
            Prelude.<*> (x Data..:? "RecurringChargeFrequency")
      )

instance Prelude.Hashable RecurringCharge where
  hashWithSalt _salt RecurringCharge' {..} =
    _salt
      `Prelude.hashWithSalt` recurringChargeAmount
      `Prelude.hashWithSalt` recurringChargeFrequency

instance Prelude.NFData RecurringCharge where
  rnf RecurringCharge' {..} =
    Prelude.rnf recurringChargeAmount `Prelude.seq`
      Prelude.rnf recurringChargeFrequency
