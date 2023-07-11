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
-- Module      : Amazonka.EC2.Types.RecurringCharge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RecurringCharge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.RecurringChargeFrequency
import qualified Amazonka.Prelude as Prelude

-- | Describes a recurring charge.
--
-- /See:/ 'newRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The amount of the recurring charge.
    amount :: Prelude.Maybe Prelude.Double,
    -- | The frequency of the recurring charge.
    frequency :: Prelude.Maybe RecurringChargeFrequency
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
-- 'amount', 'recurringCharge_amount' - The amount of the recurring charge.
--
-- 'frequency', 'recurringCharge_frequency' - The frequency of the recurring charge.
newRecurringCharge ::
  RecurringCharge
newRecurringCharge =
  RecurringCharge'
    { amount = Prelude.Nothing,
      frequency = Prelude.Nothing
    }

-- | The amount of the recurring charge.
recurringCharge_amount :: Lens.Lens' RecurringCharge (Prelude.Maybe Prelude.Double)
recurringCharge_amount = Lens.lens (\RecurringCharge' {amount} -> amount) (\s@RecurringCharge' {} a -> s {amount = a} :: RecurringCharge)

-- | The frequency of the recurring charge.
recurringCharge_frequency :: Lens.Lens' RecurringCharge (Prelude.Maybe RecurringChargeFrequency)
recurringCharge_frequency = Lens.lens (\RecurringCharge' {frequency} -> frequency) (\s@RecurringCharge' {} a -> s {frequency = a} :: RecurringCharge)

instance Data.FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      Prelude.<$> (x Data..@? "amount")
      Prelude.<*> (x Data..@? "frequency")

instance Prelude.Hashable RecurringCharge where
  hashWithSalt _salt RecurringCharge' {..} =
    _salt
      `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` frequency

instance Prelude.NFData RecurringCharge where
  rnf RecurringCharge' {..} =
    Prelude.rnf amount
      `Prelude.seq` Prelude.rnf frequency
