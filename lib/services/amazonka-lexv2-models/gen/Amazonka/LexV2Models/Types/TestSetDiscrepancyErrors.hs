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
-- Module      : Amazonka.LexV2Models.Types.TestSetDiscrepancyErrors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetDiscrepancyErrors where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestSetIntentDiscrepancyItem
import Amazonka.LexV2Models.Types.TestSetSlotDiscrepancyItem
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the errors in the test set discrepancy report
--
-- /See:/ 'newTestSetDiscrepancyErrors' smart constructor.
data TestSetDiscrepancyErrors = TestSetDiscrepancyErrors'
  { -- | Contains information about discrepancies found for intents between the
    -- test set and the bot.
    intentDiscrepancies :: [TestSetIntentDiscrepancyItem],
    -- | Contains information about discrepancies found for slots between the
    -- test set and the bot.
    slotDiscrepancies :: [TestSetSlotDiscrepancyItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetDiscrepancyErrors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentDiscrepancies', 'testSetDiscrepancyErrors_intentDiscrepancies' - Contains information about discrepancies found for intents between the
-- test set and the bot.
--
-- 'slotDiscrepancies', 'testSetDiscrepancyErrors_slotDiscrepancies' - Contains information about discrepancies found for slots between the
-- test set and the bot.
newTestSetDiscrepancyErrors ::
  TestSetDiscrepancyErrors
newTestSetDiscrepancyErrors =
  TestSetDiscrepancyErrors'
    { intentDiscrepancies =
        Prelude.mempty,
      slotDiscrepancies = Prelude.mempty
    }

-- | Contains information about discrepancies found for intents between the
-- test set and the bot.
testSetDiscrepancyErrors_intentDiscrepancies :: Lens.Lens' TestSetDiscrepancyErrors [TestSetIntentDiscrepancyItem]
testSetDiscrepancyErrors_intentDiscrepancies = Lens.lens (\TestSetDiscrepancyErrors' {intentDiscrepancies} -> intentDiscrepancies) (\s@TestSetDiscrepancyErrors' {} a -> s {intentDiscrepancies = a} :: TestSetDiscrepancyErrors) Prelude.. Lens.coerced

-- | Contains information about discrepancies found for slots between the
-- test set and the bot.
testSetDiscrepancyErrors_slotDiscrepancies :: Lens.Lens' TestSetDiscrepancyErrors [TestSetSlotDiscrepancyItem]
testSetDiscrepancyErrors_slotDiscrepancies = Lens.lens (\TestSetDiscrepancyErrors' {slotDiscrepancies} -> slotDiscrepancies) (\s@TestSetDiscrepancyErrors' {} a -> s {slotDiscrepancies = a} :: TestSetDiscrepancyErrors) Prelude.. Lens.coerced

instance Data.FromJSON TestSetDiscrepancyErrors where
  parseJSON =
    Data.withObject
      "TestSetDiscrepancyErrors"
      ( \x ->
          TestSetDiscrepancyErrors'
            Prelude.<$> ( x
                            Data..:? "intentDiscrepancies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "slotDiscrepancies"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TestSetDiscrepancyErrors where
  hashWithSalt _salt TestSetDiscrepancyErrors' {..} =
    _salt
      `Prelude.hashWithSalt` intentDiscrepancies
      `Prelude.hashWithSalt` slotDiscrepancies

instance Prelude.NFData TestSetDiscrepancyErrors where
  rnf TestSetDiscrepancyErrors' {..} =
    Prelude.rnf intentDiscrepancies
      `Prelude.seq` Prelude.rnf slotDiscrepancies
