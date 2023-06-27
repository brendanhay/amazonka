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
-- Module      : Amazonka.LexV2Models.Types.TestSetSlotDiscrepancyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetSlotDiscrepancyItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about discrepancy in a slot information between the
-- test set and the bot.
--
-- /See:/ 'newTestSetSlotDiscrepancyItem' smart constructor.
data TestSetSlotDiscrepancyItem = TestSetSlotDiscrepancyItem'
  { -- | The name of the intent associated with the slot in the discrepancy
    -- report.
    intentName :: Prelude.Text,
    -- | The name of the slot in the discrepancy report.
    slotName :: Prelude.Text,
    -- | The error message for a discrepancy for an intent between the test set
    -- and the bot.
    errorMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetSlotDiscrepancyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'testSetSlotDiscrepancyItem_intentName' - The name of the intent associated with the slot in the discrepancy
-- report.
--
-- 'slotName', 'testSetSlotDiscrepancyItem_slotName' - The name of the slot in the discrepancy report.
--
-- 'errorMessage', 'testSetSlotDiscrepancyItem_errorMessage' - The error message for a discrepancy for an intent between the test set
-- and the bot.
newTestSetSlotDiscrepancyItem ::
  -- | 'intentName'
  Prelude.Text ->
  -- | 'slotName'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  TestSetSlotDiscrepancyItem
newTestSetSlotDiscrepancyItem
  pIntentName_
  pSlotName_
  pErrorMessage_ =
    TestSetSlotDiscrepancyItem'
      { intentName =
          pIntentName_,
        slotName = pSlotName_,
        errorMessage = pErrorMessage_
      }

-- | The name of the intent associated with the slot in the discrepancy
-- report.
testSetSlotDiscrepancyItem_intentName :: Lens.Lens' TestSetSlotDiscrepancyItem Prelude.Text
testSetSlotDiscrepancyItem_intentName = Lens.lens (\TestSetSlotDiscrepancyItem' {intentName} -> intentName) (\s@TestSetSlotDiscrepancyItem' {} a -> s {intentName = a} :: TestSetSlotDiscrepancyItem)

-- | The name of the slot in the discrepancy report.
testSetSlotDiscrepancyItem_slotName :: Lens.Lens' TestSetSlotDiscrepancyItem Prelude.Text
testSetSlotDiscrepancyItem_slotName = Lens.lens (\TestSetSlotDiscrepancyItem' {slotName} -> slotName) (\s@TestSetSlotDiscrepancyItem' {} a -> s {slotName = a} :: TestSetSlotDiscrepancyItem)

-- | The error message for a discrepancy for an intent between the test set
-- and the bot.
testSetSlotDiscrepancyItem_errorMessage :: Lens.Lens' TestSetSlotDiscrepancyItem Prelude.Text
testSetSlotDiscrepancyItem_errorMessage = Lens.lens (\TestSetSlotDiscrepancyItem' {errorMessage} -> errorMessage) (\s@TestSetSlotDiscrepancyItem' {} a -> s {errorMessage = a} :: TestSetSlotDiscrepancyItem)

instance Data.FromJSON TestSetSlotDiscrepancyItem where
  parseJSON =
    Data.withObject
      "TestSetSlotDiscrepancyItem"
      ( \x ->
          TestSetSlotDiscrepancyItem'
            Prelude.<$> (x Data..: "intentName")
            Prelude.<*> (x Data..: "slotName")
            Prelude.<*> (x Data..: "errorMessage")
      )

instance Prelude.Hashable TestSetSlotDiscrepancyItem where
  hashWithSalt _salt TestSetSlotDiscrepancyItem' {..} =
    _salt
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` slotName
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData TestSetSlotDiscrepancyItem where
  rnf TestSetSlotDiscrepancyItem' {..} =
    Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf errorMessage
