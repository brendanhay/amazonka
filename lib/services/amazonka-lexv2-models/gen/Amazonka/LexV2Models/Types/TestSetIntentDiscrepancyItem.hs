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
-- Module      : Amazonka.LexV2Models.Types.TestSetIntentDiscrepancyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetIntentDiscrepancyItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about discrepancy in an intent information between
-- the test set and the bot.
--
-- /See:/ 'newTestSetIntentDiscrepancyItem' smart constructor.
data TestSetIntentDiscrepancyItem = TestSetIntentDiscrepancyItem'
  { -- | The name of the intent in the discrepancy report.
    intentName :: Prelude.Text,
    -- | The error message for a discrepancy for an intent between the test set
    -- and the bot.
    errorMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetIntentDiscrepancyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'testSetIntentDiscrepancyItem_intentName' - The name of the intent in the discrepancy report.
--
-- 'errorMessage', 'testSetIntentDiscrepancyItem_errorMessage' - The error message for a discrepancy for an intent between the test set
-- and the bot.
newTestSetIntentDiscrepancyItem ::
  -- | 'intentName'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  TestSetIntentDiscrepancyItem
newTestSetIntentDiscrepancyItem
  pIntentName_
  pErrorMessage_ =
    TestSetIntentDiscrepancyItem'
      { intentName =
          pIntentName_,
        errorMessage = pErrorMessage_
      }

-- | The name of the intent in the discrepancy report.
testSetIntentDiscrepancyItem_intentName :: Lens.Lens' TestSetIntentDiscrepancyItem Prelude.Text
testSetIntentDiscrepancyItem_intentName = Lens.lens (\TestSetIntentDiscrepancyItem' {intentName} -> intentName) (\s@TestSetIntentDiscrepancyItem' {} a -> s {intentName = a} :: TestSetIntentDiscrepancyItem)

-- | The error message for a discrepancy for an intent between the test set
-- and the bot.
testSetIntentDiscrepancyItem_errorMessage :: Lens.Lens' TestSetIntentDiscrepancyItem Prelude.Text
testSetIntentDiscrepancyItem_errorMessage = Lens.lens (\TestSetIntentDiscrepancyItem' {errorMessage} -> errorMessage) (\s@TestSetIntentDiscrepancyItem' {} a -> s {errorMessage = a} :: TestSetIntentDiscrepancyItem)

instance Data.FromJSON TestSetIntentDiscrepancyItem where
  parseJSON =
    Data.withObject
      "TestSetIntentDiscrepancyItem"
      ( \x ->
          TestSetIntentDiscrepancyItem'
            Prelude.<$> (x Data..: "intentName")
            Prelude.<*> (x Data..: "errorMessage")
      )

instance
  Prelude.Hashable
    TestSetIntentDiscrepancyItem
  where
  hashWithSalt _salt TestSetIntentDiscrepancyItem' {..} =
    _salt
      `Prelude.hashWithSalt` intentName
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData TestSetIntentDiscrepancyItem where
  rnf TestSetIntentDiscrepancyItem' {..} =
    Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf errorMessage
