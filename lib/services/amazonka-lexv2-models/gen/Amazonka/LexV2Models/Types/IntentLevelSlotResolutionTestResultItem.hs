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
-- Module      : Amazonka.LexV2Models.Types.IntentLevelSlotResolutionTestResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentLevelSlotResolutionTestResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SlotResolutionTestResultItem
import qualified Amazonka.Prelude as Prelude

-- | Information about intent-level slot resolution in a test result.
--
-- /See:/ 'newIntentLevelSlotResolutionTestResultItem' smart constructor.
data IntentLevelSlotResolutionTestResultItem = IntentLevelSlotResolutionTestResultItem'
  { -- | The name of the intent that was recognized.
    intentName :: Prelude.Text,
    -- | Indicates whether the conversation involves multiple turns or not.
    multiTurnConversation :: Prelude.Bool,
    -- | The results for the slot resolution in the test execution result.
    slotResolutionResults :: [SlotResolutionTestResultItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentLevelSlotResolutionTestResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'intentLevelSlotResolutionTestResultItem_intentName' - The name of the intent that was recognized.
--
-- 'multiTurnConversation', 'intentLevelSlotResolutionTestResultItem_multiTurnConversation' - Indicates whether the conversation involves multiple turns or not.
--
-- 'slotResolutionResults', 'intentLevelSlotResolutionTestResultItem_slotResolutionResults' - The results for the slot resolution in the test execution result.
newIntentLevelSlotResolutionTestResultItem ::
  -- | 'intentName'
  Prelude.Text ->
  -- | 'multiTurnConversation'
  Prelude.Bool ->
  IntentLevelSlotResolutionTestResultItem
newIntentLevelSlotResolutionTestResultItem
  pIntentName_
  pMultiTurnConversation_ =
    IntentLevelSlotResolutionTestResultItem'
      { intentName =
          pIntentName_,
        multiTurnConversation =
          pMultiTurnConversation_,
        slotResolutionResults =
          Prelude.mempty
      }

-- | The name of the intent that was recognized.
intentLevelSlotResolutionTestResultItem_intentName :: Lens.Lens' IntentLevelSlotResolutionTestResultItem Prelude.Text
intentLevelSlotResolutionTestResultItem_intentName = Lens.lens (\IntentLevelSlotResolutionTestResultItem' {intentName} -> intentName) (\s@IntentLevelSlotResolutionTestResultItem' {} a -> s {intentName = a} :: IntentLevelSlotResolutionTestResultItem)

-- | Indicates whether the conversation involves multiple turns or not.
intentLevelSlotResolutionTestResultItem_multiTurnConversation :: Lens.Lens' IntentLevelSlotResolutionTestResultItem Prelude.Bool
intentLevelSlotResolutionTestResultItem_multiTurnConversation = Lens.lens (\IntentLevelSlotResolutionTestResultItem' {multiTurnConversation} -> multiTurnConversation) (\s@IntentLevelSlotResolutionTestResultItem' {} a -> s {multiTurnConversation = a} :: IntentLevelSlotResolutionTestResultItem)

-- | The results for the slot resolution in the test execution result.
intentLevelSlotResolutionTestResultItem_slotResolutionResults :: Lens.Lens' IntentLevelSlotResolutionTestResultItem [SlotResolutionTestResultItem]
intentLevelSlotResolutionTestResultItem_slotResolutionResults = Lens.lens (\IntentLevelSlotResolutionTestResultItem' {slotResolutionResults} -> slotResolutionResults) (\s@IntentLevelSlotResolutionTestResultItem' {} a -> s {slotResolutionResults = a} :: IntentLevelSlotResolutionTestResultItem) Prelude.. Lens.coerced

instance
  Data.FromJSON
    IntentLevelSlotResolutionTestResultItem
  where
  parseJSON =
    Data.withObject
      "IntentLevelSlotResolutionTestResultItem"
      ( \x ->
          IntentLevelSlotResolutionTestResultItem'
            Prelude.<$> (x Data..: "intentName")
            Prelude.<*> (x Data..: "multiTurnConversation")
            Prelude.<*> ( x
                            Data..:? "slotResolutionResults"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    IntentLevelSlotResolutionTestResultItem
  where
  hashWithSalt
    _salt
    IntentLevelSlotResolutionTestResultItem' {..} =
      _salt
        `Prelude.hashWithSalt` intentName
        `Prelude.hashWithSalt` multiTurnConversation
        `Prelude.hashWithSalt` slotResolutionResults

instance
  Prelude.NFData
    IntentLevelSlotResolutionTestResultItem
  where
  rnf IntentLevelSlotResolutionTestResultItem' {..} =
    Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf multiTurnConversation
      `Prelude.seq` Prelude.rnf slotResolutionResults
