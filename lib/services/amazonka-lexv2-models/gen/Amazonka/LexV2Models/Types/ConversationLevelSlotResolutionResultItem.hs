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
-- Module      : Amazonka.LexV2Models.Types.ConversationLevelSlotResolutionResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConversationLevelSlotResolutionResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import qualified Amazonka.Prelude as Prelude

-- | The slots used for the slot resolution in the conversation.
--
-- /See:/ 'newConversationLevelSlotResolutionResultItem' smart constructor.
data ConversationLevelSlotResolutionResultItem = ConversationLevelSlotResolutionResultItem'
  { -- | The intents used in the slots list for the slot resolution details.
    intentName :: Prelude.Text,
    -- | The slot name in the slots list for the slot resolution details.
    slotName :: Prelude.Text,
    -- | The number of matching slots used in the slots listings for the slot
    -- resolution evaluation.
    matchResult :: TestResultMatchStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLevelSlotResolutionResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'conversationLevelSlotResolutionResultItem_intentName' - The intents used in the slots list for the slot resolution details.
--
-- 'slotName', 'conversationLevelSlotResolutionResultItem_slotName' - The slot name in the slots list for the slot resolution details.
--
-- 'matchResult', 'conversationLevelSlotResolutionResultItem_matchResult' - The number of matching slots used in the slots listings for the slot
-- resolution evaluation.
newConversationLevelSlotResolutionResultItem ::
  -- | 'intentName'
  Prelude.Text ->
  -- | 'slotName'
  Prelude.Text ->
  -- | 'matchResult'
  TestResultMatchStatus ->
  ConversationLevelSlotResolutionResultItem
newConversationLevelSlotResolutionResultItem
  pIntentName_
  pSlotName_
  pMatchResult_ =
    ConversationLevelSlotResolutionResultItem'
      { intentName =
          pIntentName_,
        slotName = pSlotName_,
        matchResult = pMatchResult_
      }

-- | The intents used in the slots list for the slot resolution details.
conversationLevelSlotResolutionResultItem_intentName :: Lens.Lens' ConversationLevelSlotResolutionResultItem Prelude.Text
conversationLevelSlotResolutionResultItem_intentName = Lens.lens (\ConversationLevelSlotResolutionResultItem' {intentName} -> intentName) (\s@ConversationLevelSlotResolutionResultItem' {} a -> s {intentName = a} :: ConversationLevelSlotResolutionResultItem)

-- | The slot name in the slots list for the slot resolution details.
conversationLevelSlotResolutionResultItem_slotName :: Lens.Lens' ConversationLevelSlotResolutionResultItem Prelude.Text
conversationLevelSlotResolutionResultItem_slotName = Lens.lens (\ConversationLevelSlotResolutionResultItem' {slotName} -> slotName) (\s@ConversationLevelSlotResolutionResultItem' {} a -> s {slotName = a} :: ConversationLevelSlotResolutionResultItem)

-- | The number of matching slots used in the slots listings for the slot
-- resolution evaluation.
conversationLevelSlotResolutionResultItem_matchResult :: Lens.Lens' ConversationLevelSlotResolutionResultItem TestResultMatchStatus
conversationLevelSlotResolutionResultItem_matchResult = Lens.lens (\ConversationLevelSlotResolutionResultItem' {matchResult} -> matchResult) (\s@ConversationLevelSlotResolutionResultItem' {} a -> s {matchResult = a} :: ConversationLevelSlotResolutionResultItem)

instance
  Data.FromJSON
    ConversationLevelSlotResolutionResultItem
  where
  parseJSON =
    Data.withObject
      "ConversationLevelSlotResolutionResultItem"
      ( \x ->
          ConversationLevelSlotResolutionResultItem'
            Prelude.<$> (x Data..: "intentName")
            Prelude.<*> (x Data..: "slotName")
            Prelude.<*> (x Data..: "matchResult")
      )

instance
  Prelude.Hashable
    ConversationLevelSlotResolutionResultItem
  where
  hashWithSalt
    _salt
    ConversationLevelSlotResolutionResultItem' {..} =
      _salt
        `Prelude.hashWithSalt` intentName
        `Prelude.hashWithSalt` slotName
        `Prelude.hashWithSalt` matchResult

instance
  Prelude.NFData
    ConversationLevelSlotResolutionResultItem
  where
  rnf ConversationLevelSlotResolutionResultItem' {..} =
    Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf matchResult
