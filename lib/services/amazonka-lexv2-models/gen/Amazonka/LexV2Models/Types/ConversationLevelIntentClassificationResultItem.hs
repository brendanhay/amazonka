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
-- Module      : Amazonka.LexV2Models.Types.ConversationLevelIntentClassificationResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConversationLevelIntentClassificationResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import qualified Amazonka.Prelude as Prelude

-- | The item listing the evaluation of intent level success or failure.
--
-- /See:/ 'newConversationLevelIntentClassificationResultItem' smart constructor.
data ConversationLevelIntentClassificationResultItem = ConversationLevelIntentClassificationResultItem'
  { -- | The intent name used in the evaluation of intent level success or
    -- failure.
    intentName :: Prelude.Text,
    -- | The number of times the specific intent is used in the evaluation of
    -- intent level success or failure.
    matchResult :: TestResultMatchStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLevelIntentClassificationResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'conversationLevelIntentClassificationResultItem_intentName' - The intent name used in the evaluation of intent level success or
-- failure.
--
-- 'matchResult', 'conversationLevelIntentClassificationResultItem_matchResult' - The number of times the specific intent is used in the evaluation of
-- intent level success or failure.
newConversationLevelIntentClassificationResultItem ::
  -- | 'intentName'
  Prelude.Text ->
  -- | 'matchResult'
  TestResultMatchStatus ->
  ConversationLevelIntentClassificationResultItem
newConversationLevelIntentClassificationResultItem
  pIntentName_
  pMatchResult_ =
    ConversationLevelIntentClassificationResultItem'
      { intentName =
          pIntentName_,
        matchResult =
          pMatchResult_
      }

-- | The intent name used in the evaluation of intent level success or
-- failure.
conversationLevelIntentClassificationResultItem_intentName :: Lens.Lens' ConversationLevelIntentClassificationResultItem Prelude.Text
conversationLevelIntentClassificationResultItem_intentName = Lens.lens (\ConversationLevelIntentClassificationResultItem' {intentName} -> intentName) (\s@ConversationLevelIntentClassificationResultItem' {} a -> s {intentName = a} :: ConversationLevelIntentClassificationResultItem)

-- | The number of times the specific intent is used in the evaluation of
-- intent level success or failure.
conversationLevelIntentClassificationResultItem_matchResult :: Lens.Lens' ConversationLevelIntentClassificationResultItem TestResultMatchStatus
conversationLevelIntentClassificationResultItem_matchResult = Lens.lens (\ConversationLevelIntentClassificationResultItem' {matchResult} -> matchResult) (\s@ConversationLevelIntentClassificationResultItem' {} a -> s {matchResult = a} :: ConversationLevelIntentClassificationResultItem)

instance
  Data.FromJSON
    ConversationLevelIntentClassificationResultItem
  where
  parseJSON =
    Data.withObject
      "ConversationLevelIntentClassificationResultItem"
      ( \x ->
          ConversationLevelIntentClassificationResultItem'
            Prelude.<$> (x Data..: "intentName")
            Prelude.<*> (x Data..: "matchResult")
      )

instance
  Prelude.Hashable
    ConversationLevelIntentClassificationResultItem
  where
  hashWithSalt
    _salt
    ConversationLevelIntentClassificationResultItem' {..} =
      _salt
        `Prelude.hashWithSalt` intentName
        `Prelude.hashWithSalt` matchResult

instance
  Prelude.NFData
    ConversationLevelIntentClassificationResultItem
  where
  rnf
    ConversationLevelIntentClassificationResultItem' {..} =
      Prelude.rnf intentName
        `Prelude.seq` Prelude.rnf matchResult
