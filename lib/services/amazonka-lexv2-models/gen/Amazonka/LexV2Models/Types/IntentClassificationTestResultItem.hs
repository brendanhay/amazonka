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
-- Module      : Amazonka.LexV2Models.Types.IntentClassificationTestResultItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.IntentClassificationTestResultItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.IntentClassificationTestResultItemCounts
import qualified Amazonka.Prelude as Prelude

-- | Information for an intent that is classified by the test workbench.
--
-- /See:/ 'newIntentClassificationTestResultItem' smart constructor.
data IntentClassificationTestResultItem = IntentClassificationTestResultItem'
  { -- | The name of the intent.
    intentName :: Prelude.Text,
    -- | Indicates whether the conversation involves multiple turns or not.
    multiTurnConversation :: Prelude.Bool,
    -- | The result of the intent classification test.
    resultCounts :: IntentClassificationTestResultItemCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentClassificationTestResultItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intentName', 'intentClassificationTestResultItem_intentName' - The name of the intent.
--
-- 'multiTurnConversation', 'intentClassificationTestResultItem_multiTurnConversation' - Indicates whether the conversation involves multiple turns or not.
--
-- 'resultCounts', 'intentClassificationTestResultItem_resultCounts' - The result of the intent classification test.
newIntentClassificationTestResultItem ::
  -- | 'intentName'
  Prelude.Text ->
  -- | 'multiTurnConversation'
  Prelude.Bool ->
  -- | 'resultCounts'
  IntentClassificationTestResultItemCounts ->
  IntentClassificationTestResultItem
newIntentClassificationTestResultItem
  pIntentName_
  pMultiTurnConversation_
  pResultCounts_ =
    IntentClassificationTestResultItem'
      { intentName =
          pIntentName_,
        multiTurnConversation =
          pMultiTurnConversation_,
        resultCounts = pResultCounts_
      }

-- | The name of the intent.
intentClassificationTestResultItem_intentName :: Lens.Lens' IntentClassificationTestResultItem Prelude.Text
intentClassificationTestResultItem_intentName = Lens.lens (\IntentClassificationTestResultItem' {intentName} -> intentName) (\s@IntentClassificationTestResultItem' {} a -> s {intentName = a} :: IntentClassificationTestResultItem)

-- | Indicates whether the conversation involves multiple turns or not.
intentClassificationTestResultItem_multiTurnConversation :: Lens.Lens' IntentClassificationTestResultItem Prelude.Bool
intentClassificationTestResultItem_multiTurnConversation = Lens.lens (\IntentClassificationTestResultItem' {multiTurnConversation} -> multiTurnConversation) (\s@IntentClassificationTestResultItem' {} a -> s {multiTurnConversation = a} :: IntentClassificationTestResultItem)

-- | The result of the intent classification test.
intentClassificationTestResultItem_resultCounts :: Lens.Lens' IntentClassificationTestResultItem IntentClassificationTestResultItemCounts
intentClassificationTestResultItem_resultCounts = Lens.lens (\IntentClassificationTestResultItem' {resultCounts} -> resultCounts) (\s@IntentClassificationTestResultItem' {} a -> s {resultCounts = a} :: IntentClassificationTestResultItem)

instance
  Data.FromJSON
    IntentClassificationTestResultItem
  where
  parseJSON =
    Data.withObject
      "IntentClassificationTestResultItem"
      ( \x ->
          IntentClassificationTestResultItem'
            Prelude.<$> (x Data..: "intentName")
            Prelude.<*> (x Data..: "multiTurnConversation")
            Prelude.<*> (x Data..: "resultCounts")
      )

instance
  Prelude.Hashable
    IntentClassificationTestResultItem
  where
  hashWithSalt
    _salt
    IntentClassificationTestResultItem' {..} =
      _salt
        `Prelude.hashWithSalt` intentName
        `Prelude.hashWithSalt` multiTurnConversation
        `Prelude.hashWithSalt` resultCounts

instance
  Prelude.NFData
    IntentClassificationTestResultItem
  where
  rnf IntentClassificationTestResultItem' {..} =
    Prelude.rnf intentName
      `Prelude.seq` Prelude.rnf multiTurnConversation
      `Prelude.seq` Prelude.rnf resultCounts
