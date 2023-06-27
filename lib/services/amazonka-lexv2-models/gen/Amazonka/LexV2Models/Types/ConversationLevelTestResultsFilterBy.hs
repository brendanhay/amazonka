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
-- Module      : Amazonka.LexV2Models.Types.ConversationLevelTestResultsFilterBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ConversationLevelTestResultsFilterBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import qualified Amazonka.Prelude as Prelude

-- | The selection to filter the test set results data at the conversation
-- level.
--
-- /See:/ 'newConversationLevelTestResultsFilterBy' smart constructor.
data ConversationLevelTestResultsFilterBy = ConversationLevelTestResultsFilterBy'
  { -- | The selection of matched or mismatched end-to-end status to filter test
    -- set results data at the conversation level.
    endToEndResult :: Prelude.Maybe TestResultMatchStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConversationLevelTestResultsFilterBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endToEndResult', 'conversationLevelTestResultsFilterBy_endToEndResult' - The selection of matched or mismatched end-to-end status to filter test
-- set results data at the conversation level.
newConversationLevelTestResultsFilterBy ::
  ConversationLevelTestResultsFilterBy
newConversationLevelTestResultsFilterBy =
  ConversationLevelTestResultsFilterBy'
    { endToEndResult =
        Prelude.Nothing
    }

-- | The selection of matched or mismatched end-to-end status to filter test
-- set results data at the conversation level.
conversationLevelTestResultsFilterBy_endToEndResult :: Lens.Lens' ConversationLevelTestResultsFilterBy (Prelude.Maybe TestResultMatchStatus)
conversationLevelTestResultsFilterBy_endToEndResult = Lens.lens (\ConversationLevelTestResultsFilterBy' {endToEndResult} -> endToEndResult) (\s@ConversationLevelTestResultsFilterBy' {} a -> s {endToEndResult = a} :: ConversationLevelTestResultsFilterBy)

instance
  Prelude.Hashable
    ConversationLevelTestResultsFilterBy
  where
  hashWithSalt
    _salt
    ConversationLevelTestResultsFilterBy' {..} =
      _salt `Prelude.hashWithSalt` endToEndResult

instance
  Prelude.NFData
    ConversationLevelTestResultsFilterBy
  where
  rnf ConversationLevelTestResultsFilterBy' {..} =
    Prelude.rnf endToEndResult

instance
  Data.ToJSON
    ConversationLevelTestResultsFilterBy
  where
  toJSON ConversationLevelTestResultsFilterBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endToEndResult" Data..=)
              Prelude.<$> endToEndResult
          ]
      )
