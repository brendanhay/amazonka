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
-- Module      : Amazonka.LexV2Models.Types.TestExecutionResultFilterBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestExecutionResultFilterBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConversationLevelTestResultsFilterBy
import Amazonka.LexV2Models.Types.TestResultTypeFilter
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the method by which to filter the results of
-- the test execution.
--
-- /See:/ 'newTestExecutionResultFilterBy' smart constructor.
data TestExecutionResultFilterBy = TestExecutionResultFilterBy'
  { -- | Contains information about the method for filtering Conversation level
    -- test results.
    conversationLevelTestResultsFilterBy :: Prelude.Maybe ConversationLevelTestResultsFilterBy,
    -- | Specifies which results to filter. See
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/test-results-details-test-set.html Test result details\">Test results details>
    -- for details about different types of results.
    resultTypeFilter :: TestResultTypeFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestExecutionResultFilterBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversationLevelTestResultsFilterBy', 'testExecutionResultFilterBy_conversationLevelTestResultsFilterBy' - Contains information about the method for filtering Conversation level
-- test results.
--
-- 'resultTypeFilter', 'testExecutionResultFilterBy_resultTypeFilter' - Specifies which results to filter. See
-- <https://docs.aws.amazon.com/lexv2/latest/dg/test-results-details-test-set.html Test result details\">Test results details>
-- for details about different types of results.
newTestExecutionResultFilterBy ::
  -- | 'resultTypeFilter'
  TestResultTypeFilter ->
  TestExecutionResultFilterBy
newTestExecutionResultFilterBy pResultTypeFilter_ =
  TestExecutionResultFilterBy'
    { conversationLevelTestResultsFilterBy =
        Prelude.Nothing,
      resultTypeFilter = pResultTypeFilter_
    }

-- | Contains information about the method for filtering Conversation level
-- test results.
testExecutionResultFilterBy_conversationLevelTestResultsFilterBy :: Lens.Lens' TestExecutionResultFilterBy (Prelude.Maybe ConversationLevelTestResultsFilterBy)
testExecutionResultFilterBy_conversationLevelTestResultsFilterBy = Lens.lens (\TestExecutionResultFilterBy' {conversationLevelTestResultsFilterBy} -> conversationLevelTestResultsFilterBy) (\s@TestExecutionResultFilterBy' {} a -> s {conversationLevelTestResultsFilterBy = a} :: TestExecutionResultFilterBy)

-- | Specifies which results to filter. See
-- <https://docs.aws.amazon.com/lexv2/latest/dg/test-results-details-test-set.html Test result details\">Test results details>
-- for details about different types of results.
testExecutionResultFilterBy_resultTypeFilter :: Lens.Lens' TestExecutionResultFilterBy TestResultTypeFilter
testExecutionResultFilterBy_resultTypeFilter = Lens.lens (\TestExecutionResultFilterBy' {resultTypeFilter} -> resultTypeFilter) (\s@TestExecutionResultFilterBy' {} a -> s {resultTypeFilter = a} :: TestExecutionResultFilterBy)

instance Prelude.Hashable TestExecutionResultFilterBy where
  hashWithSalt _salt TestExecutionResultFilterBy' {..} =
    _salt
      `Prelude.hashWithSalt` conversationLevelTestResultsFilterBy
      `Prelude.hashWithSalt` resultTypeFilter

instance Prelude.NFData TestExecutionResultFilterBy where
  rnf TestExecutionResultFilterBy' {..} =
    Prelude.rnf conversationLevelTestResultsFilterBy
      `Prelude.seq` Prelude.rnf resultTypeFilter

instance Data.ToJSON TestExecutionResultFilterBy where
  toJSON TestExecutionResultFilterBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("conversationLevelTestResultsFilterBy" Data..=)
              Prelude.<$> conversationLevelTestResultsFilterBy,
            Prelude.Just
              ("resultTypeFilter" Data..= resultTypeFilter)
          ]
      )
