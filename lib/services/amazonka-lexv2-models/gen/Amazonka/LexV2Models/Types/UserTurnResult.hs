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
-- Module      : Amazonka.LexV2Models.Types.UserTurnResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UserTurnResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConversationLevelResultDetail
import Amazonka.LexV2Models.Types.ExecutionErrorDetails
import Amazonka.LexV2Models.Types.TestResultMatchStatus
import Amazonka.LexV2Models.Types.UserTurnInputSpecification
import Amazonka.LexV2Models.Types.UserTurnOutputSpecification
import qualified Amazonka.Prelude as Prelude

-- | Contains the results for the user turn by the test execution.
--
-- /See:/ 'newUserTurnResult' smart constructor.
data UserTurnResult = UserTurnResult'
  { -- | Contains information about the actual output for the user turn.
    actualOutput :: Prelude.Maybe UserTurnOutputSpecification,
    -- | Contains information about the results related to the conversation
    -- associated with the user turn.
    conversationLevelResult :: Prelude.Maybe ConversationLevelResultDetail,
    -- | Specifies whether the expected and actual outputs match or not, or if
    -- there is an error in execution.
    endToEndResult :: Prelude.Maybe TestResultMatchStatus,
    errorDetails :: Prelude.Maybe ExecutionErrorDetails,
    -- | Specifies whether the expected and actual intents match or not.
    intentMatchResult :: Prelude.Maybe TestResultMatchStatus,
    -- | Specifies whether the expected and actual slots match or not.
    slotMatchResult :: Prelude.Maybe TestResultMatchStatus,
    -- | Specifies whether the expected and actual speech transcriptions match or
    -- not, or if there is an error in execution.
    speechTranscriptionResult :: Prelude.Maybe TestResultMatchStatus,
    -- | Contains information about the user messages in the turn in the input.
    input :: UserTurnInputSpecification,
    -- | Contains information about the expected output for the user turn.
    expectedOutput :: UserTurnOutputSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserTurnResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actualOutput', 'userTurnResult_actualOutput' - Contains information about the actual output for the user turn.
--
-- 'conversationLevelResult', 'userTurnResult_conversationLevelResult' - Contains information about the results related to the conversation
-- associated with the user turn.
--
-- 'endToEndResult', 'userTurnResult_endToEndResult' - Specifies whether the expected and actual outputs match or not, or if
-- there is an error in execution.
--
-- 'errorDetails', 'userTurnResult_errorDetails' - Undocumented member.
--
-- 'intentMatchResult', 'userTurnResult_intentMatchResult' - Specifies whether the expected and actual intents match or not.
--
-- 'slotMatchResult', 'userTurnResult_slotMatchResult' - Specifies whether the expected and actual slots match or not.
--
-- 'speechTranscriptionResult', 'userTurnResult_speechTranscriptionResult' - Specifies whether the expected and actual speech transcriptions match or
-- not, or if there is an error in execution.
--
-- 'input', 'userTurnResult_input' - Contains information about the user messages in the turn in the input.
--
-- 'expectedOutput', 'userTurnResult_expectedOutput' - Contains information about the expected output for the user turn.
newUserTurnResult ::
  -- | 'input'
  UserTurnInputSpecification ->
  -- | 'expectedOutput'
  UserTurnOutputSpecification ->
  UserTurnResult
newUserTurnResult pInput_ pExpectedOutput_ =
  UserTurnResult'
    { actualOutput = Prelude.Nothing,
      conversationLevelResult = Prelude.Nothing,
      endToEndResult = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      intentMatchResult = Prelude.Nothing,
      slotMatchResult = Prelude.Nothing,
      speechTranscriptionResult = Prelude.Nothing,
      input = pInput_,
      expectedOutput = pExpectedOutput_
    }

-- | Contains information about the actual output for the user turn.
userTurnResult_actualOutput :: Lens.Lens' UserTurnResult (Prelude.Maybe UserTurnOutputSpecification)
userTurnResult_actualOutput = Lens.lens (\UserTurnResult' {actualOutput} -> actualOutput) (\s@UserTurnResult' {} a -> s {actualOutput = a} :: UserTurnResult)

-- | Contains information about the results related to the conversation
-- associated with the user turn.
userTurnResult_conversationLevelResult :: Lens.Lens' UserTurnResult (Prelude.Maybe ConversationLevelResultDetail)
userTurnResult_conversationLevelResult = Lens.lens (\UserTurnResult' {conversationLevelResult} -> conversationLevelResult) (\s@UserTurnResult' {} a -> s {conversationLevelResult = a} :: UserTurnResult)

-- | Specifies whether the expected and actual outputs match or not, or if
-- there is an error in execution.
userTurnResult_endToEndResult :: Lens.Lens' UserTurnResult (Prelude.Maybe TestResultMatchStatus)
userTurnResult_endToEndResult = Lens.lens (\UserTurnResult' {endToEndResult} -> endToEndResult) (\s@UserTurnResult' {} a -> s {endToEndResult = a} :: UserTurnResult)

-- | Undocumented member.
userTurnResult_errorDetails :: Lens.Lens' UserTurnResult (Prelude.Maybe ExecutionErrorDetails)
userTurnResult_errorDetails = Lens.lens (\UserTurnResult' {errorDetails} -> errorDetails) (\s@UserTurnResult' {} a -> s {errorDetails = a} :: UserTurnResult)

-- | Specifies whether the expected and actual intents match or not.
userTurnResult_intentMatchResult :: Lens.Lens' UserTurnResult (Prelude.Maybe TestResultMatchStatus)
userTurnResult_intentMatchResult = Lens.lens (\UserTurnResult' {intentMatchResult} -> intentMatchResult) (\s@UserTurnResult' {} a -> s {intentMatchResult = a} :: UserTurnResult)

-- | Specifies whether the expected and actual slots match or not.
userTurnResult_slotMatchResult :: Lens.Lens' UserTurnResult (Prelude.Maybe TestResultMatchStatus)
userTurnResult_slotMatchResult = Lens.lens (\UserTurnResult' {slotMatchResult} -> slotMatchResult) (\s@UserTurnResult' {} a -> s {slotMatchResult = a} :: UserTurnResult)

-- | Specifies whether the expected and actual speech transcriptions match or
-- not, or if there is an error in execution.
userTurnResult_speechTranscriptionResult :: Lens.Lens' UserTurnResult (Prelude.Maybe TestResultMatchStatus)
userTurnResult_speechTranscriptionResult = Lens.lens (\UserTurnResult' {speechTranscriptionResult} -> speechTranscriptionResult) (\s@UserTurnResult' {} a -> s {speechTranscriptionResult = a} :: UserTurnResult)

-- | Contains information about the user messages in the turn in the input.
userTurnResult_input :: Lens.Lens' UserTurnResult UserTurnInputSpecification
userTurnResult_input = Lens.lens (\UserTurnResult' {input} -> input) (\s@UserTurnResult' {} a -> s {input = a} :: UserTurnResult)

-- | Contains information about the expected output for the user turn.
userTurnResult_expectedOutput :: Lens.Lens' UserTurnResult UserTurnOutputSpecification
userTurnResult_expectedOutput = Lens.lens (\UserTurnResult' {expectedOutput} -> expectedOutput) (\s@UserTurnResult' {} a -> s {expectedOutput = a} :: UserTurnResult)

instance Data.FromJSON UserTurnResult where
  parseJSON =
    Data.withObject
      "UserTurnResult"
      ( \x ->
          UserTurnResult'
            Prelude.<$> (x Data..:? "actualOutput")
            Prelude.<*> (x Data..:? "conversationLevelResult")
            Prelude.<*> (x Data..:? "endToEndResult")
            Prelude.<*> (x Data..:? "errorDetails")
            Prelude.<*> (x Data..:? "intentMatchResult")
            Prelude.<*> (x Data..:? "slotMatchResult")
            Prelude.<*> (x Data..:? "speechTranscriptionResult")
            Prelude.<*> (x Data..: "input")
            Prelude.<*> (x Data..: "expectedOutput")
      )

instance Prelude.Hashable UserTurnResult where
  hashWithSalt _salt UserTurnResult' {..} =
    _salt
      `Prelude.hashWithSalt` actualOutput
      `Prelude.hashWithSalt` conversationLevelResult
      `Prelude.hashWithSalt` endToEndResult
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` intentMatchResult
      `Prelude.hashWithSalt` slotMatchResult
      `Prelude.hashWithSalt` speechTranscriptionResult
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` expectedOutput

instance Prelude.NFData UserTurnResult where
  rnf UserTurnResult' {..} =
    Prelude.rnf actualOutput
      `Prelude.seq` Prelude.rnf conversationLevelResult
      `Prelude.seq` Prelude.rnf endToEndResult
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf intentMatchResult
      `Prelude.seq` Prelude.rnf slotMatchResult
      `Prelude.seq` Prelude.rnf speechTranscriptionResult
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf expectedOutput
