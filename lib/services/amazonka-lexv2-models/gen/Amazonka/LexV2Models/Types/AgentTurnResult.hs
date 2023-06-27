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
-- Module      : Amazonka.LexV2Models.Types.AgentTurnResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AgentTurnResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ExecutionErrorDetails
import qualified Amazonka.Prelude as Prelude

-- | The information about the agent turn in a test set execution.
--
-- /See:/ 'newAgentTurnResult' smart constructor.
data AgentTurnResult = AgentTurnResult'
  { -- | The actual agent prompt for the agent turn in a test set execution.
    actualAgentPrompt :: Prelude.Maybe Prelude.Text,
    -- | The actual elicited slot for the agent turn in a test set execution.
    actualElicitedSlot :: Prelude.Maybe Prelude.Text,
    -- | The actual intent for the agent turn in a test set execution.
    actualIntent :: Prelude.Maybe Prelude.Text,
    errorDetails :: Prelude.Maybe ExecutionErrorDetails,
    -- | The expected agent prompt for the agent turn in a test set execution.
    expectedAgentPrompt :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentTurnResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actualAgentPrompt', 'agentTurnResult_actualAgentPrompt' - The actual agent prompt for the agent turn in a test set execution.
--
-- 'actualElicitedSlot', 'agentTurnResult_actualElicitedSlot' - The actual elicited slot for the agent turn in a test set execution.
--
-- 'actualIntent', 'agentTurnResult_actualIntent' - The actual intent for the agent turn in a test set execution.
--
-- 'errorDetails', 'agentTurnResult_errorDetails' - Undocumented member.
--
-- 'expectedAgentPrompt', 'agentTurnResult_expectedAgentPrompt' - The expected agent prompt for the agent turn in a test set execution.
newAgentTurnResult ::
  -- | 'expectedAgentPrompt'
  Prelude.Text ->
  AgentTurnResult
newAgentTurnResult pExpectedAgentPrompt_ =
  AgentTurnResult'
    { actualAgentPrompt =
        Prelude.Nothing,
      actualElicitedSlot = Prelude.Nothing,
      actualIntent = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      expectedAgentPrompt = pExpectedAgentPrompt_
    }

-- | The actual agent prompt for the agent turn in a test set execution.
agentTurnResult_actualAgentPrompt :: Lens.Lens' AgentTurnResult (Prelude.Maybe Prelude.Text)
agentTurnResult_actualAgentPrompt = Lens.lens (\AgentTurnResult' {actualAgentPrompt} -> actualAgentPrompt) (\s@AgentTurnResult' {} a -> s {actualAgentPrompt = a} :: AgentTurnResult)

-- | The actual elicited slot for the agent turn in a test set execution.
agentTurnResult_actualElicitedSlot :: Lens.Lens' AgentTurnResult (Prelude.Maybe Prelude.Text)
agentTurnResult_actualElicitedSlot = Lens.lens (\AgentTurnResult' {actualElicitedSlot} -> actualElicitedSlot) (\s@AgentTurnResult' {} a -> s {actualElicitedSlot = a} :: AgentTurnResult)

-- | The actual intent for the agent turn in a test set execution.
agentTurnResult_actualIntent :: Lens.Lens' AgentTurnResult (Prelude.Maybe Prelude.Text)
agentTurnResult_actualIntent = Lens.lens (\AgentTurnResult' {actualIntent} -> actualIntent) (\s@AgentTurnResult' {} a -> s {actualIntent = a} :: AgentTurnResult)

-- | Undocumented member.
agentTurnResult_errorDetails :: Lens.Lens' AgentTurnResult (Prelude.Maybe ExecutionErrorDetails)
agentTurnResult_errorDetails = Lens.lens (\AgentTurnResult' {errorDetails} -> errorDetails) (\s@AgentTurnResult' {} a -> s {errorDetails = a} :: AgentTurnResult)

-- | The expected agent prompt for the agent turn in a test set execution.
agentTurnResult_expectedAgentPrompt :: Lens.Lens' AgentTurnResult Prelude.Text
agentTurnResult_expectedAgentPrompt = Lens.lens (\AgentTurnResult' {expectedAgentPrompt} -> expectedAgentPrompt) (\s@AgentTurnResult' {} a -> s {expectedAgentPrompt = a} :: AgentTurnResult)

instance Data.FromJSON AgentTurnResult where
  parseJSON =
    Data.withObject
      "AgentTurnResult"
      ( \x ->
          AgentTurnResult'
            Prelude.<$> (x Data..:? "actualAgentPrompt")
            Prelude.<*> (x Data..:? "actualElicitedSlot")
            Prelude.<*> (x Data..:? "actualIntent")
            Prelude.<*> (x Data..:? "errorDetails")
            Prelude.<*> (x Data..: "expectedAgentPrompt")
      )

instance Prelude.Hashable AgentTurnResult where
  hashWithSalt _salt AgentTurnResult' {..} =
    _salt
      `Prelude.hashWithSalt` actualAgentPrompt
      `Prelude.hashWithSalt` actualElicitedSlot
      `Prelude.hashWithSalt` actualIntent
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` expectedAgentPrompt

instance Prelude.NFData AgentTurnResult where
  rnf AgentTurnResult' {..} =
    Prelude.rnf actualAgentPrompt
      `Prelude.seq` Prelude.rnf actualElicitedSlot
      `Prelude.seq` Prelude.rnf actualIntent
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf expectedAgentPrompt
