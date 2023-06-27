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
-- Module      : Amazonka.LexV2Models.Types.AgentTurnSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AgentTurnSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The specification of an agent turn.
--
-- /See:/ 'newAgentTurnSpecification' smart constructor.
data AgentTurnSpecification = AgentTurnSpecification'
  { -- | The agent prompt for the agent turn in a test set.
    agentPrompt :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentTurnSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentPrompt', 'agentTurnSpecification_agentPrompt' - The agent prompt for the agent turn in a test set.
newAgentTurnSpecification ::
  -- | 'agentPrompt'
  Prelude.Text ->
  AgentTurnSpecification
newAgentTurnSpecification pAgentPrompt_ =
  AgentTurnSpecification'
    { agentPrompt =
        pAgentPrompt_
    }

-- | The agent prompt for the agent turn in a test set.
agentTurnSpecification_agentPrompt :: Lens.Lens' AgentTurnSpecification Prelude.Text
agentTurnSpecification_agentPrompt = Lens.lens (\AgentTurnSpecification' {agentPrompt} -> agentPrompt) (\s@AgentTurnSpecification' {} a -> s {agentPrompt = a} :: AgentTurnSpecification)

instance Data.FromJSON AgentTurnSpecification where
  parseJSON =
    Data.withObject
      "AgentTurnSpecification"
      ( \x ->
          AgentTurnSpecification'
            Prelude.<$> (x Data..: "agentPrompt")
      )

instance Prelude.Hashable AgentTurnSpecification where
  hashWithSalt _salt AgentTurnSpecification' {..} =
    _salt `Prelude.hashWithSalt` agentPrompt

instance Prelude.NFData AgentTurnSpecification where
  rnf AgentTurnSpecification' {..} =
    Prelude.rnf agentPrompt
