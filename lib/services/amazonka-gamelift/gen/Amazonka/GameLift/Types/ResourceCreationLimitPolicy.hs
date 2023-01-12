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
-- Module      : Amazonka.GameLift.Types.ResourceCreationLimitPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.ResourceCreationLimitPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A policy that puts limits on the number of game sessions that a player
-- can create within a specified span of time. With this policy, you can
-- control players\' ability to consume available resources.
--
-- The policy is evaluated when a player tries to create a new game
-- session. On receiving a @CreateGameSession@ request, GameLift checks
-- that the player (identified by @CreatorId@) has created fewer than game
-- session limit in the specified time period.
--
-- /See:/ 'newResourceCreationLimitPolicy' smart constructor.
data ResourceCreationLimitPolicy = ResourceCreationLimitPolicy'
  { -- | A policy that puts limits on the number of game sessions that a player
    -- can create within a specified span of time. With this policy, you can
    -- control players\' ability to consume available resources.
    --
    -- The policy is evaluated when a player tries to create a new game
    -- session. On receiving a @CreateGameSession@ request, GameLift checks
    -- that the player (identified by @CreatorId@) has created fewer than game
    -- session limit in the specified time period.
    newGameSessionsPerCreator' :: Prelude.Maybe Prelude.Natural,
    -- | The time span used in evaluating the resource creation limit policy.
    policyPeriodInMinutes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCreationLimitPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newGameSessionsPerCreator'', 'resourceCreationLimitPolicy_newGameSessionsPerCreator' - A policy that puts limits on the number of game sessions that a player
-- can create within a specified span of time. With this policy, you can
-- control players\' ability to consume available resources.
--
-- The policy is evaluated when a player tries to create a new game
-- session. On receiving a @CreateGameSession@ request, GameLift checks
-- that the player (identified by @CreatorId@) has created fewer than game
-- session limit in the specified time period.
--
-- 'policyPeriodInMinutes', 'resourceCreationLimitPolicy_policyPeriodInMinutes' - The time span used in evaluating the resource creation limit policy.
newResourceCreationLimitPolicy ::
  ResourceCreationLimitPolicy
newResourceCreationLimitPolicy =
  ResourceCreationLimitPolicy'
    { newGameSessionsPerCreator' =
        Prelude.Nothing,
      policyPeriodInMinutes = Prelude.Nothing
    }

-- | A policy that puts limits on the number of game sessions that a player
-- can create within a specified span of time. With this policy, you can
-- control players\' ability to consume available resources.
--
-- The policy is evaluated when a player tries to create a new game
-- session. On receiving a @CreateGameSession@ request, GameLift checks
-- that the player (identified by @CreatorId@) has created fewer than game
-- session limit in the specified time period.
resourceCreationLimitPolicy_newGameSessionsPerCreator :: Lens.Lens' ResourceCreationLimitPolicy (Prelude.Maybe Prelude.Natural)
resourceCreationLimitPolicy_newGameSessionsPerCreator = Lens.lens (\ResourceCreationLimitPolicy' {newGameSessionsPerCreator'} -> newGameSessionsPerCreator') (\s@ResourceCreationLimitPolicy' {} a -> s {newGameSessionsPerCreator' = a} :: ResourceCreationLimitPolicy)

-- | The time span used in evaluating the resource creation limit policy.
resourceCreationLimitPolicy_policyPeriodInMinutes :: Lens.Lens' ResourceCreationLimitPolicy (Prelude.Maybe Prelude.Natural)
resourceCreationLimitPolicy_policyPeriodInMinutes = Lens.lens (\ResourceCreationLimitPolicy' {policyPeriodInMinutes} -> policyPeriodInMinutes) (\s@ResourceCreationLimitPolicy' {} a -> s {policyPeriodInMinutes = a} :: ResourceCreationLimitPolicy)

instance Data.FromJSON ResourceCreationLimitPolicy where
  parseJSON =
    Data.withObject
      "ResourceCreationLimitPolicy"
      ( \x ->
          ResourceCreationLimitPolicy'
            Prelude.<$> (x Data..:? "NewGameSessionsPerCreator")
            Prelude.<*> (x Data..:? "PolicyPeriodInMinutes")
      )

instance Prelude.Hashable ResourceCreationLimitPolicy where
  hashWithSalt _salt ResourceCreationLimitPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` newGameSessionsPerCreator'
      `Prelude.hashWithSalt` policyPeriodInMinutes

instance Prelude.NFData ResourceCreationLimitPolicy where
  rnf ResourceCreationLimitPolicy' {..} =
    Prelude.rnf newGameSessionsPerCreator'
      `Prelude.seq` Prelude.rnf policyPeriodInMinutes

instance Data.ToJSON ResourceCreationLimitPolicy where
  toJSON ResourceCreationLimitPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NewGameSessionsPerCreator" Data..=)
              Prelude.<$> newGameSessionsPerCreator',
            ("PolicyPeriodInMinutes" Data..=)
              Prelude.<$> policyPeriodInMinutes
          ]
      )
