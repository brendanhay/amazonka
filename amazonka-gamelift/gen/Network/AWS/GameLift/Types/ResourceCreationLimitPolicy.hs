{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ResourceCreationLimitPolicy where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A policy that limits the number of game sessions a player can create on
-- the same fleet. This optional policy gives game owners control over how
-- players can consume available game server resources. A resource creation
-- policy makes the following statement: \"An individual player can create
-- a maximum number of new game sessions within a specified time period\".
--
-- The policy is evaluated when a player tries to create a new game
-- session. For example: Assume you have a policy of 10 new game sessions
-- and a time period of 60 minutes. On receiving a @CreateGameSession@
-- request, Amazon GameLift checks that the player (identified by
-- @CreatorId@) has created fewer than 10 game sessions in the past 60
-- minutes.
--
-- /See:/ 'newResourceCreationLimitPolicy' smart constructor.
data ResourceCreationLimitPolicy = ResourceCreationLimitPolicy'
  { -- | The time span used in evaluating the resource creation limit policy.
    policyPeriodInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of game sessions that an individual can create during
    -- the policy period.
    newGameSessionsPerCreator' :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceCreationLimitPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyPeriodInMinutes', 'resourceCreationLimitPolicy_policyPeriodInMinutes' - The time span used in evaluating the resource creation limit policy.
--
-- 'newGameSessionsPerCreator'', 'resourceCreationLimitPolicy_newGameSessionsPerCreator' - The maximum number of game sessions that an individual can create during
-- the policy period.
newResourceCreationLimitPolicy ::
  ResourceCreationLimitPolicy
newResourceCreationLimitPolicy =
  ResourceCreationLimitPolicy'
    { policyPeriodInMinutes =
        Prelude.Nothing,
      newGameSessionsPerCreator' = Prelude.Nothing
    }

-- | The time span used in evaluating the resource creation limit policy.
resourceCreationLimitPolicy_policyPeriodInMinutes :: Lens.Lens' ResourceCreationLimitPolicy (Prelude.Maybe Prelude.Natural)
resourceCreationLimitPolicy_policyPeriodInMinutes = Lens.lens (\ResourceCreationLimitPolicy' {policyPeriodInMinutes} -> policyPeriodInMinutes) (\s@ResourceCreationLimitPolicy' {} a -> s {policyPeriodInMinutes = a} :: ResourceCreationLimitPolicy)

-- | The maximum number of game sessions that an individual can create during
-- the policy period.
resourceCreationLimitPolicy_newGameSessionsPerCreator :: Lens.Lens' ResourceCreationLimitPolicy (Prelude.Maybe Prelude.Natural)
resourceCreationLimitPolicy_newGameSessionsPerCreator = Lens.lens (\ResourceCreationLimitPolicy' {newGameSessionsPerCreator'} -> newGameSessionsPerCreator') (\s@ResourceCreationLimitPolicy' {} a -> s {newGameSessionsPerCreator' = a} :: ResourceCreationLimitPolicy)

instance Prelude.FromJSON ResourceCreationLimitPolicy where
  parseJSON =
    Prelude.withObject
      "ResourceCreationLimitPolicy"
      ( \x ->
          ResourceCreationLimitPolicy'
            Prelude.<$> (x Prelude..:? "PolicyPeriodInMinutes")
            Prelude.<*> (x Prelude..:? "NewGameSessionsPerCreator")
      )

instance Prelude.Hashable ResourceCreationLimitPolicy

instance Prelude.NFData ResourceCreationLimitPolicy

instance Prelude.ToJSON ResourceCreationLimitPolicy where
  toJSON ResourceCreationLimitPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PolicyPeriodInMinutes" Prelude..=)
              Prelude.<$> policyPeriodInMinutes,
            ("NewGameSessionsPerCreator" Prelude..=)
              Prelude.<$> newGameSessionsPerCreator'
          ]
      )
