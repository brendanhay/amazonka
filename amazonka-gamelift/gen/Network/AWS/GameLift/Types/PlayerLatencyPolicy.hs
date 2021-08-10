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
-- Module      : Network.AWS.GameLift.Types.PlayerLatencyPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerLatencyPolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Queue setting that determines the highest latency allowed for individual
-- players when placing a game session. When a latency policy is in force,
-- a game session cannot be placed with any fleet in a Region where a
-- player reports latency higher than the cap. Latency policies are only
-- enforced when the placement request contains player latency information.
--
-- -   CreateGameSessionQueue
--
-- -   DescribeGameSessionQueues
--
-- -   UpdateGameSessionQueue
--
-- -   DeleteGameSessionQueue
--
-- /See:/ 'newPlayerLatencyPolicy' smart constructor.
data PlayerLatencyPolicy = PlayerLatencyPolicy'
  { -- | The length of time, in seconds, that the policy is enforced while
    -- placing a new game session. A null value for this property means that
    -- the policy is enforced until the queue times out.
    policyDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum latency value that is allowed for any player, in
    -- milliseconds. All policies must have a value set for this property.
    maximumIndividualPlayerLatencyMilliseconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlayerLatencyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDurationSeconds', 'playerLatencyPolicy_policyDurationSeconds' - The length of time, in seconds, that the policy is enforced while
-- placing a new game session. A null value for this property means that
-- the policy is enforced until the queue times out.
--
-- 'maximumIndividualPlayerLatencyMilliseconds', 'playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds' - The maximum latency value that is allowed for any player, in
-- milliseconds. All policies must have a value set for this property.
newPlayerLatencyPolicy ::
  PlayerLatencyPolicy
newPlayerLatencyPolicy =
  PlayerLatencyPolicy'
    { policyDurationSeconds =
        Prelude.Nothing,
      maximumIndividualPlayerLatencyMilliseconds =
        Prelude.Nothing
    }

-- | The length of time, in seconds, that the policy is enforced while
-- placing a new game session. A null value for this property means that
-- the policy is enforced until the queue times out.
playerLatencyPolicy_policyDurationSeconds :: Lens.Lens' PlayerLatencyPolicy (Prelude.Maybe Prelude.Natural)
playerLatencyPolicy_policyDurationSeconds = Lens.lens (\PlayerLatencyPolicy' {policyDurationSeconds} -> policyDurationSeconds) (\s@PlayerLatencyPolicy' {} a -> s {policyDurationSeconds = a} :: PlayerLatencyPolicy)

-- | The maximum latency value that is allowed for any player, in
-- milliseconds. All policies must have a value set for this property.
playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds :: Lens.Lens' PlayerLatencyPolicy (Prelude.Maybe Prelude.Natural)
playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds = Lens.lens (\PlayerLatencyPolicy' {maximumIndividualPlayerLatencyMilliseconds} -> maximumIndividualPlayerLatencyMilliseconds) (\s@PlayerLatencyPolicy' {} a -> s {maximumIndividualPlayerLatencyMilliseconds = a} :: PlayerLatencyPolicy)

instance Core.FromJSON PlayerLatencyPolicy where
  parseJSON =
    Core.withObject
      "PlayerLatencyPolicy"
      ( \x ->
          PlayerLatencyPolicy'
            Prelude.<$> (x Core..:? "PolicyDurationSeconds")
            Prelude.<*> ( x
                            Core..:? "MaximumIndividualPlayerLatencyMilliseconds"
                        )
      )

instance Prelude.Hashable PlayerLatencyPolicy

instance Prelude.NFData PlayerLatencyPolicy

instance Core.ToJSON PlayerLatencyPolicy where
  toJSON PlayerLatencyPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PolicyDurationSeconds" Core..=)
              Prelude.<$> policyDurationSeconds,
            ( "MaximumIndividualPlayerLatencyMilliseconds"
                Core..=
            )
              Prelude.<$> maximumIndividualPlayerLatencyMilliseconds
          ]
      )
