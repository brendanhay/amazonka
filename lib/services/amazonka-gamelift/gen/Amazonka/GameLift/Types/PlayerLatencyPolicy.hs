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
-- Module      : Amazonka.GameLift.Types.PlayerLatencyPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.PlayerLatencyPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Sets a latency cap for individual players when placing a game session.
-- With a latency policy in force, a game session cannot be placed in a
-- fleet location where a player reports latency higher than the cap.
-- Latency policies are used only with placement request that provide
-- player latency information. Player latency policies can be stacked to
-- gradually relax latency requirements over time.
--
-- Latency policies are part of a GameSessionQueue.
--
-- /See:/ 'newPlayerLatencyPolicy' smart constructor.
data PlayerLatencyPolicy = PlayerLatencyPolicy'
  { -- | The maximum latency value that is allowed for any player, in
    -- milliseconds. All policies must have a value set for this property.
    maximumIndividualPlayerLatencyMilliseconds :: Prelude.Maybe Prelude.Natural,
    -- | The length of time, in seconds, that the policy is enforced while
    -- placing a new game session. A null value for this property means that
    -- the policy is enforced until the queue times out.
    policyDurationSeconds :: Prelude.Maybe Prelude.Natural
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
-- 'maximumIndividualPlayerLatencyMilliseconds', 'playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds' - The maximum latency value that is allowed for any player, in
-- milliseconds. All policies must have a value set for this property.
--
-- 'policyDurationSeconds', 'playerLatencyPolicy_policyDurationSeconds' - The length of time, in seconds, that the policy is enforced while
-- placing a new game session. A null value for this property means that
-- the policy is enforced until the queue times out.
newPlayerLatencyPolicy ::
  PlayerLatencyPolicy
newPlayerLatencyPolicy =
  PlayerLatencyPolicy'
    { maximumIndividualPlayerLatencyMilliseconds =
        Prelude.Nothing,
      policyDurationSeconds = Prelude.Nothing
    }

-- | The maximum latency value that is allowed for any player, in
-- milliseconds. All policies must have a value set for this property.
playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds :: Lens.Lens' PlayerLatencyPolicy (Prelude.Maybe Prelude.Natural)
playerLatencyPolicy_maximumIndividualPlayerLatencyMilliseconds = Lens.lens (\PlayerLatencyPolicy' {maximumIndividualPlayerLatencyMilliseconds} -> maximumIndividualPlayerLatencyMilliseconds) (\s@PlayerLatencyPolicy' {} a -> s {maximumIndividualPlayerLatencyMilliseconds = a} :: PlayerLatencyPolicy)

-- | The length of time, in seconds, that the policy is enforced while
-- placing a new game session. A null value for this property means that
-- the policy is enforced until the queue times out.
playerLatencyPolicy_policyDurationSeconds :: Lens.Lens' PlayerLatencyPolicy (Prelude.Maybe Prelude.Natural)
playerLatencyPolicy_policyDurationSeconds = Lens.lens (\PlayerLatencyPolicy' {policyDurationSeconds} -> policyDurationSeconds) (\s@PlayerLatencyPolicy' {} a -> s {policyDurationSeconds = a} :: PlayerLatencyPolicy)

instance Core.FromJSON PlayerLatencyPolicy where
  parseJSON =
    Core.withObject
      "PlayerLatencyPolicy"
      ( \x ->
          PlayerLatencyPolicy'
            Prelude.<$> ( x
                            Core..:? "MaximumIndividualPlayerLatencyMilliseconds"
                        )
            Prelude.<*> (x Core..:? "PolicyDurationSeconds")
      )

instance Prelude.Hashable PlayerLatencyPolicy where
  hashWithSalt _salt PlayerLatencyPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` maximumIndividualPlayerLatencyMilliseconds
      `Prelude.hashWithSalt` policyDurationSeconds

instance Prelude.NFData PlayerLatencyPolicy where
  rnf PlayerLatencyPolicy' {..} =
    Prelude.rnf
      maximumIndividualPlayerLatencyMilliseconds
      `Prelude.seq` Prelude.rnf policyDurationSeconds

instance Core.ToJSON PlayerLatencyPolicy where
  toJSON PlayerLatencyPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ( "MaximumIndividualPlayerLatencyMilliseconds"
                Core..=
            )
              Prelude.<$> maximumIndividualPlayerLatencyMilliseconds,
            ("PolicyDurationSeconds" Core..=)
              Prelude.<$> policyDurationSeconds
          ]
      )
