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
-- Module      : Network.AWS.GameLift.Types.TargetConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.TargetConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for a target-based scaling policy (see ScalingPolicy. A
-- target-based policy tracks a particular fleet metric specifies a target
-- value for the metric. As player usage changes, the policy triggers
-- Amazon GameLift to adjust capacity so that the metric returns to the
-- target value. The target configuration specifies settings as needed for
-- the target based policy, including the target value.
--
-- -   DescribeFleetCapacity
--
-- -   UpdateFleetCapacity
--
-- -   DescribeEC2InstanceLimits
--
-- -   Manage scaling policies:
--
--     -   PutScalingPolicy (auto-scaling)
--
--     -   DescribeScalingPolicies (auto-scaling)
--
--     -   DeleteScalingPolicy (auto-scaling)
--
-- -   Manage fleet actions:
--
--     -   StartFleetActions
--
--     -   StopFleetActions
--
-- /See:/ 'newTargetConfiguration' smart constructor.
data TargetConfiguration = TargetConfiguration'
  { -- | Desired value to use with a target-based scaling policy. The value must
    -- be relevant for whatever metric the scaling policy is using. For
    -- example, in a policy using the metric PercentAvailableGameSessions, the
    -- target value should be the preferred size of the fleet\'s buffer (the
    -- percent of capacity that should be idle and ready for new game
    -- sessions).
    targetValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetValue', 'targetConfiguration_targetValue' - Desired value to use with a target-based scaling policy. The value must
-- be relevant for whatever metric the scaling policy is using. For
-- example, in a policy using the metric PercentAvailableGameSessions, the
-- target value should be the preferred size of the fleet\'s buffer (the
-- percent of capacity that should be idle and ready for new game
-- sessions).
newTargetConfiguration ::
  -- | 'targetValue'
  Prelude.Double ->
  TargetConfiguration
newTargetConfiguration pTargetValue_ =
  TargetConfiguration' {targetValue = pTargetValue_}

-- | Desired value to use with a target-based scaling policy. The value must
-- be relevant for whatever metric the scaling policy is using. For
-- example, in a policy using the metric PercentAvailableGameSessions, the
-- target value should be the preferred size of the fleet\'s buffer (the
-- percent of capacity that should be idle and ready for new game
-- sessions).
targetConfiguration_targetValue :: Lens.Lens' TargetConfiguration Prelude.Double
targetConfiguration_targetValue = Lens.lens (\TargetConfiguration' {targetValue} -> targetValue) (\s@TargetConfiguration' {} a -> s {targetValue = a} :: TargetConfiguration)

instance Prelude.FromJSON TargetConfiguration where
  parseJSON =
    Prelude.withObject
      "TargetConfiguration"
      ( \x ->
          TargetConfiguration'
            Prelude.<$> (x Prelude..: "TargetValue")
      )

instance Prelude.Hashable TargetConfiguration

instance Prelude.NFData TargetConfiguration

instance Prelude.ToJSON TargetConfiguration where
  toJSON TargetConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TargetValue" Prelude..= targetValue)
          ]
      )
