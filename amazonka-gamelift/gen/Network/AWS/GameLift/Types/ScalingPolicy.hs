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
-- Module      : Network.AWS.GameLift.Types.ScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ScalingPolicy where

import Network.AWS.GameLift.Types.ComparisonOperatorType
import Network.AWS.GameLift.Types.MetricName
import Network.AWS.GameLift.Types.PolicyType
import Network.AWS.GameLift.Types.ScalingAdjustmentType
import Network.AWS.GameLift.Types.ScalingStatusType
import Network.AWS.GameLift.Types.TargetConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Rule that controls how a fleet is scaled. Scaling policies are uniquely
-- identified by the combination of name and fleet ID.
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
-- /See:/ 'newScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { -- | Metric value used to trigger a scaling event.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | Current status of the scaling policy. The scaling policy can be in force
    -- only when in an @ACTIVE@ status. Scaling policies can be suspended for
    -- individual fleets (see StopFleetActions; if suspended for a fleet, the
    -- policy status does not change. View a fleet\'s stopped actions by
    -- calling DescribeFleetCapacity.
    --
    -- -   __ACTIVE__ -- The scaling policy can be used for auto-scaling a
    --     fleet.
    --
    -- -   __UPDATE_REQUESTED__ -- A request to update the scaling policy has
    --     been received.
    --
    -- -   __UPDATING__ -- A change is being made to the scaling policy.
    --
    -- -   __DELETE_REQUESTED__ -- A request to delete the scaling policy has
    --     been received.
    --
    -- -   __DELETING__ -- The scaling policy is being deleted.
    --
    -- -   __DELETED__ -- The scaling policy has been deleted.
    --
    -- -   __ERROR__ -- An error occurred in creating the policy. It should be
    --     removed and recreated.
    status :: Prelude.Maybe ScalingStatusType,
    -- | The settings for a target-based scaling policy.
    targetConfiguration :: Prelude.Maybe TargetConfiguration,
    -- | Comparison operator to use when measuring a metric against the threshold
    -- value.
    comparisonOperator :: Prelude.Maybe ComparisonOperatorType,
    -- | A unique identifier for a fleet that is associated with this scaling
    -- policy.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | Name of the Amazon GameLift-defined metric that is used to trigger a
    -- scaling adjustment. For detailed descriptions of fleet metrics, see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch>.
    --
    -- -   __ActivatingGameSessions__ -- Game sessions in the process of being
    --     created.
    --
    -- -   __ActiveGameSessions__ -- Game sessions that are currently running.
    --
    -- -   __ActiveInstances__ -- Fleet instances that are currently running at
    --     least one game session.
    --
    -- -   __AvailableGameSessions__ -- Additional game sessions that fleet
    --     could host simultaneously, given current capacity.
    --
    -- -   __AvailablePlayerSessions__ -- Empty player slots in currently
    --     active game sessions. This includes game sessions that are not
    --     currently accepting players. Reserved player slots are not included.
    --
    -- -   __CurrentPlayerSessions__ -- Player slots in active game sessions
    --     that are being used by a player or are reserved for a player.
    --
    -- -   __IdleInstances__ -- Active instances that are currently hosting
    --     zero game sessions.
    --
    -- -   __PercentAvailableGameSessions__ -- Unused percentage of the total
    --     number of game sessions that a fleet could host simultaneously,
    --     given current capacity. Use this metric for a target-based scaling
    --     policy.
    --
    -- -   __PercentIdleInstances__ -- Percentage of the total number of active
    --     instances that are hosting zero game sessions.
    --
    -- -   __QueueDepth__ -- Pending game session placement requests, in any
    --     queue, where the current fleet is the top-priority destination.
    --
    -- -   __WaitTime__ -- Current wait time for pending game session placement
    --     requests, in any queue, where the current fleet is the top-priority
    --     destination.
    metricName :: Prelude.Maybe MetricName,
    -- | The type of scaling policy to create. For a target-based policy, set the
    -- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
    -- /TargetConfiguration/. For a rule-based policy set the following
    -- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
    -- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
    policyType :: Prelude.Maybe PolicyType,
    -- | Amount of adjustment to make, based on the scaling adjustment type.
    scalingAdjustment :: Prelude.Maybe Prelude.Int,
    -- | A descriptive label that is associated with a scaling policy. Policy
    -- names do not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Length of time (in minutes) the metric must be at or beyond the
    -- threshold before a scaling event is triggered.
    evaluationPeriods :: Prelude.Maybe Prelude.Natural,
    -- | The type of adjustment to make to a fleet\'s instance count (see
    -- FleetCapacity):
    --
    -- -   __ChangeInCapacity__ -- add (or subtract) the scaling adjustment
    --     value from the current instance count. Positive values scale up
    --     while negative values scale down.
    --
    -- -   __ExactCapacity__ -- set the instance count to the scaling
    --     adjustment value.
    --
    -- -   __PercentChangeInCapacity__ -- increase or reduce the current
    --     instance count by the scaling adjustment, read as a percentage.
    --     Positive values scale up while negative values scale down.
    scalingAdjustmentType :: Prelude.Maybe ScalingAdjustmentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threshold', 'scalingPolicy_threshold' - Metric value used to trigger a scaling event.
--
-- 'status', 'scalingPolicy_status' - Current status of the scaling policy. The scaling policy can be in force
-- only when in an @ACTIVE@ status. Scaling policies can be suspended for
-- individual fleets (see StopFleetActions; if suspended for a fleet, the
-- policy status does not change. View a fleet\'s stopped actions by
-- calling DescribeFleetCapacity.
--
-- -   __ACTIVE__ -- The scaling policy can be used for auto-scaling a
--     fleet.
--
-- -   __UPDATE_REQUESTED__ -- A request to update the scaling policy has
--     been received.
--
-- -   __UPDATING__ -- A change is being made to the scaling policy.
--
-- -   __DELETE_REQUESTED__ -- A request to delete the scaling policy has
--     been received.
--
-- -   __DELETING__ -- The scaling policy is being deleted.
--
-- -   __DELETED__ -- The scaling policy has been deleted.
--
-- -   __ERROR__ -- An error occurred in creating the policy. It should be
--     removed and recreated.
--
-- 'targetConfiguration', 'scalingPolicy_targetConfiguration' - The settings for a target-based scaling policy.
--
-- 'comparisonOperator', 'scalingPolicy_comparisonOperator' - Comparison operator to use when measuring a metric against the threshold
-- value.
--
-- 'fleetId', 'scalingPolicy_fleetId' - A unique identifier for a fleet that is associated with this scaling
-- policy.
--
-- 'metricName', 'scalingPolicy_metricName' - Name of the Amazon GameLift-defined metric that is used to trigger a
-- scaling adjustment. For detailed descriptions of fleet metrics, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch>.
--
-- -   __ActivatingGameSessions__ -- Game sessions in the process of being
--     created.
--
-- -   __ActiveGameSessions__ -- Game sessions that are currently running.
--
-- -   __ActiveInstances__ -- Fleet instances that are currently running at
--     least one game session.
--
-- -   __AvailableGameSessions__ -- Additional game sessions that fleet
--     could host simultaneously, given current capacity.
--
-- -   __AvailablePlayerSessions__ -- Empty player slots in currently
--     active game sessions. This includes game sessions that are not
--     currently accepting players. Reserved player slots are not included.
--
-- -   __CurrentPlayerSessions__ -- Player slots in active game sessions
--     that are being used by a player or are reserved for a player.
--
-- -   __IdleInstances__ -- Active instances that are currently hosting
--     zero game sessions.
--
-- -   __PercentAvailableGameSessions__ -- Unused percentage of the total
--     number of game sessions that a fleet could host simultaneously,
--     given current capacity. Use this metric for a target-based scaling
--     policy.
--
-- -   __PercentIdleInstances__ -- Percentage of the total number of active
--     instances that are hosting zero game sessions.
--
-- -   __QueueDepth__ -- Pending game session placement requests, in any
--     queue, where the current fleet is the top-priority destination.
--
-- -   __WaitTime__ -- Current wait time for pending game session placement
--     requests, in any queue, where the current fleet is the top-priority
--     destination.
--
-- 'policyType', 'scalingPolicy_policyType' - The type of scaling policy to create. For a target-based policy, set the
-- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
-- /TargetConfiguration/. For a rule-based policy set the following
-- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
-- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
--
-- 'scalingAdjustment', 'scalingPolicy_scalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
--
-- 'name', 'scalingPolicy_name' - A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique.
--
-- 'evaluationPeriods', 'scalingPolicy_evaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the
-- threshold before a scaling event is triggered.
--
-- 'scalingAdjustmentType', 'scalingPolicy_scalingAdjustmentType' - The type of adjustment to make to a fleet\'s instance count (see
-- FleetCapacity):
--
-- -   __ChangeInCapacity__ -- add (or subtract) the scaling adjustment
--     value from the current instance count. Positive values scale up
--     while negative values scale down.
--
-- -   __ExactCapacity__ -- set the instance count to the scaling
--     adjustment value.
--
-- -   __PercentChangeInCapacity__ -- increase or reduce the current
--     instance count by the scaling adjustment, read as a percentage.
--     Positive values scale up while negative values scale down.
newScalingPolicy ::
  ScalingPolicy
newScalingPolicy =
  ScalingPolicy'
    { threshold = Prelude.Nothing,
      status = Prelude.Nothing,
      targetConfiguration = Prelude.Nothing,
      comparisonOperator = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      metricName = Prelude.Nothing,
      policyType = Prelude.Nothing,
      scalingAdjustment = Prelude.Nothing,
      name = Prelude.Nothing,
      evaluationPeriods = Prelude.Nothing,
      scalingAdjustmentType = Prelude.Nothing
    }

-- | Metric value used to trigger a scaling event.
scalingPolicy_threshold :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Double)
scalingPolicy_threshold = Lens.lens (\ScalingPolicy' {threshold} -> threshold) (\s@ScalingPolicy' {} a -> s {threshold = a} :: ScalingPolicy)

-- | Current status of the scaling policy. The scaling policy can be in force
-- only when in an @ACTIVE@ status. Scaling policies can be suspended for
-- individual fleets (see StopFleetActions; if suspended for a fleet, the
-- policy status does not change. View a fleet\'s stopped actions by
-- calling DescribeFleetCapacity.
--
-- -   __ACTIVE__ -- The scaling policy can be used for auto-scaling a
--     fleet.
--
-- -   __UPDATE_REQUESTED__ -- A request to update the scaling policy has
--     been received.
--
-- -   __UPDATING__ -- A change is being made to the scaling policy.
--
-- -   __DELETE_REQUESTED__ -- A request to delete the scaling policy has
--     been received.
--
-- -   __DELETING__ -- The scaling policy is being deleted.
--
-- -   __DELETED__ -- The scaling policy has been deleted.
--
-- -   __ERROR__ -- An error occurred in creating the policy. It should be
--     removed and recreated.
scalingPolicy_status :: Lens.Lens' ScalingPolicy (Prelude.Maybe ScalingStatusType)
scalingPolicy_status = Lens.lens (\ScalingPolicy' {status} -> status) (\s@ScalingPolicy' {} a -> s {status = a} :: ScalingPolicy)

-- | The settings for a target-based scaling policy.
scalingPolicy_targetConfiguration :: Lens.Lens' ScalingPolicy (Prelude.Maybe TargetConfiguration)
scalingPolicy_targetConfiguration = Lens.lens (\ScalingPolicy' {targetConfiguration} -> targetConfiguration) (\s@ScalingPolicy' {} a -> s {targetConfiguration = a} :: ScalingPolicy)

-- | Comparison operator to use when measuring a metric against the threshold
-- value.
scalingPolicy_comparisonOperator :: Lens.Lens' ScalingPolicy (Prelude.Maybe ComparisonOperatorType)
scalingPolicy_comparisonOperator = Lens.lens (\ScalingPolicy' {comparisonOperator} -> comparisonOperator) (\s@ScalingPolicy' {} a -> s {comparisonOperator = a} :: ScalingPolicy)

-- | A unique identifier for a fleet that is associated with this scaling
-- policy.
scalingPolicy_fleetId :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_fleetId = Lens.lens (\ScalingPolicy' {fleetId} -> fleetId) (\s@ScalingPolicy' {} a -> s {fleetId = a} :: ScalingPolicy)

-- | Name of the Amazon GameLift-defined metric that is used to trigger a
-- scaling adjustment. For detailed descriptions of fleet metrics, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch>.
--
-- -   __ActivatingGameSessions__ -- Game sessions in the process of being
--     created.
--
-- -   __ActiveGameSessions__ -- Game sessions that are currently running.
--
-- -   __ActiveInstances__ -- Fleet instances that are currently running at
--     least one game session.
--
-- -   __AvailableGameSessions__ -- Additional game sessions that fleet
--     could host simultaneously, given current capacity.
--
-- -   __AvailablePlayerSessions__ -- Empty player slots in currently
--     active game sessions. This includes game sessions that are not
--     currently accepting players. Reserved player slots are not included.
--
-- -   __CurrentPlayerSessions__ -- Player slots in active game sessions
--     that are being used by a player or are reserved for a player.
--
-- -   __IdleInstances__ -- Active instances that are currently hosting
--     zero game sessions.
--
-- -   __PercentAvailableGameSessions__ -- Unused percentage of the total
--     number of game sessions that a fleet could host simultaneously,
--     given current capacity. Use this metric for a target-based scaling
--     policy.
--
-- -   __PercentIdleInstances__ -- Percentage of the total number of active
--     instances that are hosting zero game sessions.
--
-- -   __QueueDepth__ -- Pending game session placement requests, in any
--     queue, where the current fleet is the top-priority destination.
--
-- -   __WaitTime__ -- Current wait time for pending game session placement
--     requests, in any queue, where the current fleet is the top-priority
--     destination.
scalingPolicy_metricName :: Lens.Lens' ScalingPolicy (Prelude.Maybe MetricName)
scalingPolicy_metricName = Lens.lens (\ScalingPolicy' {metricName} -> metricName) (\s@ScalingPolicy' {} a -> s {metricName = a} :: ScalingPolicy)

-- | The type of scaling policy to create. For a target-based policy, set the
-- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
-- /TargetConfiguration/. For a rule-based policy set the following
-- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
-- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
scalingPolicy_policyType :: Lens.Lens' ScalingPolicy (Prelude.Maybe PolicyType)
scalingPolicy_policyType = Lens.lens (\ScalingPolicy' {policyType} -> policyType) (\s@ScalingPolicy' {} a -> s {policyType = a} :: ScalingPolicy)

-- | Amount of adjustment to make, based on the scaling adjustment type.
scalingPolicy_scalingAdjustment :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Int)
scalingPolicy_scalingAdjustment = Lens.lens (\ScalingPolicy' {scalingAdjustment} -> scalingAdjustment) (\s@ScalingPolicy' {} a -> s {scalingAdjustment = a} :: ScalingPolicy)

-- | A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique.
scalingPolicy_name :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_name = Lens.lens (\ScalingPolicy' {name} -> name) (\s@ScalingPolicy' {} a -> s {name = a} :: ScalingPolicy)

-- | Length of time (in minutes) the metric must be at or beyond the
-- threshold before a scaling event is triggered.
scalingPolicy_evaluationPeriods :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Natural)
scalingPolicy_evaluationPeriods = Lens.lens (\ScalingPolicy' {evaluationPeriods} -> evaluationPeriods) (\s@ScalingPolicy' {} a -> s {evaluationPeriods = a} :: ScalingPolicy)

-- | The type of adjustment to make to a fleet\'s instance count (see
-- FleetCapacity):
--
-- -   __ChangeInCapacity__ -- add (or subtract) the scaling adjustment
--     value from the current instance count. Positive values scale up
--     while negative values scale down.
--
-- -   __ExactCapacity__ -- set the instance count to the scaling
--     adjustment value.
--
-- -   __PercentChangeInCapacity__ -- increase or reduce the current
--     instance count by the scaling adjustment, read as a percentage.
--     Positive values scale up while negative values scale down.
scalingPolicy_scalingAdjustmentType :: Lens.Lens' ScalingPolicy (Prelude.Maybe ScalingAdjustmentType)
scalingPolicy_scalingAdjustmentType = Lens.lens (\ScalingPolicy' {scalingAdjustmentType} -> scalingAdjustmentType) (\s@ScalingPolicy' {} a -> s {scalingAdjustmentType = a} :: ScalingPolicy)

instance Prelude.FromJSON ScalingPolicy where
  parseJSON =
    Prelude.withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            Prelude.<$> (x Prelude..:? "Threshold")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "TargetConfiguration")
            Prelude.<*> (x Prelude..:? "ComparisonOperator")
            Prelude.<*> (x Prelude..:? "FleetId")
            Prelude.<*> (x Prelude..:? "MetricName")
            Prelude.<*> (x Prelude..:? "PolicyType")
            Prelude.<*> (x Prelude..:? "ScalingAdjustment")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "EvaluationPeriods")
            Prelude.<*> (x Prelude..:? "ScalingAdjustmentType")
      )

instance Prelude.Hashable ScalingPolicy

instance Prelude.NFData ScalingPolicy
