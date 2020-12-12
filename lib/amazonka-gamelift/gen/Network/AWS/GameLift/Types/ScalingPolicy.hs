{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ScalingPolicy
  ( ScalingPolicy (..),

    -- * Smart constructor
    mkScalingPolicy,

    -- * Lenses
    spStatus,
    spScalingAdjustmentType,
    spEvaluationPeriods,
    spPolicyType,
    spMetricName,
    spComparisonOperator,
    spName,
    spThreshold,
    spScalingAdjustment,
    spFleetId,
    spTargetConfiguration,
  )
where

import Network.AWS.GameLift.Types.ComparisonOperatorType
import Network.AWS.GameLift.Types.MetricName
import Network.AWS.GameLift.Types.PolicyType
import Network.AWS.GameLift.Types.ScalingAdjustmentType
import Network.AWS.GameLift.Types.ScalingStatusType
import Network.AWS.GameLift.Types.TargetConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Rule that controls how a fleet is scaled. Scaling policies are uniquely identified by the combination of name and fleet ID.
--
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'UpdateFleetCapacity'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--
--     * 'StopFleetActions'
--
--
--
--
--
-- /See:/ 'mkScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { status ::
      Lude.Maybe ScalingStatusType,
    scalingAdjustmentType :: Lude.Maybe ScalingAdjustmentType,
    evaluationPeriods :: Lude.Maybe Lude.Natural,
    policyType :: Lude.Maybe PolicyType,
    metricName :: Lude.Maybe MetricName,
    comparisonOperator :: Lude.Maybe ComparisonOperatorType,
    name :: Lude.Maybe Lude.Text,
    threshold :: Lude.Maybe Lude.Double,
    scalingAdjustment :: Lude.Maybe Lude.Int,
    fleetId :: Lude.Maybe Lude.Text,
    targetConfiguration :: Lude.Maybe TargetConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- * 'comparisonOperator' - Comparison operator to use when measuring a metric against the threshold value.
-- * 'evaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
-- * 'fleetId' - A unique identifier for a fleet that is associated with this scaling policy.
-- * 'metricName' - Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> .
--
--
--     * __ActivatingGameSessions__ -- Game sessions in the process of being created.
--
--
--     * __ActiveGameSessions__ -- Game sessions that are currently running.
--
--
--     * __ActiveInstances__ -- Fleet instances that are currently running at least one game session.
--
--
--     * __AvailableGameSessions__ -- Additional game sessions that fleet could host simultaneously, given current capacity.
--
--
--     * __AvailablePlayerSessions__ -- Empty player slots in currently active game sessions. This includes game sessions that are not currently accepting players. Reserved player slots are not included.
--
--
--     * __CurrentPlayerSessions__ -- Player slots in active game sessions that are being used by a player or are reserved for a player.
--
--
--     * __IdleInstances__ -- Active instances that are currently hosting zero game sessions.
--
--
--     * __PercentAvailableGameSessions__ -- Unused percentage of the total number of game sessions that a fleet could host simultaneously, given current capacity. Use this metric for a target-based scaling policy.
--
--
--     * __PercentIdleInstances__ -- Percentage of the total number of active instances that are hosting zero game sessions.
--
--
--     * __QueueDepth__ -- Pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
--
--
--     * __WaitTime__ -- Current wait time for pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
--
--
-- * 'name' - A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
-- * 'policyType' - The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
-- * 'scalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
-- * 'scalingAdjustmentType' - The type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):
--
--
--     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.
--
--
--     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.
--
--
--     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down.
--
--
-- * 'status' - Current status of the scaling policy. The scaling policy can be in force only when in an @ACTIVE@ status. Scaling policies can be suspended for individual fleets (see 'StopFleetActions' ; if suspended for a fleet, the policy status does not change. View a fleet's stopped actions by calling 'DescribeFleetCapacity' .
--
--
--     * __ACTIVE__ -- The scaling policy can be used for auto-scaling a fleet.
--
--
--     * __UPDATE_REQUESTED__ -- A request to update the scaling policy has been received.
--
--
--     * __UPDATING__ -- A change is being made to the scaling policy.
--
--
--     * __DELETE_REQUESTED__ -- A request to delete the scaling policy has been received.
--
--
--     * __DELETING__ -- The scaling policy is being deleted.
--
--
--     * __DELETED__ -- The scaling policy has been deleted.
--
--
--     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
--
--
-- * 'targetConfiguration' - The settings for a target-based scaling policy.
-- * 'threshold' - Metric value used to trigger a scaling event.
mkScalingPolicy ::
  ScalingPolicy
mkScalingPolicy =
  ScalingPolicy'
    { status = Lude.Nothing,
      scalingAdjustmentType = Lude.Nothing,
      evaluationPeriods = Lude.Nothing,
      policyType = Lude.Nothing,
      metricName = Lude.Nothing,
      comparisonOperator = Lude.Nothing,
      name = Lude.Nothing,
      threshold = Lude.Nothing,
      scalingAdjustment = Lude.Nothing,
      fleetId = Lude.Nothing,
      targetConfiguration = Lude.Nothing
    }

-- | Current status of the scaling policy. The scaling policy can be in force only when in an @ACTIVE@ status. Scaling policies can be suspended for individual fleets (see 'StopFleetActions' ; if suspended for a fleet, the policy status does not change. View a fleet's stopped actions by calling 'DescribeFleetCapacity' .
--
--
--     * __ACTIVE__ -- The scaling policy can be used for auto-scaling a fleet.
--
--
--     * __UPDATE_REQUESTED__ -- A request to update the scaling policy has been received.
--
--
--     * __UPDATING__ -- A change is being made to the scaling policy.
--
--
--     * __DELETE_REQUESTED__ -- A request to delete the scaling policy has been received.
--
--
--     * __DELETING__ -- The scaling policy is being deleted.
--
--
--     * __DELETED__ -- The scaling policy has been deleted.
--
--
--     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spStatus :: Lens.Lens' ScalingPolicy (Lude.Maybe ScalingStatusType)
spStatus = Lens.lens (status :: ScalingPolicy -> Lude.Maybe ScalingStatusType) (\s a -> s {status = a} :: ScalingPolicy)
{-# DEPRECATED spStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):
--
--
--     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.
--
--
--     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.
--
--
--     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down.
--
--
--
-- /Note:/ Consider using 'scalingAdjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spScalingAdjustmentType :: Lens.Lens' ScalingPolicy (Lude.Maybe ScalingAdjustmentType)
spScalingAdjustmentType = Lens.lens (scalingAdjustmentType :: ScalingPolicy -> Lude.Maybe ScalingAdjustmentType) (\s a -> s {scalingAdjustmentType = a} :: ScalingPolicy)
{-# DEPRECATED spScalingAdjustmentType "Use generic-lens or generic-optics with 'scalingAdjustmentType' instead." #-}

-- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spEvaluationPeriods :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Natural)
spEvaluationPeriods = Lens.lens (evaluationPeriods :: ScalingPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {evaluationPeriods = a} :: ScalingPolicy)
{-# DEPRECATED spEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyType :: Lens.Lens' ScalingPolicy (Lude.Maybe PolicyType)
spPolicyType = Lens.lens (policyType :: ScalingPolicy -> Lude.Maybe PolicyType) (\s a -> s {policyType = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> .
--
--
--     * __ActivatingGameSessions__ -- Game sessions in the process of being created.
--
--
--     * __ActiveGameSessions__ -- Game sessions that are currently running.
--
--
--     * __ActiveInstances__ -- Fleet instances that are currently running at least one game session.
--
--
--     * __AvailableGameSessions__ -- Additional game sessions that fleet could host simultaneously, given current capacity.
--
--
--     * __AvailablePlayerSessions__ -- Empty player slots in currently active game sessions. This includes game sessions that are not currently accepting players. Reserved player slots are not included.
--
--
--     * __CurrentPlayerSessions__ -- Player slots in active game sessions that are being used by a player or are reserved for a player.
--
--
--     * __IdleInstances__ -- Active instances that are currently hosting zero game sessions.
--
--
--     * __PercentAvailableGameSessions__ -- Unused percentage of the total number of game sessions that a fleet could host simultaneously, given current capacity. Use this metric for a target-based scaling policy.
--
--
--     * __PercentIdleInstances__ -- Percentage of the total number of active instances that are hosting zero game sessions.
--
--
--     * __QueueDepth__ -- Pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
--
--
--     * __WaitTime__ -- Current wait time for pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
--
--
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMetricName :: Lens.Lens' ScalingPolicy (Lude.Maybe MetricName)
spMetricName = Lens.lens (metricName :: ScalingPolicy -> Lude.Maybe MetricName) (\s a -> s {metricName = a} :: ScalingPolicy)
{-# DEPRECATED spMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | Comparison operator to use when measuring a metric against the threshold value.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spComparisonOperator :: Lens.Lens' ScalingPolicy (Lude.Maybe ComparisonOperatorType)
spComparisonOperator = Lens.lens (comparisonOperator :: ScalingPolicy -> Lude.Maybe ComparisonOperatorType) (\s a -> s {comparisonOperator = a} :: ScalingPolicy)
{-# DEPRECATED spComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spName :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
spName = Lens.lens (name :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ScalingPolicy)
{-# DEPRECATED spName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Metric value used to trigger a scaling event.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spThreshold :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Double)
spThreshold = Lens.lens (threshold :: ScalingPolicy -> Lude.Maybe Lude.Double) (\s a -> s {threshold = a} :: ScalingPolicy)
{-# DEPRECATED spThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | Amount of adjustment to make, based on the scaling adjustment type.
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spScalingAdjustment :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Int)
spScalingAdjustment = Lens.lens (scalingAdjustment :: ScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {scalingAdjustment = a} :: ScalingPolicy)
{-# DEPRECATED spScalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead." #-}

-- | A unique identifier for a fleet that is associated with this scaling policy.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spFleetId :: Lens.Lens' ScalingPolicy (Lude.Maybe Lude.Text)
spFleetId = Lens.lens (fleetId :: ScalingPolicy -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: ScalingPolicy)
{-# DEPRECATED spFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The settings for a target-based scaling policy.
--
-- /Note:/ Consider using 'targetConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTargetConfiguration :: Lens.Lens' ScalingPolicy (Lude.Maybe TargetConfiguration)
spTargetConfiguration = Lens.lens (targetConfiguration :: ScalingPolicy -> Lude.Maybe TargetConfiguration) (\s a -> s {targetConfiguration = a} :: ScalingPolicy)
{-# DEPRECATED spTargetConfiguration "Use generic-lens or generic-optics with 'targetConfiguration' instead." #-}

instance Lude.FromJSON ScalingPolicy where
  parseJSON =
    Lude.withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ScalingAdjustmentType")
            Lude.<*> (x Lude..:? "EvaluationPeriods")
            Lude.<*> (x Lude..:? "PolicyType")
            Lude.<*> (x Lude..:? "MetricName")
            Lude.<*> (x Lude..:? "ComparisonOperator")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Threshold")
            Lude.<*> (x Lude..:? "ScalingAdjustment")
            Lude.<*> (x Lude..:? "FleetId")
            Lude.<*> (x Lude..:? "TargetConfiguration")
      )
