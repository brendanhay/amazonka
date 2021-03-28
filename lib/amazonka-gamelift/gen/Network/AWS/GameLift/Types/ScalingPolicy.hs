{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.ScalingPolicy
  ( ScalingPolicy (..)
  -- * Smart constructor
  , mkScalingPolicy
  -- * Lenses
  , spComparisonOperator
  , spEvaluationPeriods
  , spFleetId
  , spMetricName
  , spName
  , spPolicyType
  , spScalingAdjustment
  , spScalingAdjustmentType
  , spStatus
  , spTargetConfiguration
  , spThreshold
  ) where

import qualified Network.AWS.GameLift.Types.ComparisonOperatorType as Types
import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.GameLift.Types.MetricName as Types
import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.PolicyType as Types
import qualified Network.AWS.GameLift.Types.ScalingAdjustmentType as Types
import qualified Network.AWS.GameLift.Types.ScalingStatusType as Types
import qualified Network.AWS.GameLift.Types.TargetConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { comparisonOperator :: Core.Maybe Types.ComparisonOperatorType
    -- ^ Comparison operator to use when measuring a metric against the threshold value.
  , evaluationPeriods :: Core.Maybe Core.Natural
    -- ^ Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
  , fleetId :: Core.Maybe Types.FleetId
    -- ^ A unique identifier for a fleet that is associated with this scaling policy.
  , metricName :: Core.Maybe Types.MetricName
    -- ^ Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> . 
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
  , name :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
  , policyType :: Core.Maybe Types.PolicyType
    -- ^ The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
  , scalingAdjustment :: Core.Maybe Core.Int
    -- ^ Amount of adjustment to make, based on the scaling adjustment type.
  , scalingAdjustmentType :: Core.Maybe Types.ScalingAdjustmentType
    -- ^ The type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):
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
  , status :: Core.Maybe Types.ScalingStatusType
    -- ^ Current status of the scaling policy. The scaling policy can be in force only when in an @ACTIVE@ status. Scaling policies can be suspended for individual fleets (see 'StopFleetActions' ; if suspended for a fleet, the policy status does not change. View a fleet's stopped actions by calling 'DescribeFleetCapacity' .
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
  , targetConfiguration :: Core.Maybe Types.TargetConfiguration
    -- ^ The settings for a target-based scaling policy.
  , threshold :: Core.Maybe Core.Double
    -- ^ Metric value used to trigger a scaling event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingPolicy' value with any optional fields omitted.
mkScalingPolicy
    :: ScalingPolicy
mkScalingPolicy
  = ScalingPolicy'{comparisonOperator = Core.Nothing,
                   evaluationPeriods = Core.Nothing, fleetId = Core.Nothing,
                   metricName = Core.Nothing, name = Core.Nothing,
                   policyType = Core.Nothing, scalingAdjustment = Core.Nothing,
                   scalingAdjustmentType = Core.Nothing, status = Core.Nothing,
                   targetConfiguration = Core.Nothing, threshold = Core.Nothing}

-- | Comparison operator to use when measuring a metric against the threshold value.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spComparisonOperator :: Lens.Lens' ScalingPolicy (Core.Maybe Types.ComparisonOperatorType)
spComparisonOperator = Lens.field @"comparisonOperator"
{-# INLINEABLE spComparisonOperator #-}
{-# DEPRECATED comparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead"  #-}

-- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spEvaluationPeriods :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Natural)
spEvaluationPeriods = Lens.field @"evaluationPeriods"
{-# INLINEABLE spEvaluationPeriods #-}
{-# DEPRECATED evaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead"  #-}

-- | A unique identifier for a fleet that is associated with this scaling policy.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spFleetId :: Lens.Lens' ScalingPolicy (Core.Maybe Types.FleetId)
spFleetId = Lens.field @"fleetId"
{-# INLINEABLE spFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

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
spMetricName :: Lens.Lens' ScalingPolicy (Core.Maybe Types.MetricName)
spMetricName = Lens.field @"metricName"
{-# INLINEABLE spMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spName :: Lens.Lens' ScalingPolicy (Core.Maybe Types.NonZeroAndMaxString)
spName = Lens.field @"name"
{-# INLINEABLE spName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyType :: Lens.Lens' ScalingPolicy (Core.Maybe Types.PolicyType)
spPolicyType = Lens.field @"policyType"
{-# INLINEABLE spPolicyType #-}
{-# DEPRECATED policyType "Use generic-lens or generic-optics with 'policyType' instead"  #-}

-- | Amount of adjustment to make, based on the scaling adjustment type.
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spScalingAdjustment :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Int)
spScalingAdjustment = Lens.field @"scalingAdjustment"
{-# INLINEABLE spScalingAdjustment #-}
{-# DEPRECATED scalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead"  #-}

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
spScalingAdjustmentType :: Lens.Lens' ScalingPolicy (Core.Maybe Types.ScalingAdjustmentType)
spScalingAdjustmentType = Lens.field @"scalingAdjustmentType"
{-# INLINEABLE spScalingAdjustmentType #-}
{-# DEPRECATED scalingAdjustmentType "Use generic-lens or generic-optics with 'scalingAdjustmentType' instead"  #-}

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
spStatus :: Lens.Lens' ScalingPolicy (Core.Maybe Types.ScalingStatusType)
spStatus = Lens.field @"status"
{-# INLINEABLE spStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The settings for a target-based scaling policy.
--
-- /Note:/ Consider using 'targetConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTargetConfiguration :: Lens.Lens' ScalingPolicy (Core.Maybe Types.TargetConfiguration)
spTargetConfiguration = Lens.field @"targetConfiguration"
{-# INLINEABLE spTargetConfiguration #-}
{-# DEPRECATED targetConfiguration "Use generic-lens or generic-optics with 'targetConfiguration' instead"  #-}

-- | Metric value used to trigger a scaling event.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spThreshold :: Lens.Lens' ScalingPolicy (Core.Maybe Core.Double)
spThreshold = Lens.field @"threshold"
{-# INLINEABLE spThreshold #-}
{-# DEPRECATED threshold "Use generic-lens or generic-optics with 'threshold' instead"  #-}

instance Core.FromJSON ScalingPolicy where
        parseJSON
          = Core.withObject "ScalingPolicy" Core.$
              \ x ->
                ScalingPolicy' Core.<$>
                  (x Core..:? "ComparisonOperator") Core.<*>
                    x Core..:? "EvaluationPeriods"
                    Core.<*> x Core..:? "FleetId"
                    Core.<*> x Core..:? "MetricName"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "PolicyType"
                    Core.<*> x Core..:? "ScalingAdjustment"
                    Core.<*> x Core..:? "ScalingAdjustmentType"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "TargetConfiguration"
                    Core.<*> x Core..:? "Threshold"
