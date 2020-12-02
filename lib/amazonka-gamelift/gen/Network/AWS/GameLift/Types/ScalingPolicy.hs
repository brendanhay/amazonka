{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Rule that controls how a fleet is scaled. Scaling policies are uniquely identified by the combination of name and fleet ID.
--
--
--     * 'DescribeFleetCapacity'
--
--     * 'UpdateFleetCapacity'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--     * 'StopFleetActions'
--
--
--
--
--
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { _spStatus ::
      !(Maybe ScalingStatusType),
    _spScalingAdjustmentType :: !(Maybe ScalingAdjustmentType),
    _spEvaluationPeriods :: !(Maybe Nat),
    _spPolicyType :: !(Maybe PolicyType),
    _spMetricName :: !(Maybe MetricName),
    _spComparisonOperator :: !(Maybe ComparisonOperatorType),
    _spName :: !(Maybe Text),
    _spThreshold :: !(Maybe Double),
    _spScalingAdjustment :: !(Maybe Int),
    _spFleetId :: !(Maybe Text),
    _spTargetConfiguration :: !(Maybe TargetConfiguration)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spStatus' - Current status of the scaling policy. The scaling policy can be in force only when in an @ACTIVE@ status. Scaling policies can be suspended for individual fleets (see 'StopFleetActions' ; if suspended for a fleet, the policy status does not change. View a fleet's stopped actions by calling 'DescribeFleetCapacity' .     * __ACTIVE__ -- The scaling policy can be used for auto-scaling a fleet.     * __UPDATE_REQUESTED__ -- A request to update the scaling policy has been received.     * __UPDATING__ -- A change is being made to the scaling policy.     * __DELETE_REQUESTED__ -- A request to delete the scaling policy has been received.     * __DELETING__ -- The scaling policy is being deleted.     * __DELETED__ -- The scaling policy has been deleted.     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
--
-- * 'spScalingAdjustmentType' - The type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down.
--
-- * 'spEvaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
--
-- * 'spPolicyType' - The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
--
-- * 'spMetricName' - Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> .      * __ActivatingGameSessions__ -- Game sessions in the process of being created.     * __ActiveGameSessions__ -- Game sessions that are currently running.     * __ActiveInstances__ -- Fleet instances that are currently running at least one game session.     * __AvailableGameSessions__ -- Additional game sessions that fleet could host simultaneously, given current capacity.     * __AvailablePlayerSessions__ -- Empty player slots in currently active game sessions. This includes game sessions that are not currently accepting players. Reserved player slots are not included.     * __CurrentPlayerSessions__ -- Player slots in active game sessions that are being used by a player or are reserved for a player.      * __IdleInstances__ -- Active instances that are currently hosting zero game sessions.      * __PercentAvailableGameSessions__ -- Unused percentage of the total number of game sessions that a fleet could host simultaneously, given current capacity. Use this metric for a target-based scaling policy.     * __PercentIdleInstances__ -- Percentage of the total number of active instances that are hosting zero game sessions.     * __QueueDepth__ -- Pending game session placement requests, in any queue, where the current fleet is the top-priority destination.     * __WaitTime__ -- Current wait time for pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
--
-- * 'spComparisonOperator' - Comparison operator to use when measuring a metric against the threshold value.
--
-- * 'spName' - A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- * 'spThreshold' - Metric value used to trigger a scaling event.
--
-- * 'spScalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
--
-- * 'spFleetId' - A unique identifier for a fleet that is associated with this scaling policy.
--
-- * 'spTargetConfiguration' - The settings for a target-based scaling policy.
scalingPolicy ::
  ScalingPolicy
scalingPolicy =
  ScalingPolicy'
    { _spStatus = Nothing,
      _spScalingAdjustmentType = Nothing,
      _spEvaluationPeriods = Nothing,
      _spPolicyType = Nothing,
      _spMetricName = Nothing,
      _spComparisonOperator = Nothing,
      _spName = Nothing,
      _spThreshold = Nothing,
      _spScalingAdjustment = Nothing,
      _spFleetId = Nothing,
      _spTargetConfiguration = Nothing
    }

-- | Current status of the scaling policy. The scaling policy can be in force only when in an @ACTIVE@ status. Scaling policies can be suspended for individual fleets (see 'StopFleetActions' ; if suspended for a fleet, the policy status does not change. View a fleet's stopped actions by calling 'DescribeFleetCapacity' .     * __ACTIVE__ -- The scaling policy can be used for auto-scaling a fleet.     * __UPDATE_REQUESTED__ -- A request to update the scaling policy has been received.     * __UPDATING__ -- A change is being made to the scaling policy.     * __DELETE_REQUESTED__ -- A request to delete the scaling policy has been received.     * __DELETING__ -- The scaling policy is being deleted.     * __DELETED__ -- The scaling policy has been deleted.     * __ERROR__ -- An error occurred in creating the policy. It should be removed and recreated.
spStatus :: Lens' ScalingPolicy (Maybe ScalingStatusType)
spStatus = lens _spStatus (\s a -> s {_spStatus = a})

-- | The type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down.
spScalingAdjustmentType :: Lens' ScalingPolicy (Maybe ScalingAdjustmentType)
spScalingAdjustmentType = lens _spScalingAdjustmentType (\s a -> s {_spScalingAdjustmentType = a})

-- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
spEvaluationPeriods :: Lens' ScalingPolicy (Maybe Natural)
spEvaluationPeriods = lens _spEvaluationPeriods (\s a -> s {_spEvaluationPeriods = a}) . mapping _Nat

-- | The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
spPolicyType :: Lens' ScalingPolicy (Maybe PolicyType)
spPolicyType = lens _spPolicyType (\s a -> s {_spPolicyType = a})

-- | Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> .      * __ActivatingGameSessions__ -- Game sessions in the process of being created.     * __ActiveGameSessions__ -- Game sessions that are currently running.     * __ActiveInstances__ -- Fleet instances that are currently running at least one game session.     * __AvailableGameSessions__ -- Additional game sessions that fleet could host simultaneously, given current capacity.     * __AvailablePlayerSessions__ -- Empty player slots in currently active game sessions. This includes game sessions that are not currently accepting players. Reserved player slots are not included.     * __CurrentPlayerSessions__ -- Player slots in active game sessions that are being used by a player or are reserved for a player.      * __IdleInstances__ -- Active instances that are currently hosting zero game sessions.      * __PercentAvailableGameSessions__ -- Unused percentage of the total number of game sessions that a fleet could host simultaneously, given current capacity. Use this metric for a target-based scaling policy.     * __PercentIdleInstances__ -- Percentage of the total number of active instances that are hosting zero game sessions.     * __QueueDepth__ -- Pending game session placement requests, in any queue, where the current fleet is the top-priority destination.     * __WaitTime__ -- Current wait time for pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
spMetricName :: Lens' ScalingPolicy (Maybe MetricName)
spMetricName = lens _spMetricName (\s a -> s {_spMetricName = a})

-- | Comparison operator to use when measuring a metric against the threshold value.
spComparisonOperator :: Lens' ScalingPolicy (Maybe ComparisonOperatorType)
spComparisonOperator = lens _spComparisonOperator (\s a -> s {_spComparisonOperator = a})

-- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
spName :: Lens' ScalingPolicy (Maybe Text)
spName = lens _spName (\s a -> s {_spName = a})

-- | Metric value used to trigger a scaling event.
spThreshold :: Lens' ScalingPolicy (Maybe Double)
spThreshold = lens _spThreshold (\s a -> s {_spThreshold = a})

-- | Amount of adjustment to make, based on the scaling adjustment type.
spScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
spScalingAdjustment = lens _spScalingAdjustment (\s a -> s {_spScalingAdjustment = a})

-- | A unique identifier for a fleet that is associated with this scaling policy.
spFleetId :: Lens' ScalingPolicy (Maybe Text)
spFleetId = lens _spFleetId (\s a -> s {_spFleetId = a})

-- | The settings for a target-based scaling policy.
spTargetConfiguration :: Lens' ScalingPolicy (Maybe TargetConfiguration)
spTargetConfiguration = lens _spTargetConfiguration (\s a -> s {_spTargetConfiguration = a})

instance FromJSON ScalingPolicy where
  parseJSON =
    withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            <$> (x .:? "Status")
            <*> (x .:? "ScalingAdjustmentType")
            <*> (x .:? "EvaluationPeriods")
            <*> (x .:? "PolicyType")
            <*> (x .:? "MetricName")
            <*> (x .:? "ComparisonOperator")
            <*> (x .:? "Name")
            <*> (x .:? "Threshold")
            <*> (x .:? "ScalingAdjustment")
            <*> (x .:? "FleetId")
            <*> (x .:? "TargetConfiguration")
      )

instance Hashable ScalingPolicy

instance NFData ScalingPolicy
