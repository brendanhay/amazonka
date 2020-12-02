{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.PutScalingPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for a fleet. Scaling policies are used to automatically scale a fleet's hosting capacity to meet player demand. An active scaling policy instructs Amazon GameLift to track a fleet metric and automatically change the fleet's capacity when a certain threshold is reached. There are two types of scaling policies: target-based and rule-based. Use a target-based policy to quickly and efficiently manage fleet scaling; this option is the most commonly used. Use rule-based policies when you need to exert fine-grained control over auto-scaling.
--
--
-- Fleets can have multiple scaling policies of each type in force at the same time; you can have one target-based policy, one or multiple rule-based scaling policies, or both. We recommend caution, however, because multiple auto-scaling policies can have unintended consequences.
--
-- You can temporarily suspend all scaling policies for a fleet by calling 'StopFleetActions' with the fleet action AUTO_SCALING. To resume scaling policies, call 'StartFleetActions' with the same fleet action. To stop just one scaling policy--or to permanently remove it, you must delete the policy with 'DeleteScalingPolicy' .
--
-- Learn more about how to work with auto-scaling in <http://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-autoscaling.html Set Up Fleet Automatic Scaling> .
--
-- __Target-based policy__
--
-- A target-based policy tracks a single metric: PercentAvailableGameSessions. This metric tells us how much of a fleet's hosting capacity is ready to host game sessions but is not currently in use. This is the fleet's buffer; it measures the additional player demand that the fleet could handle at current capacity. With a target-based policy, you set your ideal buffer size and leave it to Amazon GameLift to take whatever action is needed to maintain that target.
--
-- For example, you might choose to maintain a 10% buffer for a fleet that has the capacity to host 100 simultaneous game sessions. This policy tells Amazon GameLift to take action whenever the fleet's available capacity falls below or rises above 10 game sessions. Amazon GameLift will start new instances or stop unused instances in order to return to the 10% buffer.
--
-- To create or update a target-based policy, specify a fleet ID and name, and set the policy type to "TargetBased". Specify the metric to track (PercentAvailableGameSessions) and reference a 'TargetConfiguration' object with your desired buffer value. Exclude all other parameters. On a successful request, the policy name is returned. The scaling policy is automatically in force as soon as it's successfully created. If the fleet's auto-scaling actions are temporarily suspended, the new policy will be in force once the fleet actions are restarted.
--
-- __Rule-based policy__
--
-- A rule-based policy tracks specified fleet metric, sets a threshold value, and specifies the type of action to initiate when triggered. With a rule-based policy, you can select from several available fleet metrics. Each policy specifies whether to scale up or scale down (and by how much), so you need one policy for each type of action.
--
-- For example, a policy may make the following statement: "If the percentage of idle instances is greater than 20% for more than 15 minutes, then reduce the fleet capacity by 10%."
--
-- A policy's rule statement has the following structure:
--
-- If @[MetricName]@ is @[ComparisonOperator]@ @[Threshold]@ for @[EvaluationPeriods]@ minutes, then @[ScalingAdjustmentType]@ to/by @[ScalingAdjustment]@ .
--
-- To implement the example, the rule statement would look like this:
--
-- If @[PercentIdleInstances]@ is @[GreaterThanThreshold]@ @[20]@ for @[15]@ minutes, then @[PercentChangeInCapacity]@ to/by @[10]@ .
--
-- To create or update a scaling policy, specify a unique combination of name and fleet ID, and set the policy type to "RuleBased". Specify the parameter values for a policy rule statement. On a successful request, the policy name is returned. Scaling policies are automatically in force as soon as they're successfully created. If the fleet's auto-scaling actions are temporarily suspended, the new policy will be in force once the fleet actions are restarted.
--
-- Operations related to fleet capacity scaling include:
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
module Network.AWS.GameLift.PutScalingPolicy
    (
    -- * Creating a Request
      putScalingPolicy
    , PutScalingPolicy
    -- * Request Lenses
    , pspScalingAdjustmentType
    , pspEvaluationPeriods
    , pspPolicyType
    , pspComparisonOperator
    , pspThreshold
    , pspScalingAdjustment
    , pspTargetConfiguration
    , pspName
    , pspFleetId
    , pspMetricName

    -- * Destructuring the Response
    , putScalingPolicyResponse
    , PutScalingPolicyResponse
    -- * Response Lenses
    , psprsName
    , psprsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'putScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { _pspScalingAdjustmentType :: !(Maybe ScalingAdjustmentType)
  , _pspEvaluationPeriods     :: !(Maybe Nat)
  , _pspPolicyType            :: !(Maybe PolicyType)
  , _pspComparisonOperator    :: !(Maybe ComparisonOperatorType)
  , _pspThreshold             :: !(Maybe Double)
  , _pspScalingAdjustment     :: !(Maybe Int)
  , _pspTargetConfiguration   :: !(Maybe TargetConfiguration)
  , _pspName                  :: !Text
  , _pspFleetId               :: !Text
  , _pspMetricName            :: !MetricName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspScalingAdjustmentType' - Type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down; for example, a value of "-10" scales the fleet down by 10%.
--
-- * 'pspEvaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
--
-- * 'pspPolicyType' - Type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
--
-- * 'pspComparisonOperator' - Comparison operator to use when measuring the metric against the threshold value.
--
-- * 'pspThreshold' - Metric value used to trigger a scaling event.
--
-- * 'pspScalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
--
-- * 'pspTargetConfiguration' - Object that contains settings for a target-based scaling policy.
--
-- * 'pspName' - Descriptive label that is associated with a scaling policy. Policy names do not need to be unique. A fleet can have only one scaling policy with the same name.
--
-- * 'pspFleetId' - Unique identifier for a fleet to apply this policy to. The fleet cannot be in any of the following statuses: ERROR or DELETING.
--
-- * 'pspMetricName' - Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> .      * __ActivatingGameSessions__ -- Game sessions in the process of being created.     * __ActiveGameSessions__ -- Game sessions that are currently running.     * __ActiveInstances__ -- Fleet instances that are currently running at least one game session.     * __AvailableGameSessions__ -- Additional game sessions that fleet could host simultaneously, given current capacity.     * __AvailablePlayerSessions__ -- Empty player slots in currently active game sessions. This includes game sessions that are not currently accepting players. Reserved player slots are not included.     * __CurrentPlayerSessions__ -- Player slots in active game sessions that are being used by a player or are reserved for a player.      * __IdleInstances__ -- Active instances that are currently hosting zero game sessions.      * __PercentAvailableGameSessions__ -- Unused percentage of the total number of game sessions that a fleet could host simultaneously, given current capacity. Use this metric for a target-based scaling policy.     * __PercentIdleInstances__ -- Percentage of the total number of active instances that are hosting zero game sessions.     * __QueueDepth__ -- Pending game session placement requests, in any queue, where the current fleet is the top-priority destination.     * __WaitTime__ -- Current wait time for pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
putScalingPolicy
    :: Text -- ^ 'pspName'
    -> Text -- ^ 'pspFleetId'
    -> MetricName -- ^ 'pspMetricName'
    -> PutScalingPolicy
putScalingPolicy pName_ pFleetId_ pMetricName_ =
  PutScalingPolicy'
    { _pspScalingAdjustmentType = Nothing
    , _pspEvaluationPeriods = Nothing
    , _pspPolicyType = Nothing
    , _pspComparisonOperator = Nothing
    , _pspThreshold = Nothing
    , _pspScalingAdjustment = Nothing
    , _pspTargetConfiguration = Nothing
    , _pspName = pName_
    , _pspFleetId = pFleetId_
    , _pspMetricName = pMetricName_
    }


-- | Type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down; for example, a value of "-10" scales the fleet down by 10%.
pspScalingAdjustmentType :: Lens' PutScalingPolicy (Maybe ScalingAdjustmentType)
pspScalingAdjustmentType = lens _pspScalingAdjustmentType (\ s a -> s{_pspScalingAdjustmentType = a})

-- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
pspEvaluationPeriods :: Lens' PutScalingPolicy (Maybe Natural)
pspEvaluationPeriods = lens _pspEvaluationPeriods (\ s a -> s{_pspEvaluationPeriods = a}) . mapping _Nat

-- | Type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
pspPolicyType :: Lens' PutScalingPolicy (Maybe PolicyType)
pspPolicyType = lens _pspPolicyType (\ s a -> s{_pspPolicyType = a})

-- | Comparison operator to use when measuring the metric against the threshold value.
pspComparisonOperator :: Lens' PutScalingPolicy (Maybe ComparisonOperatorType)
pspComparisonOperator = lens _pspComparisonOperator (\ s a -> s{_pspComparisonOperator = a})

-- | Metric value used to trigger a scaling event.
pspThreshold :: Lens' PutScalingPolicy (Maybe Double)
pspThreshold = lens _pspThreshold (\ s a -> s{_pspThreshold = a})

-- | Amount of adjustment to make, based on the scaling adjustment type.
pspScalingAdjustment :: Lens' PutScalingPolicy (Maybe Int)
pspScalingAdjustment = lens _pspScalingAdjustment (\ s a -> s{_pspScalingAdjustment = a})

-- | Object that contains settings for a target-based scaling policy.
pspTargetConfiguration :: Lens' PutScalingPolicy (Maybe TargetConfiguration)
pspTargetConfiguration = lens _pspTargetConfiguration (\ s a -> s{_pspTargetConfiguration = a})

-- | Descriptive label that is associated with a scaling policy. Policy names do not need to be unique. A fleet can have only one scaling policy with the same name.
pspName :: Lens' PutScalingPolicy Text
pspName = lens _pspName (\ s a -> s{_pspName = a})

-- | Unique identifier for a fleet to apply this policy to. The fleet cannot be in any of the following statuses: ERROR or DELETING.
pspFleetId :: Lens' PutScalingPolicy Text
pspFleetId = lens _pspFleetId (\ s a -> s{_pspFleetId = a})

-- | Name of the Amazon GameLift-defined metric that is used to trigger a scaling adjustment. For detailed descriptions of fleet metrics, see <http://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch> .      * __ActivatingGameSessions__ -- Game sessions in the process of being created.     * __ActiveGameSessions__ -- Game sessions that are currently running.     * __ActiveInstances__ -- Fleet instances that are currently running at least one game session.     * __AvailableGameSessions__ -- Additional game sessions that fleet could host simultaneously, given current capacity.     * __AvailablePlayerSessions__ -- Empty player slots in currently active game sessions. This includes game sessions that are not currently accepting players. Reserved player slots are not included.     * __CurrentPlayerSessions__ -- Player slots in active game sessions that are being used by a player or are reserved for a player.      * __IdleInstances__ -- Active instances that are currently hosting zero game sessions.      * __PercentAvailableGameSessions__ -- Unused percentage of the total number of game sessions that a fleet could host simultaneously, given current capacity. Use this metric for a target-based scaling policy.     * __PercentIdleInstances__ -- Percentage of the total number of active instances that are hosting zero game sessions.     * __QueueDepth__ -- Pending game session placement requests, in any queue, where the current fleet is the top-priority destination.     * __WaitTime__ -- Current wait time for pending game session placement requests, in any queue, where the current fleet is the top-priority destination.
pspMetricName :: Lens' PutScalingPolicy MetricName
pspMetricName = lens _pspMetricName (\ s a -> s{_pspMetricName = a})

instance AWSRequest PutScalingPolicy where
        type Rs PutScalingPolicy = PutScalingPolicyResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 PutScalingPolicyResponse' <$>
                   (x .?> "Name") <*> (pure (fromEnum s)))

instance Hashable PutScalingPolicy where

instance NFData PutScalingPolicy where

instance ToHeaders PutScalingPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.PutScalingPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutScalingPolicy where
        toJSON PutScalingPolicy'{..}
          = object
              (catMaybes
                 [("ScalingAdjustmentType" .=) <$>
                    _pspScalingAdjustmentType,
                  ("EvaluationPeriods" .=) <$> _pspEvaluationPeriods,
                  ("PolicyType" .=) <$> _pspPolicyType,
                  ("ComparisonOperator" .=) <$> _pspComparisonOperator,
                  ("Threshold" .=) <$> _pspThreshold,
                  ("ScalingAdjustment" .=) <$> _pspScalingAdjustment,
                  ("TargetConfiguration" .=) <$>
                    _pspTargetConfiguration,
                  Just ("Name" .= _pspName),
                  Just ("FleetId" .= _pspFleetId),
                  Just ("MetricName" .= _pspMetricName)])

instance ToPath PutScalingPolicy where
        toPath = const "/"

instance ToQuery PutScalingPolicy where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'putScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { _psprsName           :: !(Maybe Text)
  , _psprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psprsName' - Descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- * 'psprsResponseStatus' - -- | The response status code.
putScalingPolicyResponse
    :: Int -- ^ 'psprsResponseStatus'
    -> PutScalingPolicyResponse
putScalingPolicyResponse pResponseStatus_ =
  PutScalingPolicyResponse'
    {_psprsName = Nothing, _psprsResponseStatus = pResponseStatus_}


-- | Descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
psprsName :: Lens' PutScalingPolicyResponse (Maybe Text)
psprsName = lens _psprsName (\ s a -> s{_psprsName = a})

-- | -- | The response status code.
psprsResponseStatus :: Lens' PutScalingPolicyResponse Int
psprsResponseStatus = lens _psprsResponseStatus (\ s a -> s{_psprsResponseStatus = a})

instance NFData PutScalingPolicyResponse where
