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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for a fleet. An active scaling policy prompts Amazon GameLift to track a certain metric for a fleet and automatically change the fleet's capacity in specific circumstances. Each scaling policy contains one rule statement. Fleets can have multiple scaling policies in force simultaneously.
--
--
-- A scaling policy rule statement has the following structure:
--
-- If @[MetricName]@ is @[ComparisonOperator]@ @[Threshold]@ for @[EvaluationPeriods]@ minutes, then @[ScalingAdjustmentType]@ to/by @[ScalingAdjustment]@ .
--
-- For example, this policy: "If the number of idle instances exceeds 20 for more than 15 minutes, then reduce the fleet capacity by 10 instances" could be implemented as the following rule statement:
--
-- If [IdleInstances] is [GreaterThanOrEqualToThreshold] [20] for [15] minutes, then [ChangeInCapacity] by [-10].
--
-- To create or update a scaling policy, specify a unique combination of name and fleet ID, and set the rule values. All parameters for this action are required. If successful, the policy name is returned. Scaling policies cannot be suspended or made inactive. To stop enforcing a scaling policy, call 'DeleteScalingPolicy' .
--
-- Fleet-related operations include:
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--     * 'DescribeFleetPortSettings'
--
--     * 'DescribeFleetUtilization'
--
--     * 'DescribeRuntimeConfiguration'
--
--     * 'DescribeFleetEvents'
--
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--     * 'UpdateFleetCapacity'
--
--     * 'UpdateFleetPortSettings'
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--     * Manage fleet capacity:
--
--     * 'DescribeFleetCapacity'
--
--     * 'UpdateFleetCapacity'
--
--     * 'PutScalingPolicy' (automatic scaling)
--
--     * 'DescribeScalingPolicies' (automatic scaling)
--
--     * 'DeleteScalingPolicy' (automatic scaling)
--
--     * 'DescribeEC2InstanceLimits'
--
--
--
--     * 'DeleteFleet'
--
--
--
module Network.AWS.GameLift.PutScalingPolicy
    (
    -- * Creating a Request
      putScalingPolicy
    , PutScalingPolicy
    -- * Request Lenses
    , pspName
    , pspFleetId
    , pspScalingAdjustment
    , pspScalingAdjustmentType
    , pspThreshold
    , pspComparisonOperator
    , pspEvaluationPeriods
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
  { _pspName                  :: !Text
  , _pspFleetId               :: !Text
  , _pspScalingAdjustment     :: !Int
  , _pspScalingAdjustmentType :: !ScalingAdjustmentType
  , _pspThreshold             :: !Double
  , _pspComparisonOperator    :: !ComparisonOperatorType
  , _pspEvaluationPeriods     :: !Nat
  , _pspMetricName            :: !MetricName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspName' - Descriptive label that is associated with a scaling policy. Policy names do not need to be unique. A fleet can have only one scaling policy with the same name.
--
-- * 'pspFleetId' - Unique identifier for a fleet to apply this policy to.
--
-- * 'pspScalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
--
-- * 'pspScalingAdjustmentType' - Type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down; for example, a value of "-10" scales the fleet down by 10%.
--
-- * 'pspThreshold' - Metric value used to trigger a scaling event.
--
-- * 'pspComparisonOperator' - Comparison operator to use when measuring the metric against the threshold value.
--
-- * 'pspEvaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
--
-- * 'pspMetricName' - Name of the Amazon GameLift-defined metric that is used to trigger an adjustment.     * __ActivatingGameSessions__ -- number of game sessions in the process of being created (game session status = @ACTIVATING@ ).     * __ActiveGameSessions__ -- number of game sessions currently running (game session status = @ACTIVE@ ).     * __CurrentPlayerSessions__ -- number of active or reserved player sessions (player session status = @ACTIVE@ or @RESERVED@ ).      * __AvailablePlayerSessions__ -- number of player session slots currently available in active game sessions across the fleet, calculated by subtracting a game session's current player session count from its maximum player session count. This number includes game sessions that are not currently accepting players (game session @PlayerSessionCreationPolicy@ = @DENY_ALL@ ).     * __ActiveInstances__ -- number of instances currently running a game session.     * __IdleInstances__ -- number of instances not currently running a game session.
putScalingPolicy
    :: Text -- ^ 'pspName'
    -> Text -- ^ 'pspFleetId'
    -> Int -- ^ 'pspScalingAdjustment'
    -> ScalingAdjustmentType -- ^ 'pspScalingAdjustmentType'
    -> Double -- ^ 'pspThreshold'
    -> ComparisonOperatorType -- ^ 'pspComparisonOperator'
    -> Natural -- ^ 'pspEvaluationPeriods'
    -> MetricName -- ^ 'pspMetricName'
    -> PutScalingPolicy
putScalingPolicy pName_ pFleetId_ pScalingAdjustment_ pScalingAdjustmentType_ pThreshold_ pComparisonOperator_ pEvaluationPeriods_ pMetricName_ =
  PutScalingPolicy'
  { _pspName = pName_
  , _pspFleetId = pFleetId_
  , _pspScalingAdjustment = pScalingAdjustment_
  , _pspScalingAdjustmentType = pScalingAdjustmentType_
  , _pspThreshold = pThreshold_
  , _pspComparisonOperator = pComparisonOperator_
  , _pspEvaluationPeriods = _Nat # pEvaluationPeriods_
  , _pspMetricName = pMetricName_
  }


-- | Descriptive label that is associated with a scaling policy. Policy names do not need to be unique. A fleet can have only one scaling policy with the same name.
pspName :: Lens' PutScalingPolicy Text
pspName = lens _pspName (\ s a -> s{_pspName = a});

-- | Unique identifier for a fleet to apply this policy to.
pspFleetId :: Lens' PutScalingPolicy Text
pspFleetId = lens _pspFleetId (\ s a -> s{_pspFleetId = a});

-- | Amount of adjustment to make, based on the scaling adjustment type.
pspScalingAdjustment :: Lens' PutScalingPolicy Int
pspScalingAdjustment = lens _pspScalingAdjustment (\ s a -> s{_pspScalingAdjustment = a});

-- | Type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down; for example, a value of "-10" scales the fleet down by 10%.
pspScalingAdjustmentType :: Lens' PutScalingPolicy ScalingAdjustmentType
pspScalingAdjustmentType = lens _pspScalingAdjustmentType (\ s a -> s{_pspScalingAdjustmentType = a});

-- | Metric value used to trigger a scaling event.
pspThreshold :: Lens' PutScalingPolicy Double
pspThreshold = lens _pspThreshold (\ s a -> s{_pspThreshold = a});

-- | Comparison operator to use when measuring the metric against the threshold value.
pspComparisonOperator :: Lens' PutScalingPolicy ComparisonOperatorType
pspComparisonOperator = lens _pspComparisonOperator (\ s a -> s{_pspComparisonOperator = a});

-- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
pspEvaluationPeriods :: Lens' PutScalingPolicy Natural
pspEvaluationPeriods = lens _pspEvaluationPeriods (\ s a -> s{_pspEvaluationPeriods = a}) . _Nat;

-- | Name of the Amazon GameLift-defined metric that is used to trigger an adjustment.     * __ActivatingGameSessions__ -- number of game sessions in the process of being created (game session status = @ACTIVATING@ ).     * __ActiveGameSessions__ -- number of game sessions currently running (game session status = @ACTIVE@ ).     * __CurrentPlayerSessions__ -- number of active or reserved player sessions (player session status = @ACTIVE@ or @RESERVED@ ).      * __AvailablePlayerSessions__ -- number of player session slots currently available in active game sessions across the fleet, calculated by subtracting a game session's current player session count from its maximum player session count. This number includes game sessions that are not currently accepting players (game session @PlayerSessionCreationPolicy@ = @DENY_ALL@ ).     * __ActiveInstances__ -- number of instances currently running a game session.     * __IdleInstances__ -- number of instances not currently running a game session.
pspMetricName :: Lens' PutScalingPolicy MetricName
pspMetricName = lens _pspMetricName (\ s a -> s{_pspMetricName = a});

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
                 [Just ("Name" .= _pspName),
                  Just ("FleetId" .= _pspFleetId),
                  Just ("ScalingAdjustment" .= _pspScalingAdjustment),
                  Just
                    ("ScalingAdjustmentType" .=
                       _pspScalingAdjustmentType),
                  Just ("Threshold" .= _pspThreshold),
                  Just
                    ("ComparisonOperator" .= _pspComparisonOperator),
                  Just ("EvaluationPeriods" .= _pspEvaluationPeriods),
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
psprsName = lens _psprsName (\ s a -> s{_psprsName = a});

-- | -- | The response status code.
psprsResponseStatus :: Lens' PutScalingPolicyResponse Int
psprsResponseStatus = lens _psprsResponseStatus (\ s a -> s{_psprsResponseStatus = a});

instance NFData PutScalingPolicyResponse where
