{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.PutScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for a fleet. Scaling policies are used to automatically scale a fleet's hosting capacity to meet player demand. An active scaling policy instructs Amazon GameLift to track a fleet metric and automatically change the fleet's capacity when a certain threshold is reached. There are two types of scaling policies: target-based and rule-based. Use a target-based policy to quickly and efficiently manage fleet scaling; this option is the most commonly used. Use rule-based policies when you need to exert fine-grained control over auto-scaling.
--
-- Fleets can have multiple scaling policies of each type in force at the same time; you can have one target-based policy, one or multiple rule-based scaling policies, or both. We recommend caution, however, because multiple auto-scaling policies can have unintended consequences.
-- You can temporarily suspend all scaling policies for a fleet by calling 'StopFleetActions' with the fleet action AUTO_SCALING. To resume scaling policies, call 'StartFleetActions' with the same fleet action. To stop just one scaling policy--or to permanently remove it, you must delete the policy with 'DeleteScalingPolicy' .
-- Learn more about how to work with auto-scaling in <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-autoscaling.html Set Up Fleet Automatic Scaling> .
-- __Target-based policy__
-- A target-based policy tracks a single metric: PercentAvailableGameSessions. This metric tells us how much of a fleet's hosting capacity is ready to host game sessions but is not currently in use. This is the fleet's buffer; it measures the additional player demand that the fleet could handle at current capacity. With a target-based policy, you set your ideal buffer size and leave it to Amazon GameLift to take whatever action is needed to maintain that target.
-- For example, you might choose to maintain a 10% buffer for a fleet that has the capacity to host 100 simultaneous game sessions. This policy tells Amazon GameLift to take action whenever the fleet's available capacity falls below or rises above 10 game sessions. Amazon GameLift will start new instances or stop unused instances in order to return to the 10% buffer.
-- To create or update a target-based policy, specify a fleet ID and name, and set the policy type to "TargetBased". Specify the metric to track (PercentAvailableGameSessions) and reference a 'TargetConfiguration' object with your desired buffer value. Exclude all other parameters. On a successful request, the policy name is returned. The scaling policy is automatically in force as soon as it's successfully created. If the fleet's auto-scaling actions are temporarily suspended, the new policy will be in force once the fleet actions are restarted.
-- __Rule-based policy__
-- A rule-based policy tracks specified fleet metric, sets a threshold value, and specifies the type of action to initiate when triggered. With a rule-based policy, you can select from several available fleet metrics. Each policy specifies whether to scale up or scale down (and by how much), so you need one policy for each type of action.
-- For example, a policy may make the following statement: "If the percentage of idle instances is greater than 20% for more than 15 minutes, then reduce the fleet capacity by 10%."
-- A policy's rule statement has the following structure:
-- If @[MetricName]@ is @[ComparisonOperator]@ @[Threshold]@ for @[EvaluationPeriods]@ minutes, then @[ScalingAdjustmentType]@ to/by @[ScalingAdjustment]@ .
-- To implement the example, the rule statement would look like this:
-- If @[PercentIdleInstances]@ is @[GreaterThanThreshold]@ @[20]@ for @[15]@ minutes, then @[PercentChangeInCapacity]@ to/by @[10]@ .
-- To create or update a scaling policy, specify a unique combination of name and fleet ID, and set the policy type to "RuleBased". Specify the parameter values for a policy rule statement. On a successful request, the policy name is returned. Scaling policies are automatically in force as soon as they're successfully created. If the fleet's auto-scaling actions are temporarily suspended, the new policy will be in force once the fleet actions are restarted.
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
module Network.AWS.GameLift.PutScalingPolicy
  ( -- * Creating a request
    PutScalingPolicy (..),
    mkPutScalingPolicy,

    -- ** Request lenses
    pspScalingAdjustmentType,
    pspEvaluationPeriods,
    pspPolicyType,
    pspMetricName,
    pspComparisonOperator,
    pspName,
    pspThreshold,
    pspScalingAdjustment,
    pspFleetId,
    pspTargetConfiguration,

    -- * Destructuring the response
    PutScalingPolicyResponse (..),
    mkPutScalingPolicyResponse,

    -- ** Response lenses
    psprsName,
    psprsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkPutScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { -- | The type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):
    --
    --
    --     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.
    --
    --
    --     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.
    --
    --
    --     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down; for example, a value of "-10" scales the fleet down by 10%.
    scalingAdjustmentType :: Lude.Maybe ScalingAdjustmentType,
    -- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
    evaluationPeriods :: Lude.Maybe Lude.Natural,
    -- | The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
    policyType :: Lude.Maybe PolicyType,
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
    metricName :: MetricName,
    -- | Comparison operator to use when measuring the metric against the threshold value.
    comparisonOperator :: Lude.Maybe ComparisonOperatorType,
    -- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique. A fleet can have only one scaling policy with the same name.
    name :: Lude.Text,
    -- | Metric value used to trigger a scaling event.
    threshold :: Lude.Maybe Lude.Double,
    -- | Amount of adjustment to make, based on the scaling adjustment type.
    scalingAdjustment :: Lude.Maybe Lude.Int,
    -- | A unique identifier for a fleet to apply this policy to. You can use either the fleet ID or ARN value. The fleet cannot be in any of the following statuses: ERROR or DELETING.
    fleetId :: Lude.Text,
    -- | The settings for a target-based scaling policy.
    targetConfiguration :: Lude.Maybe TargetConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- * 'scalingAdjustmentType' - The type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):
--
--
--     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.
--
--
--     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.
--
--
--     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down; for example, a value of "-10" scales the fleet down by 10%.
--
--
-- * 'evaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
-- * 'policyType' - The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
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
-- * 'comparisonOperator' - Comparison operator to use when measuring the metric against the threshold value.
-- * 'name' - A descriptive label that is associated with a scaling policy. Policy names do not need to be unique. A fleet can have only one scaling policy with the same name.
-- * 'threshold' - Metric value used to trigger a scaling event.
-- * 'scalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
-- * 'fleetId' - A unique identifier for a fleet to apply this policy to. You can use either the fleet ID or ARN value. The fleet cannot be in any of the following statuses: ERROR or DELETING.
-- * 'targetConfiguration' - The settings for a target-based scaling policy.
mkPutScalingPolicy ::
  -- | 'metricName'
  MetricName ->
  -- | 'name'
  Lude.Text ->
  -- | 'fleetId'
  Lude.Text ->
  PutScalingPolicy
mkPutScalingPolicy pMetricName_ pName_ pFleetId_ =
  PutScalingPolicy'
    { scalingAdjustmentType = Lude.Nothing,
      evaluationPeriods = Lude.Nothing,
      policyType = Lude.Nothing,
      metricName = pMetricName_,
      comparisonOperator = Lude.Nothing,
      name = pName_,
      threshold = Lude.Nothing,
      scalingAdjustment = Lude.Nothing,
      fleetId = pFleetId_,
      targetConfiguration = Lude.Nothing
    }

-- | The type of adjustment to make to a fleet's instance count (see 'FleetCapacity' ):
--
--
--     * __ChangeInCapacity__ -- add (or subtract) the scaling adjustment value from the current instance count. Positive values scale up while negative values scale down.
--
--
--     * __ExactCapacity__ -- set the instance count to the scaling adjustment value.
--
--
--     * __PercentChangeInCapacity__ -- increase or reduce the current instance count by the scaling adjustment, read as a percentage. Positive values scale up while negative values scale down; for example, a value of "-10" scales the fleet down by 10%.
--
--
--
-- /Note:/ Consider using 'scalingAdjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspScalingAdjustmentType :: Lens.Lens' PutScalingPolicy (Lude.Maybe ScalingAdjustmentType)
pspScalingAdjustmentType = Lens.lens (scalingAdjustmentType :: PutScalingPolicy -> Lude.Maybe ScalingAdjustmentType) (\s a -> s {scalingAdjustmentType = a} :: PutScalingPolicy)
{-# DEPRECATED pspScalingAdjustmentType "Use generic-lens or generic-optics with 'scalingAdjustmentType' instead." #-}

-- | Length of time (in minutes) the metric must be at or beyond the threshold before a scaling event is triggered.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspEvaluationPeriods :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Natural)
pspEvaluationPeriods = Lens.lens (evaluationPeriods :: PutScalingPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {evaluationPeriods = a} :: PutScalingPolicy)
{-# DEPRECATED pspEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The type of scaling policy to create. For a target-based policy, set the parameter /MetricName/ to 'PercentAvailableGameSessions' and specify a /TargetConfiguration/ . For a rule-based policy set the following parameters: /MetricName/ , /ComparisonOperator/ , /Threshold/ , /EvaluationPeriods/ , /ScalingAdjustmentType/ , and /ScalingAdjustment/ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspPolicyType :: Lens.Lens' PutScalingPolicy (Lude.Maybe PolicyType)
pspPolicyType = Lens.lens (policyType :: PutScalingPolicy -> Lude.Maybe PolicyType) (\s a -> s {policyType = a} :: PutScalingPolicy)
{-# DEPRECATED pspPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

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
pspMetricName :: Lens.Lens' PutScalingPolicy MetricName
pspMetricName = Lens.lens (metricName :: PutScalingPolicy -> MetricName) (\s a -> s {metricName = a} :: PutScalingPolicy)
{-# DEPRECATED pspMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | Comparison operator to use when measuring the metric against the threshold value.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspComparisonOperator :: Lens.Lens' PutScalingPolicy (Lude.Maybe ComparisonOperatorType)
pspComparisonOperator = Lens.lens (comparisonOperator :: PutScalingPolicy -> Lude.Maybe ComparisonOperatorType) (\s a -> s {comparisonOperator = a} :: PutScalingPolicy)
{-# DEPRECATED pspComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique. A fleet can have only one scaling policy with the same name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspName :: Lens.Lens' PutScalingPolicy Lude.Text
pspName = Lens.lens (name :: PutScalingPolicy -> Lude.Text) (\s a -> s {name = a} :: PutScalingPolicy)
{-# DEPRECATED pspName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Metric value used to trigger a scaling event.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspThreshold :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Double)
pspThreshold = Lens.lens (threshold :: PutScalingPolicy -> Lude.Maybe Lude.Double) (\s a -> s {threshold = a} :: PutScalingPolicy)
{-# DEPRECATED pspThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | Amount of adjustment to make, based on the scaling adjustment type.
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspScalingAdjustment :: Lens.Lens' PutScalingPolicy (Lude.Maybe Lude.Int)
pspScalingAdjustment = Lens.lens (scalingAdjustment :: PutScalingPolicy -> Lude.Maybe Lude.Int) (\s a -> s {scalingAdjustment = a} :: PutScalingPolicy)
{-# DEPRECATED pspScalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead." #-}

-- | A unique identifier for a fleet to apply this policy to. You can use either the fleet ID or ARN value. The fleet cannot be in any of the following statuses: ERROR or DELETING.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspFleetId :: Lens.Lens' PutScalingPolicy Lude.Text
pspFleetId = Lens.lens (fleetId :: PutScalingPolicy -> Lude.Text) (\s a -> s {fleetId = a} :: PutScalingPolicy)
{-# DEPRECATED pspFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The settings for a target-based scaling policy.
--
-- /Note:/ Consider using 'targetConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pspTargetConfiguration :: Lens.Lens' PutScalingPolicy (Lude.Maybe TargetConfiguration)
pspTargetConfiguration = Lens.lens (targetConfiguration :: PutScalingPolicy -> Lude.Maybe TargetConfiguration) (\s a -> s {targetConfiguration = a} :: PutScalingPolicy)
{-# DEPRECATED pspTargetConfiguration "Use generic-lens or generic-optics with 'targetConfiguration' instead." #-}

instance Lude.AWSRequest PutScalingPolicy where
  type Rs PutScalingPolicy = PutScalingPolicyResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutScalingPolicyResponse'
            Lude.<$> (x Lude..?> "Name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutScalingPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.PutScalingPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutScalingPolicy where
  toJSON PutScalingPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ScalingAdjustmentType" Lude..=) Lude.<$> scalingAdjustmentType,
            ("EvaluationPeriods" Lude..=) Lude.<$> evaluationPeriods,
            ("PolicyType" Lude..=) Lude.<$> policyType,
            Lude.Just ("MetricName" Lude..= metricName),
            ("ComparisonOperator" Lude..=) Lude.<$> comparisonOperator,
            Lude.Just ("Name" Lude..= name),
            ("Threshold" Lude..=) Lude.<$> threshold,
            ("ScalingAdjustment" Lude..=) Lude.<$> scalingAdjustment,
            Lude.Just ("FleetId" Lude..= fleetId),
            ("TargetConfiguration" Lude..=) Lude.<$> targetConfiguration
          ]
      )

instance Lude.ToPath PutScalingPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutScalingPolicy where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkPutScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { -- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- * 'name' - A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
-- * 'responseStatus' - The response status code.
mkPutScalingPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutScalingPolicyResponse
mkPutScalingPolicyResponse pResponseStatus_ =
  PutScalingPolicyResponse'
    { name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprsName :: Lens.Lens' PutScalingPolicyResponse (Lude.Maybe Lude.Text)
psprsName = Lens.lens (name :: PutScalingPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PutScalingPolicyResponse)
{-# DEPRECATED psprsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psprsResponseStatus :: Lens.Lens' PutScalingPolicyResponse Lude.Int
psprsResponseStatus = Lens.lens (responseStatus :: PutScalingPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutScalingPolicyResponse)
{-# DEPRECATED psprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
