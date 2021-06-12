{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.PutScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for a fleet. Scaling policies are
-- used to automatically scale a fleet\'s hosting capacity to meet player
-- demand. An active scaling policy instructs Amazon GameLift to track a
-- fleet metric and automatically change the fleet\'s capacity when a
-- certain threshold is reached. There are two types of scaling policies:
-- target-based and rule-based. Use a target-based policy to quickly and
-- efficiently manage fleet scaling; this option is the most commonly used.
-- Use rule-based policies when you need to exert fine-grained control over
-- auto-scaling.
--
-- Fleets can have multiple scaling policies of each type in force at the
-- same time; you can have one target-based policy, one or multiple
-- rule-based scaling policies, or both. We recommend caution, however,
-- because multiple auto-scaling policies can have unintended consequences.
--
-- You can temporarily suspend all scaling policies for a fleet by calling
-- StopFleetActions with the fleet action AUTO_SCALING. To resume scaling
-- policies, call StartFleetActions with the same fleet action. To stop
-- just one scaling policy--or to permanently remove it, you must delete
-- the policy with DeleteScalingPolicy.
--
-- Learn more about how to work with auto-scaling in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-autoscaling.html Set Up Fleet Automatic Scaling>.
--
-- __Target-based policy__
--
-- A target-based policy tracks a single metric:
-- PercentAvailableGameSessions. This metric tells us how much of a
-- fleet\'s hosting capacity is ready to host game sessions but is not
-- currently in use. This is the fleet\'s buffer; it measures the
-- additional player demand that the fleet could handle at current
-- capacity. With a target-based policy, you set your ideal buffer size and
-- leave it to Amazon GameLift to take whatever action is needed to
-- maintain that target.
--
-- For example, you might choose to maintain a 10% buffer for a fleet that
-- has the capacity to host 100 simultaneous game sessions. This policy
-- tells Amazon GameLift to take action whenever the fleet\'s available
-- capacity falls below or rises above 10 game sessions. Amazon GameLift
-- will start new instances or stop unused instances in order to return to
-- the 10% buffer.
--
-- To create or update a target-based policy, specify a fleet ID and name,
-- and set the policy type to \"TargetBased\". Specify the metric to track
-- (PercentAvailableGameSessions) and reference a TargetConfiguration
-- object with your desired buffer value. Exclude all other parameters. On
-- a successful request, the policy name is returned. The scaling policy is
-- automatically in force as soon as it\'s successfully created. If the
-- fleet\'s auto-scaling actions are temporarily suspended, the new policy
-- will be in force once the fleet actions are restarted.
--
-- __Rule-based policy__
--
-- A rule-based policy tracks specified fleet metric, sets a threshold
-- value, and specifies the type of action to initiate when triggered. With
-- a rule-based policy, you can select from several available fleet
-- metrics. Each policy specifies whether to scale up or scale down (and by
-- how much), so you need one policy for each type of action.
--
-- For example, a policy may make the following statement: \"If the
-- percentage of idle instances is greater than 20% for more than 15
-- minutes, then reduce the fleet capacity by 10%.\"
--
-- A policy\'s rule statement has the following structure:
--
-- If @[MetricName]@ is @[ComparisonOperator]@ @[Threshold]@ for
-- @[EvaluationPeriods]@ minutes, then @[ScalingAdjustmentType]@ to\/by
-- @[ScalingAdjustment]@.
--
-- To implement the example, the rule statement would look like this:
--
-- If @[PercentIdleInstances]@ is @[GreaterThanThreshold]@ @[20]@ for
-- @[15]@ minutes, then @[PercentChangeInCapacity]@ to\/by @[10]@.
--
-- To create or update a scaling policy, specify a unique combination of
-- name and fleet ID, and set the policy type to \"RuleBased\". Specify the
-- parameter values for a policy rule statement. On a successful request,
-- the policy name is returned. Scaling policies are automatically in force
-- as soon as they\'re successfully created. If the fleet\'s auto-scaling
-- actions are temporarily suspended, the new policy will be in force once
-- the fleet actions are restarted.
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
module Network.AWS.GameLift.PutScalingPolicy
  ( -- * Creating a Request
    PutScalingPolicy (..),
    newPutScalingPolicy,

    -- * Request Lenses
    putScalingPolicy_threshold,
    putScalingPolicy_targetConfiguration,
    putScalingPolicy_comparisonOperator,
    putScalingPolicy_policyType,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_evaluationPeriods,
    putScalingPolicy_scalingAdjustmentType,
    putScalingPolicy_name,
    putScalingPolicy_fleetId,
    putScalingPolicy_metricName,

    -- * Destructuring the Response
    PutScalingPolicyResponse (..),
    newPutScalingPolicyResponse,

    -- * Response Lenses
    putScalingPolicyResponse_name,
    putScalingPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newPutScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { -- | Metric value used to trigger a scaling event.
    threshold :: Core.Maybe Core.Double,
    -- | The settings for a target-based scaling policy.
    targetConfiguration :: Core.Maybe TargetConfiguration,
    -- | Comparison operator to use when measuring the metric against the
    -- threshold value.
    comparisonOperator :: Core.Maybe ComparisonOperatorType,
    -- | The type of scaling policy to create. For a target-based policy, set the
    -- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
    -- /TargetConfiguration/. For a rule-based policy set the following
    -- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
    -- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
    policyType :: Core.Maybe PolicyType,
    -- | Amount of adjustment to make, based on the scaling adjustment type.
    scalingAdjustment :: Core.Maybe Core.Int,
    -- | Length of time (in minutes) the metric must be at or beyond the
    -- threshold before a scaling event is triggered.
    evaluationPeriods :: Core.Maybe Core.Natural,
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
    --     Positive values scale up while negative values scale down; for
    --     example, a value of \"-10\" scales the fleet down by 10%.
    scalingAdjustmentType :: Core.Maybe ScalingAdjustmentType,
    -- | A descriptive label that is associated with a scaling policy. Policy
    -- names do not need to be unique. A fleet can have only one scaling policy
    -- with the same name.
    name :: Core.Text,
    -- | A unique identifier for a fleet to apply this policy to. You can use
    -- either the fleet ID or ARN value. The fleet cannot be in any of the
    -- following statuses: ERROR or DELETING.
    fleetId :: Core.Text,
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
    metricName :: MetricName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threshold', 'putScalingPolicy_threshold' - Metric value used to trigger a scaling event.
--
-- 'targetConfiguration', 'putScalingPolicy_targetConfiguration' - The settings for a target-based scaling policy.
--
-- 'comparisonOperator', 'putScalingPolicy_comparisonOperator' - Comparison operator to use when measuring the metric against the
-- threshold value.
--
-- 'policyType', 'putScalingPolicy_policyType' - The type of scaling policy to create. For a target-based policy, set the
-- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
-- /TargetConfiguration/. For a rule-based policy set the following
-- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
-- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
--
-- 'scalingAdjustment', 'putScalingPolicy_scalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
--
-- 'evaluationPeriods', 'putScalingPolicy_evaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the
-- threshold before a scaling event is triggered.
--
-- 'scalingAdjustmentType', 'putScalingPolicy_scalingAdjustmentType' - The type of adjustment to make to a fleet\'s instance count (see
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
--     Positive values scale up while negative values scale down; for
--     example, a value of \"-10\" scales the fleet down by 10%.
--
-- 'name', 'putScalingPolicy_name' - A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique. A fleet can have only one scaling policy
-- with the same name.
--
-- 'fleetId', 'putScalingPolicy_fleetId' - A unique identifier for a fleet to apply this policy to. You can use
-- either the fleet ID or ARN value. The fleet cannot be in any of the
-- following statuses: ERROR or DELETING.
--
-- 'metricName', 'putScalingPolicy_metricName' - Name of the Amazon GameLift-defined metric that is used to trigger a
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
newPutScalingPolicy ::
  -- | 'name'
  Core.Text ->
  -- | 'fleetId'
  Core.Text ->
  -- | 'metricName'
  MetricName ->
  PutScalingPolicy
newPutScalingPolicy pName_ pFleetId_ pMetricName_ =
  PutScalingPolicy'
    { threshold = Core.Nothing,
      targetConfiguration = Core.Nothing,
      comparisonOperator = Core.Nothing,
      policyType = Core.Nothing,
      scalingAdjustment = Core.Nothing,
      evaluationPeriods = Core.Nothing,
      scalingAdjustmentType = Core.Nothing,
      name = pName_,
      fleetId = pFleetId_,
      metricName = pMetricName_
    }

-- | Metric value used to trigger a scaling event.
putScalingPolicy_threshold :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Double)
putScalingPolicy_threshold = Lens.lens (\PutScalingPolicy' {threshold} -> threshold) (\s@PutScalingPolicy' {} a -> s {threshold = a} :: PutScalingPolicy)

-- | The settings for a target-based scaling policy.
putScalingPolicy_targetConfiguration :: Lens.Lens' PutScalingPolicy (Core.Maybe TargetConfiguration)
putScalingPolicy_targetConfiguration = Lens.lens (\PutScalingPolicy' {targetConfiguration} -> targetConfiguration) (\s@PutScalingPolicy' {} a -> s {targetConfiguration = a} :: PutScalingPolicy)

-- | Comparison operator to use when measuring the metric against the
-- threshold value.
putScalingPolicy_comparisonOperator :: Lens.Lens' PutScalingPolicy (Core.Maybe ComparisonOperatorType)
putScalingPolicy_comparisonOperator = Lens.lens (\PutScalingPolicy' {comparisonOperator} -> comparisonOperator) (\s@PutScalingPolicy' {} a -> s {comparisonOperator = a} :: PutScalingPolicy)

-- | The type of scaling policy to create. For a target-based policy, set the
-- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
-- /TargetConfiguration/. For a rule-based policy set the following
-- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
-- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
putScalingPolicy_policyType :: Lens.Lens' PutScalingPolicy (Core.Maybe PolicyType)
putScalingPolicy_policyType = Lens.lens (\PutScalingPolicy' {policyType} -> policyType) (\s@PutScalingPolicy' {} a -> s {policyType = a} :: PutScalingPolicy)

-- | Amount of adjustment to make, based on the scaling adjustment type.
putScalingPolicy_scalingAdjustment :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Int)
putScalingPolicy_scalingAdjustment = Lens.lens (\PutScalingPolicy' {scalingAdjustment} -> scalingAdjustment) (\s@PutScalingPolicy' {} a -> s {scalingAdjustment = a} :: PutScalingPolicy)

-- | Length of time (in minutes) the metric must be at or beyond the
-- threshold before a scaling event is triggered.
putScalingPolicy_evaluationPeriods :: Lens.Lens' PutScalingPolicy (Core.Maybe Core.Natural)
putScalingPolicy_evaluationPeriods = Lens.lens (\PutScalingPolicy' {evaluationPeriods} -> evaluationPeriods) (\s@PutScalingPolicy' {} a -> s {evaluationPeriods = a} :: PutScalingPolicy)

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
--     Positive values scale up while negative values scale down; for
--     example, a value of \"-10\" scales the fleet down by 10%.
putScalingPolicy_scalingAdjustmentType :: Lens.Lens' PutScalingPolicy (Core.Maybe ScalingAdjustmentType)
putScalingPolicy_scalingAdjustmentType = Lens.lens (\PutScalingPolicy' {scalingAdjustmentType} -> scalingAdjustmentType) (\s@PutScalingPolicy' {} a -> s {scalingAdjustmentType = a} :: PutScalingPolicy)

-- | A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique. A fleet can have only one scaling policy
-- with the same name.
putScalingPolicy_name :: Lens.Lens' PutScalingPolicy Core.Text
putScalingPolicy_name = Lens.lens (\PutScalingPolicy' {name} -> name) (\s@PutScalingPolicy' {} a -> s {name = a} :: PutScalingPolicy)

-- | A unique identifier for a fleet to apply this policy to. You can use
-- either the fleet ID or ARN value. The fleet cannot be in any of the
-- following statuses: ERROR or DELETING.
putScalingPolicy_fleetId :: Lens.Lens' PutScalingPolicy Core.Text
putScalingPolicy_fleetId = Lens.lens (\PutScalingPolicy' {fleetId} -> fleetId) (\s@PutScalingPolicy' {} a -> s {fleetId = a} :: PutScalingPolicy)

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
putScalingPolicy_metricName :: Lens.Lens' PutScalingPolicy MetricName
putScalingPolicy_metricName = Lens.lens (\PutScalingPolicy' {metricName} -> metricName) (\s@PutScalingPolicy' {} a -> s {metricName = a} :: PutScalingPolicy)

instance Core.AWSRequest PutScalingPolicy where
  type
    AWSResponse PutScalingPolicy =
      PutScalingPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutScalingPolicyResponse'
            Core.<$> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutScalingPolicy

instance Core.NFData PutScalingPolicy

instance Core.ToHeaders PutScalingPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.PutScalingPolicy" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutScalingPolicy where
  toJSON PutScalingPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Threshold" Core..=) Core.<$> threshold,
            ("TargetConfiguration" Core..=)
              Core.<$> targetConfiguration,
            ("ComparisonOperator" Core..=)
              Core.<$> comparisonOperator,
            ("PolicyType" Core..=) Core.<$> policyType,
            ("ScalingAdjustment" Core..=)
              Core.<$> scalingAdjustment,
            ("EvaluationPeriods" Core..=)
              Core.<$> evaluationPeriods,
            ("ScalingAdjustmentType" Core..=)
              Core.<$> scalingAdjustmentType,
            Core.Just ("Name" Core..= name),
            Core.Just ("FleetId" Core..= fleetId),
            Core.Just ("MetricName" Core..= metricName)
          ]
      )

instance Core.ToPath PutScalingPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutScalingPolicy where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newPutScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { -- | A descriptive label that is associated with a scaling policy. Policy
    -- names do not need to be unique.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'putScalingPolicyResponse_name' - A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique.
--
-- 'httpStatus', 'putScalingPolicyResponse_httpStatus' - The response's http status code.
newPutScalingPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutScalingPolicyResponse
newPutScalingPolicyResponse pHttpStatus_ =
  PutScalingPolicyResponse'
    { name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A descriptive label that is associated with a scaling policy. Policy
-- names do not need to be unique.
putScalingPolicyResponse_name :: Lens.Lens' PutScalingPolicyResponse (Core.Maybe Core.Text)
putScalingPolicyResponse_name = Lens.lens (\PutScalingPolicyResponse' {name} -> name) (\s@PutScalingPolicyResponse' {} a -> s {name = a} :: PutScalingPolicyResponse)

-- | The response's http status code.
putScalingPolicyResponse_httpStatus :: Lens.Lens' PutScalingPolicyResponse Core.Int
putScalingPolicyResponse_httpStatus = Lens.lens (\PutScalingPolicyResponse' {httpStatus} -> httpStatus) (\s@PutScalingPolicyResponse' {} a -> s {httpStatus = a} :: PutScalingPolicyResponse)

instance Core.NFData PutScalingPolicyResponse
