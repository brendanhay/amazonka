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
-- Module      : Network.AWS.Redshift.Types.ScheduledAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduledActionState
import Network.AWS.Redshift.Types.ScheduledActionType

-- | Describes a scheduled action. You can use a scheduled action to trigger
-- some Amazon Redshift API operations on a schedule. For information about
-- which API operations can be scheduled, see ScheduledActionType.
--
-- /See:/ 'newScheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { -- | The state of the scheduled action. For example, @DISABLED@.
    state :: Prelude.Maybe ScheduledActionState,
    -- | A JSON format string of the Amazon Redshift API operation with input
    -- parameters.
    --
    -- \"@{\\\"ResizeCluster\\\":{\\\"NodeType\\\":\\\"ds2.8xlarge\\\",\\\"ClusterIdentifier\\\":\\\"my-test-cluster\\\",\\\"NumberOfNodes\\\":3}}@\".
    targetAction :: Prelude.Maybe ScheduledActionType,
    -- | The start time in UTC when the schedule is active. Before this time, the
    -- scheduled action does not trigger.
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | The schedule for a one-time (at format) or recurring (cron format)
    -- scheduled action. Schedule invocations must be separated by at least one
    -- hour.
    --
    -- Format of at expressions is \"@at(yyyy-mm-ddThh:mm:ss)@\". For example,
    -- \"@at(2016-03-04T17:27:00)@\".
    --
    -- Format of cron expressions is
    -- \"@cron(Minutes Hours Day-of-month Month Day-of-week Year)@\". For
    -- example, \"@cron(0 10 ? * MON *)@\". For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions>
    -- in the /Amazon CloudWatch Events User Guide/.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled action.
    scheduledActionName :: Prelude.Maybe Prelude.Text,
    -- | The description of the scheduled action.
    scheduledActionDescription :: Prelude.Maybe Prelude.Text,
    -- | List of times when the scheduled action will run.
    nextInvocations :: Prelude.Maybe [Core.ISO8601],
    -- | The end time in UTC when the schedule is no longer active. After this
    -- time, the scheduled action does not trigger.
    endTime :: Prelude.Maybe Core.ISO8601,
    -- | The IAM role to assume to run the scheduled action. This IAM role must
    -- have permission to run the Amazon Redshift API operation in the
    -- scheduled action. This IAM role must allow the Amazon Redshift scheduler
    -- (Principal scheduler.redshift.amazonaws.com) to assume permissions on
    -- your behalf. For more information about the IAM role to use with the
    -- Amazon Redshift scheduler, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift>
    -- in the /Amazon Redshift Cluster Management Guide/.
    iamRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'scheduledAction_state' - The state of the scheduled action. For example, @DISABLED@.
--
-- 'targetAction', 'scheduledAction_targetAction' - A JSON format string of the Amazon Redshift API operation with input
-- parameters.
--
-- \"@{\\\"ResizeCluster\\\":{\\\"NodeType\\\":\\\"ds2.8xlarge\\\",\\\"ClusterIdentifier\\\":\\\"my-test-cluster\\\",\\\"NumberOfNodes\\\":3}}@\".
--
-- 'startTime', 'scheduledAction_startTime' - The start time in UTC when the schedule is active. Before this time, the
-- scheduled action does not trigger.
--
-- 'schedule', 'scheduledAction_schedule' - The schedule for a one-time (at format) or recurring (cron format)
-- scheduled action. Schedule invocations must be separated by at least one
-- hour.
--
-- Format of at expressions is \"@at(yyyy-mm-ddThh:mm:ss)@\". For example,
-- \"@at(2016-03-04T17:27:00)@\".
--
-- Format of cron expressions is
-- \"@cron(Minutes Hours Day-of-month Month Day-of-week Year)@\". For
-- example, \"@cron(0 10 ? * MON *)@\". For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions>
-- in the /Amazon CloudWatch Events User Guide/.
--
-- 'scheduledActionName', 'scheduledAction_scheduledActionName' - The name of the scheduled action.
--
-- 'scheduledActionDescription', 'scheduledAction_scheduledActionDescription' - The description of the scheduled action.
--
-- 'nextInvocations', 'scheduledAction_nextInvocations' - List of times when the scheduled action will run.
--
-- 'endTime', 'scheduledAction_endTime' - The end time in UTC when the schedule is no longer active. After this
-- time, the scheduled action does not trigger.
--
-- 'iamRole', 'scheduledAction_iamRole' - The IAM role to assume to run the scheduled action. This IAM role must
-- have permission to run the Amazon Redshift API operation in the
-- scheduled action. This IAM role must allow the Amazon Redshift scheduler
-- (Principal scheduler.redshift.amazonaws.com) to assume permissions on
-- your behalf. For more information about the IAM role to use with the
-- Amazon Redshift scheduler, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
newScheduledAction ::
  ScheduledAction
newScheduledAction =
  ScheduledAction'
    { state = Prelude.Nothing,
      targetAction = Prelude.Nothing,
      startTime = Prelude.Nothing,
      schedule = Prelude.Nothing,
      scheduledActionName = Prelude.Nothing,
      scheduledActionDescription = Prelude.Nothing,
      nextInvocations = Prelude.Nothing,
      endTime = Prelude.Nothing,
      iamRole = Prelude.Nothing
    }

-- | The state of the scheduled action. For example, @DISABLED@.
scheduledAction_state :: Lens.Lens' ScheduledAction (Prelude.Maybe ScheduledActionState)
scheduledAction_state = Lens.lens (\ScheduledAction' {state} -> state) (\s@ScheduledAction' {} a -> s {state = a} :: ScheduledAction)

-- | A JSON format string of the Amazon Redshift API operation with input
-- parameters.
--
-- \"@{\\\"ResizeCluster\\\":{\\\"NodeType\\\":\\\"ds2.8xlarge\\\",\\\"ClusterIdentifier\\\":\\\"my-test-cluster\\\",\\\"NumberOfNodes\\\":3}}@\".
scheduledAction_targetAction :: Lens.Lens' ScheduledAction (Prelude.Maybe ScheduledActionType)
scheduledAction_targetAction = Lens.lens (\ScheduledAction' {targetAction} -> targetAction) (\s@ScheduledAction' {} a -> s {targetAction = a} :: ScheduledAction)

-- | The start time in UTC when the schedule is active. Before this time, the
-- scheduled action does not trigger.
scheduledAction_startTime :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.UTCTime)
scheduledAction_startTime = Lens.lens (\ScheduledAction' {startTime} -> startTime) (\s@ScheduledAction' {} a -> s {startTime = a} :: ScheduledAction) Prelude.. Lens.mapping Core._Time

-- | The schedule for a one-time (at format) or recurring (cron format)
-- scheduled action. Schedule invocations must be separated by at least one
-- hour.
--
-- Format of at expressions is \"@at(yyyy-mm-ddThh:mm:ss)@\". For example,
-- \"@at(2016-03-04T17:27:00)@\".
--
-- Format of cron expressions is
-- \"@cron(Minutes Hours Day-of-month Month Day-of-week Year)@\". For
-- example, \"@cron(0 10 ? * MON *)@\". For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions>
-- in the /Amazon CloudWatch Events User Guide/.
scheduledAction_schedule :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Text)
scheduledAction_schedule = Lens.lens (\ScheduledAction' {schedule} -> schedule) (\s@ScheduledAction' {} a -> s {schedule = a} :: ScheduledAction)

-- | The name of the scheduled action.
scheduledAction_scheduledActionName :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Text)
scheduledAction_scheduledActionName = Lens.lens (\ScheduledAction' {scheduledActionName} -> scheduledActionName) (\s@ScheduledAction' {} a -> s {scheduledActionName = a} :: ScheduledAction)

-- | The description of the scheduled action.
scheduledAction_scheduledActionDescription :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Text)
scheduledAction_scheduledActionDescription = Lens.lens (\ScheduledAction' {scheduledActionDescription} -> scheduledActionDescription) (\s@ScheduledAction' {} a -> s {scheduledActionDescription = a} :: ScheduledAction)

-- | List of times when the scheduled action will run.
scheduledAction_nextInvocations :: Lens.Lens' ScheduledAction (Prelude.Maybe [Prelude.UTCTime])
scheduledAction_nextInvocations = Lens.lens (\ScheduledAction' {nextInvocations} -> nextInvocations) (\s@ScheduledAction' {} a -> s {nextInvocations = a} :: ScheduledAction) Prelude.. Lens.mapping Lens.coerced

-- | The end time in UTC when the schedule is no longer active. After this
-- time, the scheduled action does not trigger.
scheduledAction_endTime :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.UTCTime)
scheduledAction_endTime = Lens.lens (\ScheduledAction' {endTime} -> endTime) (\s@ScheduledAction' {} a -> s {endTime = a} :: ScheduledAction) Prelude.. Lens.mapping Core._Time

-- | The IAM role to assume to run the scheduled action. This IAM role must
-- have permission to run the Amazon Redshift API operation in the
-- scheduled action. This IAM role must allow the Amazon Redshift scheduler
-- (Principal scheduler.redshift.amazonaws.com) to assume permissions on
-- your behalf. For more information about the IAM role to use with the
-- Amazon Redshift scheduler, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift>
-- in the /Amazon Redshift Cluster Management Guide/.
scheduledAction_iamRole :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Text)
scheduledAction_iamRole = Lens.lens (\ScheduledAction' {iamRole} -> iamRole) (\s@ScheduledAction' {} a -> s {iamRole = a} :: ScheduledAction)

instance Core.FromXML ScheduledAction where
  parseXML x =
    ScheduledAction'
      Prelude.<$> (x Core..@? "State")
      Prelude.<*> (x Core..@? "TargetAction")
      Prelude.<*> (x Core..@? "StartTime")
      Prelude.<*> (x Core..@? "Schedule")
      Prelude.<*> (x Core..@? "ScheduledActionName")
      Prelude.<*> (x Core..@? "ScheduledActionDescription")
      Prelude.<*> ( x Core..@? "NextInvocations" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "ScheduledActionTime")
                  )
      Prelude.<*> (x Core..@? "EndTime")
      Prelude.<*> (x Core..@? "IamRole")

instance Prelude.Hashable ScheduledAction

instance Prelude.NFData ScheduledAction
