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
-- Module      : Amazonka.Redshift.Types.ScheduledAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ScheduledAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.ScheduledActionState
import Amazonka.Redshift.Types.ScheduledActionType

-- | Describes a scheduled action. You can use a scheduled action to trigger
-- some Amazon Redshift API operations on a schedule. For information about
-- which API operations can be scheduled, see ScheduledActionType.
--
-- /See:/ 'newScheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { -- | The end time in UTC when the schedule is no longer active. After this
    -- time, the scheduled action does not trigger.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The IAM role to assume to run the scheduled action. This IAM role must
    -- have permission to run the Amazon Redshift API operation in the
    -- scheduled action. This IAM role must allow the Amazon Redshift scheduler
    -- (Principal scheduler.redshift.amazonaws.com) to assume permissions on
    -- your behalf. For more information about the IAM role to use with the
    -- Amazon Redshift scheduler, see
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift>
    -- in the /Amazon Redshift Cluster Management Guide/.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | List of times when the scheduled action will run.
    nextInvocations :: Prelude.Maybe [Data.ISO8601],
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
    -- | The description of the scheduled action.
    scheduledActionDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the scheduled action.
    scheduledActionName :: Prelude.Maybe Prelude.Text,
    -- | The start time in UTC when the schedule is active. Before this time, the
    -- scheduled action does not trigger.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The state of the scheduled action. For example, @DISABLED@.
    state :: Prelude.Maybe ScheduledActionState,
    -- | A JSON format string of the Amazon Redshift API operation with input
    -- parameters.
    --
    -- \"@{\\\"ResizeCluster\\\":{\\\"NodeType\\\":\\\"ds2.8xlarge\\\",\\\"ClusterIdentifier\\\":\\\"my-test-cluster\\\",\\\"NumberOfNodes\\\":3}}@\".
    targetAction :: Prelude.Maybe ScheduledActionType
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
--
-- 'nextInvocations', 'scheduledAction_nextInvocations' - List of times when the scheduled action will run.
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
-- 'scheduledActionDescription', 'scheduledAction_scheduledActionDescription' - The description of the scheduled action.
--
-- 'scheduledActionName', 'scheduledAction_scheduledActionName' - The name of the scheduled action.
--
-- 'startTime', 'scheduledAction_startTime' - The start time in UTC when the schedule is active. Before this time, the
-- scheduled action does not trigger.
--
-- 'state', 'scheduledAction_state' - The state of the scheduled action. For example, @DISABLED@.
--
-- 'targetAction', 'scheduledAction_targetAction' - A JSON format string of the Amazon Redshift API operation with input
-- parameters.
--
-- \"@{\\\"ResizeCluster\\\":{\\\"NodeType\\\":\\\"ds2.8xlarge\\\",\\\"ClusterIdentifier\\\":\\\"my-test-cluster\\\",\\\"NumberOfNodes\\\":3}}@\".
newScheduledAction ::
  ScheduledAction
newScheduledAction =
  ScheduledAction'
    { endTime = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      nextInvocations = Prelude.Nothing,
      schedule = Prelude.Nothing,
      scheduledActionDescription = Prelude.Nothing,
      scheduledActionName = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      targetAction = Prelude.Nothing
    }

-- | The end time in UTC when the schedule is no longer active. After this
-- time, the scheduled action does not trigger.
scheduledAction_endTime :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.UTCTime)
scheduledAction_endTime = Lens.lens (\ScheduledAction' {endTime} -> endTime) (\s@ScheduledAction' {} a -> s {endTime = a} :: ScheduledAction) Prelude.. Lens.mapping Data._Time

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

-- | List of times when the scheduled action will run.
scheduledAction_nextInvocations :: Lens.Lens' ScheduledAction (Prelude.Maybe [Prelude.UTCTime])
scheduledAction_nextInvocations = Lens.lens (\ScheduledAction' {nextInvocations} -> nextInvocations) (\s@ScheduledAction' {} a -> s {nextInvocations = a} :: ScheduledAction) Prelude.. Lens.mapping Lens.coerced

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

-- | The description of the scheduled action.
scheduledAction_scheduledActionDescription :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Text)
scheduledAction_scheduledActionDescription = Lens.lens (\ScheduledAction' {scheduledActionDescription} -> scheduledActionDescription) (\s@ScheduledAction' {} a -> s {scheduledActionDescription = a} :: ScheduledAction)

-- | The name of the scheduled action.
scheduledAction_scheduledActionName :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Text)
scheduledAction_scheduledActionName = Lens.lens (\ScheduledAction' {scheduledActionName} -> scheduledActionName) (\s@ScheduledAction' {} a -> s {scheduledActionName = a} :: ScheduledAction)

-- | The start time in UTC when the schedule is active. Before this time, the
-- scheduled action does not trigger.
scheduledAction_startTime :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.UTCTime)
scheduledAction_startTime = Lens.lens (\ScheduledAction' {startTime} -> startTime) (\s@ScheduledAction' {} a -> s {startTime = a} :: ScheduledAction) Prelude.. Lens.mapping Data._Time

-- | The state of the scheduled action. For example, @DISABLED@.
scheduledAction_state :: Lens.Lens' ScheduledAction (Prelude.Maybe ScheduledActionState)
scheduledAction_state = Lens.lens (\ScheduledAction' {state} -> state) (\s@ScheduledAction' {} a -> s {state = a} :: ScheduledAction)

-- | A JSON format string of the Amazon Redshift API operation with input
-- parameters.
--
-- \"@{\\\"ResizeCluster\\\":{\\\"NodeType\\\":\\\"ds2.8xlarge\\\",\\\"ClusterIdentifier\\\":\\\"my-test-cluster\\\",\\\"NumberOfNodes\\\":3}}@\".
scheduledAction_targetAction :: Lens.Lens' ScheduledAction (Prelude.Maybe ScheduledActionType)
scheduledAction_targetAction = Lens.lens (\ScheduledAction' {targetAction} -> targetAction) (\s@ScheduledAction' {} a -> s {targetAction = a} :: ScheduledAction)

instance Data.FromXML ScheduledAction where
  parseXML x =
    ScheduledAction'
      Prelude.<$> (x Data..@? "EndTime")
      Prelude.<*> (x Data..@? "IamRole")
      Prelude.<*> ( x
                      Data..@? "NextInvocations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "ScheduledActionTime")
                  )
      Prelude.<*> (x Data..@? "Schedule")
      Prelude.<*> (x Data..@? "ScheduledActionDescription")
      Prelude.<*> (x Data..@? "ScheduledActionName")
      Prelude.<*> (x Data..@? "StartTime")
      Prelude.<*> (x Data..@? "State")
      Prelude.<*> (x Data..@? "TargetAction")

instance Prelude.Hashable ScheduledAction where
  hashWithSalt _salt ScheduledAction' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` nextInvocations
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` scheduledActionDescription
      `Prelude.hashWithSalt` scheduledActionName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` targetAction

instance Prelude.NFData ScheduledAction where
  rnf ScheduledAction' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf nextInvocations
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf scheduledActionDescription
      `Prelude.seq` Prelude.rnf scheduledActionName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf targetAction
