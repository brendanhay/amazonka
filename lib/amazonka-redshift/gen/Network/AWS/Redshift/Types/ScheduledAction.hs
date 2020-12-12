{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledAction
  ( ScheduledAction (..),

    -- * Smart constructor
    mkScheduledAction,

    -- * Lenses
    saState,
    saTargetAction,
    saStartTime,
    saSchedule,
    saScheduledActionName,
    saScheduledActionDescription,
    saNextInvocations,
    saEndTime,
    saIAMRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduledActionState
import Network.AWS.Redshift.Types.ScheduledActionType

-- | Describes a scheduled action. You can use a scheduled action to trigger some Amazon Redshift API operations on a schedule. For information about which API operations can be scheduled, see 'ScheduledActionType' .
--
-- /See:/ 'mkScheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { state ::
      Lude.Maybe ScheduledActionState,
    targetAction :: Lude.Maybe ScheduledActionType,
    startTime :: Lude.Maybe Lude.DateTime,
    schedule :: Lude.Maybe Lude.Text,
    scheduledActionName :: Lude.Maybe Lude.Text,
    scheduledActionDescription :: Lude.Maybe Lude.Text,
    nextInvocations :: Lude.Maybe [Lude.DateTime],
    endTime :: Lude.Maybe Lude.DateTime,
    iamRole :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledAction' with the minimum fields required to make a request.
--
-- * 'endTime' - The end time in UTC when the schedule is no longer active. After this time, the scheduled action does not trigger.
-- * 'iamRole' - The IAM role to assume to run the scheduled action. This IAM role must have permission to run the Amazon Redshift API operation in the scheduled action. This IAM role must allow the Amazon Redshift scheduler (Principal scheduler.redshift.amazonaws.com) to assume permissions on your behalf. For more information about the IAM role to use with the Amazon Redshift scheduler, see <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
-- * 'nextInvocations' - List of times when the scheduled action will run.
-- * 'schedule' - The schedule for a one-time (at format) or recurring (cron format) scheduled action. Schedule invocations must be separated by at least one hour.
--
-- Format of at expressions is "@at(yyyy-mm-ddThh:mm:ss)@ ". For example, "@at(2016-03-04T17:27:00)@ ".
-- Format of cron expressions is "@cron(Minutes Hours Day-of-month Month Day-of-week Year)@ ". For example, "@cron(0 10 ? * MON *)@ ". For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
-- * 'scheduledActionDescription' - The description of the scheduled action.
-- * 'scheduledActionName' - The name of the scheduled action.
-- * 'startTime' - The start time in UTC when the schedule is active. Before this time, the scheduled action does not trigger.
-- * 'state' - The state of the scheduled action. For example, @DISABLED@ .
-- * 'targetAction' - A JSON format string of the Amazon Redshift API operation with input parameters.
--
-- "@{\"ResizeCluster\":{\"NodeType\":\"ds2.8xlarge\",\"ClusterIdentifier\":\"my-test-cluster\",\"NumberOfNodes\":3}}@ ".
mkScheduledAction ::
  ScheduledAction
mkScheduledAction =
  ScheduledAction'
    { state = Lude.Nothing,
      targetAction = Lude.Nothing,
      startTime = Lude.Nothing,
      schedule = Lude.Nothing,
      scheduledActionName = Lude.Nothing,
      scheduledActionDescription = Lude.Nothing,
      nextInvocations = Lude.Nothing,
      endTime = Lude.Nothing,
      iamRole = Lude.Nothing
    }

-- | The state of the scheduled action. For example, @DISABLED@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saState :: Lens.Lens' ScheduledAction (Lude.Maybe ScheduledActionState)
saState = Lens.lens (state :: ScheduledAction -> Lude.Maybe ScheduledActionState) (\s a -> s {state = a} :: ScheduledAction)
{-# DEPRECATED saState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A JSON format string of the Amazon Redshift API operation with input parameters.
--
-- "@{\"ResizeCluster\":{\"NodeType\":\"ds2.8xlarge\",\"ClusterIdentifier\":\"my-test-cluster\",\"NumberOfNodes\":3}}@ ".
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saTargetAction :: Lens.Lens' ScheduledAction (Lude.Maybe ScheduledActionType)
saTargetAction = Lens.lens (targetAction :: ScheduledAction -> Lude.Maybe ScheduledActionType) (\s a -> s {targetAction = a} :: ScheduledAction)
{-# DEPRECATED saTargetAction "Use generic-lens or generic-optics with 'targetAction' instead." #-}

-- | The start time in UTC when the schedule is active. Before this time, the scheduled action does not trigger.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saStartTime :: Lens.Lens' ScheduledAction (Lude.Maybe Lude.DateTime)
saStartTime = Lens.lens (startTime :: ScheduledAction -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: ScheduledAction)
{-# DEPRECATED saStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The schedule for a one-time (at format) or recurring (cron format) scheduled action. Schedule invocations must be separated by at least one hour.
--
-- Format of at expressions is "@at(yyyy-mm-ddThh:mm:ss)@ ". For example, "@at(2016-03-04T17:27:00)@ ".
-- Format of cron expressions is "@cron(Minutes Hours Day-of-month Month Day-of-week Year)@ ". For example, "@cron(0 10 ? * MON *)@ ". For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saSchedule :: Lens.Lens' ScheduledAction (Lude.Maybe Lude.Text)
saSchedule = Lens.lens (schedule :: ScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: ScheduledAction)
{-# DEPRECATED saSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The name of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduledActionName :: Lens.Lens' ScheduledAction (Lude.Maybe Lude.Text)
saScheduledActionName = Lens.lens (scheduledActionName :: ScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {scheduledActionName = a} :: ScheduledAction)
{-# DEPRECATED saScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The description of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduledActionDescription :: Lens.Lens' ScheduledAction (Lude.Maybe Lude.Text)
saScheduledActionDescription = Lens.lens (scheduledActionDescription :: ScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {scheduledActionDescription = a} :: ScheduledAction)
{-# DEPRECATED saScheduledActionDescription "Use generic-lens or generic-optics with 'scheduledActionDescription' instead." #-}

-- | List of times when the scheduled action will run.
--
-- /Note:/ Consider using 'nextInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saNextInvocations :: Lens.Lens' ScheduledAction (Lude.Maybe [Lude.DateTime])
saNextInvocations = Lens.lens (nextInvocations :: ScheduledAction -> Lude.Maybe [Lude.DateTime]) (\s a -> s {nextInvocations = a} :: ScheduledAction)
{-# DEPRECATED saNextInvocations "Use generic-lens or generic-optics with 'nextInvocations' instead." #-}

-- | The end time in UTC when the schedule is no longer active. After this time, the scheduled action does not trigger.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saEndTime :: Lens.Lens' ScheduledAction (Lude.Maybe Lude.DateTime)
saEndTime = Lens.lens (endTime :: ScheduledAction -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: ScheduledAction)
{-# DEPRECATED saEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The IAM role to assume to run the scheduled action. This IAM role must have permission to run the Amazon Redshift API operation in the scheduled action. This IAM role must allow the Amazon Redshift scheduler (Principal scheduler.redshift.amazonaws.com) to assume permissions on your behalf. For more information about the IAM role to use with the Amazon Redshift scheduler, see <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saIAMRole :: Lens.Lens' ScheduledAction (Lude.Maybe Lude.Text)
saIAMRole = Lens.lens (iamRole :: ScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {iamRole = a} :: ScheduledAction)
{-# DEPRECATED saIAMRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

instance Lude.FromXML ScheduledAction where
  parseXML x =
    ScheduledAction'
      Lude.<$> (x Lude..@? "State")
      Lude.<*> (x Lude..@? "TargetAction")
      Lude.<*> (x Lude..@? "StartTime")
      Lude.<*> (x Lude..@? "Schedule")
      Lude.<*> (x Lude..@? "ScheduledActionName")
      Lude.<*> (x Lude..@? "ScheduledActionDescription")
      Lude.<*> ( x Lude..@? "NextInvocations" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ScheduledActionTime")
               )
      Lude.<*> (x Lude..@? "EndTime")
      Lude.<*> (x Lude..@? "IamRole")
