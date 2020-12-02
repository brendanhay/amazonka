{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledAction where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduledActionState
import Network.AWS.Redshift.Types.ScheduledActionType

-- | Describes a scheduled action. You can use a scheduled action to trigger some Amazon Redshift API operations on a schedule. For information about which API operations can be scheduled, see 'ScheduledActionType' .
--
--
--
-- /See:/ 'scheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { _saState ::
      !(Maybe ScheduledActionState),
    _saTargetAction :: !(Maybe ScheduledActionType),
    _saStartTime :: !(Maybe ISO8601),
    _saSchedule :: !(Maybe Text),
    _saScheduledActionName :: !(Maybe Text),
    _saScheduledActionDescription :: !(Maybe Text),
    _saNextInvocations :: !(Maybe [ISO8601]),
    _saEndTime :: !(Maybe ISO8601),
    _saIAMRole :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saState' - The state of the scheduled action. For example, @DISABLED@ .
--
-- * 'saTargetAction' - A JSON format string of the Amazon Redshift API operation with input parameters.  "@{\"ResizeCluster\":{\"NodeType\":\"ds2.8xlarge\",\"ClusterIdentifier\":\"my-test-cluster\",\"NumberOfNodes\":3}}@ ".
--
-- * 'saStartTime' - The start time in UTC when the schedule is active. Before this time, the scheduled action does not trigger.
--
-- * 'saSchedule' - The schedule for a one-time (at format) or recurring (cron format) scheduled action. Schedule invocations must be separated by at least one hour. Format of at expressions is "@at(yyyy-mm-ddThh:mm:ss)@ ". For example, "@at(2016-03-04T17:27:00)@ ". Format of cron expressions is "@cron(Minutes Hours Day-of-month Month Day-of-week Year)@ ". For example, "@cron(0 10 ? * MON *)@ ". For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
--
-- * 'saScheduledActionName' - The name of the scheduled action.
--
-- * 'saScheduledActionDescription' - The description of the scheduled action.
--
-- * 'saNextInvocations' - List of times when the scheduled action will run.
--
-- * 'saEndTime' - The end time in UTC when the schedule is no longer active. After this time, the scheduled action does not trigger.
--
-- * 'saIAMRole' - The IAM role to assume to run the scheduled action. This IAM role must have permission to run the Amazon Redshift API operation in the scheduled action. This IAM role must allow the Amazon Redshift scheduler (Principal scheduler.redshift.amazonaws.com) to assume permissions on your behalf. For more information about the IAM role to use with the Amazon Redshift scheduler, see <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
scheduledAction ::
  ScheduledAction
scheduledAction =
  ScheduledAction'
    { _saState = Nothing,
      _saTargetAction = Nothing,
      _saStartTime = Nothing,
      _saSchedule = Nothing,
      _saScheduledActionName = Nothing,
      _saScheduledActionDescription = Nothing,
      _saNextInvocations = Nothing,
      _saEndTime = Nothing,
      _saIAMRole = Nothing
    }

-- | The state of the scheduled action. For example, @DISABLED@ .
saState :: Lens' ScheduledAction (Maybe ScheduledActionState)
saState = lens _saState (\s a -> s {_saState = a})

-- | A JSON format string of the Amazon Redshift API operation with input parameters.  "@{\"ResizeCluster\":{\"NodeType\":\"ds2.8xlarge\",\"ClusterIdentifier\":\"my-test-cluster\",\"NumberOfNodes\":3}}@ ".
saTargetAction :: Lens' ScheduledAction (Maybe ScheduledActionType)
saTargetAction = lens _saTargetAction (\s a -> s {_saTargetAction = a})

-- | The start time in UTC when the schedule is active. Before this time, the scheduled action does not trigger.
saStartTime :: Lens' ScheduledAction (Maybe UTCTime)
saStartTime = lens _saStartTime (\s a -> s {_saStartTime = a}) . mapping _Time

-- | The schedule for a one-time (at format) or recurring (cron format) scheduled action. Schedule invocations must be separated by at least one hour. Format of at expressions is "@at(yyyy-mm-ddThh:mm:ss)@ ". For example, "@at(2016-03-04T17:27:00)@ ". Format of cron expressions is "@cron(Minutes Hours Day-of-month Month Day-of-week Year)@ ". For example, "@cron(0 10 ? * MON *)@ ". For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
saSchedule :: Lens' ScheduledAction (Maybe Text)
saSchedule = lens _saSchedule (\s a -> s {_saSchedule = a})

-- | The name of the scheduled action.
saScheduledActionName :: Lens' ScheduledAction (Maybe Text)
saScheduledActionName = lens _saScheduledActionName (\s a -> s {_saScheduledActionName = a})

-- | The description of the scheduled action.
saScheduledActionDescription :: Lens' ScheduledAction (Maybe Text)
saScheduledActionDescription = lens _saScheduledActionDescription (\s a -> s {_saScheduledActionDescription = a})

-- | List of times when the scheduled action will run.
saNextInvocations :: Lens' ScheduledAction [UTCTime]
saNextInvocations = lens _saNextInvocations (\s a -> s {_saNextInvocations = a}) . _Default . _Coerce

-- | The end time in UTC when the schedule is no longer active. After this time, the scheduled action does not trigger.
saEndTime :: Lens' ScheduledAction (Maybe UTCTime)
saEndTime = lens _saEndTime (\s a -> s {_saEndTime = a}) . mapping _Time

-- | The IAM role to assume to run the scheduled action. This IAM role must have permission to run the Amazon Redshift API operation in the scheduled action. This IAM role must allow the Amazon Redshift scheduler (Principal scheduler.redshift.amazonaws.com) to assume permissions on your behalf. For more information about the IAM role to use with the Amazon Redshift scheduler, see <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
saIAMRole :: Lens' ScheduledAction (Maybe Text)
saIAMRole = lens _saIAMRole (\s a -> s {_saIAMRole = a})

instance FromXML ScheduledAction where
  parseXML x =
    ScheduledAction'
      <$> (x .@? "State")
      <*> (x .@? "TargetAction")
      <*> (x .@? "StartTime")
      <*> (x .@? "Schedule")
      <*> (x .@? "ScheduledActionName")
      <*> (x .@? "ScheduledActionDescription")
      <*> ( x .@? "NextInvocations" .!@ mempty
              >>= may (parseXMLList "ScheduledActionTime")
          )
      <*> (x .@? "EndTime")
      <*> (x .@? "IamRole")

instance Hashable ScheduledAction

instance NFData ScheduledAction
