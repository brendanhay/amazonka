{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled action. A scheduled action contains a schedule and an Amazon Redshift API action. For example, you can create a schedule of when to run the @ResizeCluster@ API operation.
module Network.AWS.Redshift.CreateScheduledAction
  ( -- * Creating a Request
    createScheduledAction,
    CreateScheduledAction,

    -- * Request Lenses
    csaStartTime,
    csaScheduledActionDescription,
    csaEnable,
    csaEndTime,
    csaScheduledActionName,
    csaTargetAction,
    csaSchedule,
    csaIAMRole,

    -- * Destructuring the Response
    scheduledAction,
    ScheduledAction,

    -- * Response Lenses
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createScheduledAction' smart constructor.
data CreateScheduledAction = CreateScheduledAction'
  { _csaStartTime ::
      !(Maybe ISO8601),
    _csaScheduledActionDescription :: !(Maybe Text),
    _csaEnable :: !(Maybe Bool),
    _csaEndTime :: !(Maybe ISO8601),
    _csaScheduledActionName :: !Text,
    _csaTargetAction :: !ScheduledActionType,
    _csaSchedule :: !Text,
    _csaIAMRole :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csaStartTime' - The start time in UTC of the scheduled action. Before this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
--
-- * 'csaScheduledActionDescription' - The description of the scheduled action.
--
-- * 'csaEnable' - If true, the schedule is enabled. If false, the scheduled action does not trigger. For more information about @state@ of the scheduled action, see 'ScheduledAction' .
--
-- * 'csaEndTime' - The end time in UTC of the scheduled action. After this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
--
-- * 'csaScheduledActionName' - The name of the scheduled action. The name must be unique within an account. For more information about this parameter, see 'ScheduledAction' .
--
-- * 'csaTargetAction' - A JSON format string of the Amazon Redshift API operation with input parameters. For more information about this parameter, see 'ScheduledAction' .
--
-- * 'csaSchedule' - The schedule in @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
--
-- * 'csaIAMRole' - The IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
createScheduledAction ::
  -- | 'csaScheduledActionName'
  Text ->
  -- | 'csaTargetAction'
  ScheduledActionType ->
  -- | 'csaSchedule'
  Text ->
  -- | 'csaIAMRole'
  Text ->
  CreateScheduledAction
createScheduledAction
  pScheduledActionName_
  pTargetAction_
  pSchedule_
  pIAMRole_ =
    CreateScheduledAction'
      { _csaStartTime = Nothing,
        _csaScheduledActionDescription = Nothing,
        _csaEnable = Nothing,
        _csaEndTime = Nothing,
        _csaScheduledActionName = pScheduledActionName_,
        _csaTargetAction = pTargetAction_,
        _csaSchedule = pSchedule_,
        _csaIAMRole = pIAMRole_
      }

-- | The start time in UTC of the scheduled action. Before this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
csaStartTime :: Lens' CreateScheduledAction (Maybe UTCTime)
csaStartTime = lens _csaStartTime (\s a -> s {_csaStartTime = a}) . mapping _Time

-- | The description of the scheduled action.
csaScheduledActionDescription :: Lens' CreateScheduledAction (Maybe Text)
csaScheduledActionDescription = lens _csaScheduledActionDescription (\s a -> s {_csaScheduledActionDescription = a})

-- | If true, the schedule is enabled. If false, the scheduled action does not trigger. For more information about @state@ of the scheduled action, see 'ScheduledAction' .
csaEnable :: Lens' CreateScheduledAction (Maybe Bool)
csaEnable = lens _csaEnable (\s a -> s {_csaEnable = a})

-- | The end time in UTC of the scheduled action. After this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
csaEndTime :: Lens' CreateScheduledAction (Maybe UTCTime)
csaEndTime = lens _csaEndTime (\s a -> s {_csaEndTime = a}) . mapping _Time

-- | The name of the scheduled action. The name must be unique within an account. For more information about this parameter, see 'ScheduledAction' .
csaScheduledActionName :: Lens' CreateScheduledAction Text
csaScheduledActionName = lens _csaScheduledActionName (\s a -> s {_csaScheduledActionName = a})

-- | A JSON format string of the Amazon Redshift API operation with input parameters. For more information about this parameter, see 'ScheduledAction' .
csaTargetAction :: Lens' CreateScheduledAction ScheduledActionType
csaTargetAction = lens _csaTargetAction (\s a -> s {_csaTargetAction = a})

-- | The schedule in @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
csaSchedule :: Lens' CreateScheduledAction Text
csaSchedule = lens _csaSchedule (\s a -> s {_csaSchedule = a})

-- | The IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
csaIAMRole :: Lens' CreateScheduledAction Text
csaIAMRole = lens _csaIAMRole (\s a -> s {_csaIAMRole = a})

instance AWSRequest CreateScheduledAction where
  type Rs CreateScheduledAction = ScheduledAction
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "CreateScheduledActionResult"
      (\s h x -> parseXML x)

instance Hashable CreateScheduledAction

instance NFData CreateScheduledAction

instance ToHeaders CreateScheduledAction where
  toHeaders = const mempty

instance ToPath CreateScheduledAction where
  toPath = const "/"

instance ToQuery CreateScheduledAction where
  toQuery CreateScheduledAction' {..} =
    mconcat
      [ "Action" =: ("CreateScheduledAction" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "StartTime" =: _csaStartTime,
        "ScheduledActionDescription" =: _csaScheduledActionDescription,
        "Enable" =: _csaEnable,
        "EndTime" =: _csaEndTime,
        "ScheduledActionName" =: _csaScheduledActionName,
        "TargetAction" =: _csaTargetAction,
        "Schedule" =: _csaSchedule,
        "IamRole" =: _csaIAMRole
      ]
