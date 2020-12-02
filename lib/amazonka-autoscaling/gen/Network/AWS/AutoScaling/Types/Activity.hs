{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Activity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Activity where

import Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes scaling activity, which is a long-running process that represents a change to your Auto Scaling group, such as changing its size or replacing an instance.
--
--
--
-- /See:/ 'activity' smart constructor.
data Activity = Activity'
  { _aProgress :: !(Maybe Int),
    _aStatusMessage :: !(Maybe Text),
    _aEndTime :: !(Maybe ISO8601),
    _aDetails :: !(Maybe Text),
    _aDescription :: !(Maybe Text),
    _aActivityId :: !Text,
    _aAutoScalingGroupName :: !Text,
    _aCause :: !Text,
    _aStartTime :: !ISO8601,
    _aStatusCode :: !ScalingActivityStatusCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Activity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aProgress' - A value between 0 and 100 that indicates the progress of the activity.
--
-- * 'aStatusMessage' - A friendly, more verbose description of the activity status.
--
-- * 'aEndTime' - The end time of the activity.
--
-- * 'aDetails' - The details about the activity.
--
-- * 'aDescription' - A friendly, more verbose description of the activity.
--
-- * 'aActivityId' - The ID of the activity.
--
-- * 'aAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'aCause' - The reason the activity began.
--
-- * 'aStartTime' - The start time of the activity.
--
-- * 'aStatusCode' - The current status of the activity.
activity ::
  -- | 'aActivityId'
  Text ->
  -- | 'aAutoScalingGroupName'
  Text ->
  -- | 'aCause'
  Text ->
  -- | 'aStartTime'
  UTCTime ->
  -- | 'aStatusCode'
  ScalingActivityStatusCode ->
  Activity
activity
  pActivityId_
  pAutoScalingGroupName_
  pCause_
  pStartTime_
  pStatusCode_ =
    Activity'
      { _aProgress = Nothing,
        _aStatusMessage = Nothing,
        _aEndTime = Nothing,
        _aDetails = Nothing,
        _aDescription = Nothing,
        _aActivityId = pActivityId_,
        _aAutoScalingGroupName = pAutoScalingGroupName_,
        _aCause = pCause_,
        _aStartTime = _Time # pStartTime_,
        _aStatusCode = pStatusCode_
      }

-- | A value between 0 and 100 that indicates the progress of the activity.
aProgress :: Lens' Activity (Maybe Int)
aProgress = lens _aProgress (\s a -> s {_aProgress = a})

-- | A friendly, more verbose description of the activity status.
aStatusMessage :: Lens' Activity (Maybe Text)
aStatusMessage = lens _aStatusMessage (\s a -> s {_aStatusMessage = a})

-- | The end time of the activity.
aEndTime :: Lens' Activity (Maybe UTCTime)
aEndTime = lens _aEndTime (\s a -> s {_aEndTime = a}) . mapping _Time

-- | The details about the activity.
aDetails :: Lens' Activity (Maybe Text)
aDetails = lens _aDetails (\s a -> s {_aDetails = a})

-- | A friendly, more verbose description of the activity.
aDescription :: Lens' Activity (Maybe Text)
aDescription = lens _aDescription (\s a -> s {_aDescription = a})

-- | The ID of the activity.
aActivityId :: Lens' Activity Text
aActivityId = lens _aActivityId (\s a -> s {_aActivityId = a})

-- | The name of the Auto Scaling group.
aAutoScalingGroupName :: Lens' Activity Text
aAutoScalingGroupName = lens _aAutoScalingGroupName (\s a -> s {_aAutoScalingGroupName = a})

-- | The reason the activity began.
aCause :: Lens' Activity Text
aCause = lens _aCause (\s a -> s {_aCause = a})

-- | The start time of the activity.
aStartTime :: Lens' Activity UTCTime
aStartTime = lens _aStartTime (\s a -> s {_aStartTime = a}) . _Time

-- | The current status of the activity.
aStatusCode :: Lens' Activity ScalingActivityStatusCode
aStatusCode = lens _aStatusCode (\s a -> s {_aStatusCode = a})

instance FromXML Activity where
  parseXML x =
    Activity'
      <$> (x .@? "Progress")
      <*> (x .@? "StatusMessage")
      <*> (x .@? "EndTime")
      <*> (x .@? "Details")
      <*> (x .@? "Description")
      <*> (x .@ "ActivityId")
      <*> (x .@ "AutoScalingGroupName")
      <*> (x .@ "Cause")
      <*> (x .@ "StartTime")
      <*> (x .@ "StatusCode")

instance Hashable Activity

instance NFData Activity
