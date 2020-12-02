{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an activity scheduled during an execution.
--
--
--
-- /See:/ 'activityScheduledEventDetails' smart constructor.
data ActivityScheduledEventDetails = ActivityScheduledEventDetails'
  { _asedHeartbeatInSeconds ::
      !(Maybe Integer),
    _asedInputDetails ::
      !( Maybe
           HistoryEventExecutionDataDetails
       ),
    _asedInput ::
      !(Maybe (Sensitive Text)),
    _asedTimeoutInSeconds ::
      !(Maybe Integer),
    _asedResource :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityScheduledEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asedHeartbeatInSeconds' - The maximum allowed duration between two heartbeats for the activity task.
--
-- * 'asedInputDetails' - Contains details about the input for an execution history event.
--
-- * 'asedInput' - The JSON data input to the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- * 'asedTimeoutInSeconds' - The maximum allowed duration of the activity task.
--
-- * 'asedResource' - The Amazon Resource Name (ARN) of the scheduled activity.
activityScheduledEventDetails ::
  -- | 'asedResource'
  Text ->
  ActivityScheduledEventDetails
activityScheduledEventDetails pResource_ =
  ActivityScheduledEventDetails'
    { _asedHeartbeatInSeconds = Nothing,
      _asedInputDetails = Nothing,
      _asedInput = Nothing,
      _asedTimeoutInSeconds = Nothing,
      _asedResource = pResource_
    }

-- | The maximum allowed duration between two heartbeats for the activity task.
asedHeartbeatInSeconds :: Lens' ActivityScheduledEventDetails (Maybe Integer)
asedHeartbeatInSeconds = lens _asedHeartbeatInSeconds (\s a -> s {_asedHeartbeatInSeconds = a})

-- | Contains details about the input for an execution history event.
asedInputDetails :: Lens' ActivityScheduledEventDetails (Maybe HistoryEventExecutionDataDetails)
asedInputDetails = lens _asedInputDetails (\s a -> s {_asedInputDetails = a})

-- | The JSON data input to the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
asedInput :: Lens' ActivityScheduledEventDetails (Maybe Text)
asedInput = lens _asedInput (\s a -> s {_asedInput = a}) . mapping _Sensitive

-- | The maximum allowed duration of the activity task.
asedTimeoutInSeconds :: Lens' ActivityScheduledEventDetails (Maybe Integer)
asedTimeoutInSeconds = lens _asedTimeoutInSeconds (\s a -> s {_asedTimeoutInSeconds = a})

-- | The Amazon Resource Name (ARN) of the scheduled activity.
asedResource :: Lens' ActivityScheduledEventDetails Text
asedResource = lens _asedResource (\s a -> s {_asedResource = a})

instance FromJSON ActivityScheduledEventDetails where
  parseJSON =
    withObject
      "ActivityScheduledEventDetails"
      ( \x ->
          ActivityScheduledEventDetails'
            <$> (x .:? "heartbeatInSeconds")
            <*> (x .:? "inputDetails")
            <*> (x .:? "input")
            <*> (x .:? "timeoutInSeconds")
            <*> (x .: "resource")
      )

instance Hashable ActivityScheduledEventDetails

instance NFData ActivityScheduledEventDetails
