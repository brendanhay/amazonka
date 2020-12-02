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
-- Module      : Network.AWS.CloudWatchEvents.DescribeReplay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a replay. Use @DescribeReplay@ to determine the progress of a running replay. A replay processes events to replay based on the time in the event, and replays them using 1 minute intervals. If you use @StartReplay@ and specify an @EventStartTime@ and an @EventEndTime@ that covers a 20 minute time range, the events are replayed from the first minute of that 20 minute range first. Then the events from the second minute are replayed. You can use @DescribeReplay@ to determine the progress of a replay. The value returned for @EventLastReplayedTime@ indicates the time within the specified time range associated with the last event replayed.
module Network.AWS.CloudWatchEvents.DescribeReplay
  ( -- * Creating a Request
    describeReplay,
    DescribeReplay,

    -- * Request Lenses
    drReplayName,

    -- * Destructuring the Response
    describeReplayResponse,
    DescribeReplayResponse,

    -- * Response Lenses
    drsEventSourceARN,
    drsDestination,
    drsState,
    drsEventEndTime,
    drsReplayStartTime,
    drsReplayARN,
    drsReplayEndTime,
    drsEventLastReplayedTime,
    drsEventStartTime,
    drsReplayName,
    drsStateReason,
    drsDescription,
    drsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeReplay' smart constructor.
newtype DescribeReplay = DescribeReplay' {_drReplayName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReplay' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drReplayName' - The name of the replay to retrieve.
describeReplay ::
  -- | 'drReplayName'
  Text ->
  DescribeReplay
describeReplay pReplayName_ =
  DescribeReplay' {_drReplayName = pReplayName_}

-- | The name of the replay to retrieve.
drReplayName :: Lens' DescribeReplay Text
drReplayName = lens _drReplayName (\s a -> s {_drReplayName = a})

instance AWSRequest DescribeReplay where
  type Rs DescribeReplay = DescribeReplayResponse
  request = postJSON cloudWatchEvents
  response =
    receiveJSON
      ( \s h x ->
          DescribeReplayResponse'
            <$> (x .?> "EventSourceArn")
            <*> (x .?> "Destination")
            <*> (x .?> "State")
            <*> (x .?> "EventEndTime")
            <*> (x .?> "ReplayStartTime")
            <*> (x .?> "ReplayArn")
            <*> (x .?> "ReplayEndTime")
            <*> (x .?> "EventLastReplayedTime")
            <*> (x .?> "EventStartTime")
            <*> (x .?> "ReplayName")
            <*> (x .?> "StateReason")
            <*> (x .?> "Description")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReplay

instance NFData DescribeReplay

instance ToHeaders DescribeReplay where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.DescribeReplay" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeReplay where
  toJSON DescribeReplay' {..} =
    object (catMaybes [Just ("ReplayName" .= _drReplayName)])

instance ToPath DescribeReplay where
  toPath = const "/"

instance ToQuery DescribeReplay where
  toQuery = const mempty

-- | /See:/ 'describeReplayResponse' smart constructor.
data DescribeReplayResponse = DescribeReplayResponse'
  { _drsEventSourceARN ::
      !(Maybe Text),
    _drsDestination :: !(Maybe ReplayDestination),
    _drsState :: !(Maybe ReplayState),
    _drsEventEndTime :: !(Maybe POSIX),
    _drsReplayStartTime :: !(Maybe POSIX),
    _drsReplayARN :: !(Maybe Text),
    _drsReplayEndTime :: !(Maybe POSIX),
    _drsEventLastReplayedTime :: !(Maybe POSIX),
    _drsEventStartTime :: !(Maybe POSIX),
    _drsReplayName :: !(Maybe Text),
    _drsStateReason :: !(Maybe Text),
    _drsDescription :: !(Maybe Text),
    _drsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReplayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsEventSourceARN' - The ARN of the archive events were replayed from.
--
-- * 'drsDestination' - A @ReplayDestination@ object that contains details about the replay.
--
-- * 'drsState' - The current state of the replay.
--
-- * 'drsEventEndTime' - The time stamp for the last event that was replayed from the archive.
--
-- * 'drsReplayStartTime' - A time stamp for the time that the replay started.
--
-- * 'drsReplayARN' - The ARN of the replay.
--
-- * 'drsReplayEndTime' - A time stamp for the time that the replay stopped.
--
-- * 'drsEventLastReplayedTime' - The time that the event was last replayed.
--
-- * 'drsEventStartTime' - The time stamp of the first event that was last replayed from the archive.
--
-- * 'drsReplayName' - The name of the replay.
--
-- * 'drsStateReason' - The reason that the replay is in the current state.
--
-- * 'drsDescription' - The description of the replay.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeReplayResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DescribeReplayResponse
describeReplayResponse pResponseStatus_ =
  DescribeReplayResponse'
    { _drsEventSourceARN = Nothing,
      _drsDestination = Nothing,
      _drsState = Nothing,
      _drsEventEndTime = Nothing,
      _drsReplayStartTime = Nothing,
      _drsReplayARN = Nothing,
      _drsReplayEndTime = Nothing,
      _drsEventLastReplayedTime = Nothing,
      _drsEventStartTime = Nothing,
      _drsReplayName = Nothing,
      _drsStateReason = Nothing,
      _drsDescription = Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | The ARN of the archive events were replayed from.
drsEventSourceARN :: Lens' DescribeReplayResponse (Maybe Text)
drsEventSourceARN = lens _drsEventSourceARN (\s a -> s {_drsEventSourceARN = a})

-- | A @ReplayDestination@ object that contains details about the replay.
drsDestination :: Lens' DescribeReplayResponse (Maybe ReplayDestination)
drsDestination = lens _drsDestination (\s a -> s {_drsDestination = a})

-- | The current state of the replay.
drsState :: Lens' DescribeReplayResponse (Maybe ReplayState)
drsState = lens _drsState (\s a -> s {_drsState = a})

-- | The time stamp for the last event that was replayed from the archive.
drsEventEndTime :: Lens' DescribeReplayResponse (Maybe UTCTime)
drsEventEndTime = lens _drsEventEndTime (\s a -> s {_drsEventEndTime = a}) . mapping _Time

-- | A time stamp for the time that the replay started.
drsReplayStartTime :: Lens' DescribeReplayResponse (Maybe UTCTime)
drsReplayStartTime = lens _drsReplayStartTime (\s a -> s {_drsReplayStartTime = a}) . mapping _Time

-- | The ARN of the replay.
drsReplayARN :: Lens' DescribeReplayResponse (Maybe Text)
drsReplayARN = lens _drsReplayARN (\s a -> s {_drsReplayARN = a})

-- | A time stamp for the time that the replay stopped.
drsReplayEndTime :: Lens' DescribeReplayResponse (Maybe UTCTime)
drsReplayEndTime = lens _drsReplayEndTime (\s a -> s {_drsReplayEndTime = a}) . mapping _Time

-- | The time that the event was last replayed.
drsEventLastReplayedTime :: Lens' DescribeReplayResponse (Maybe UTCTime)
drsEventLastReplayedTime = lens _drsEventLastReplayedTime (\s a -> s {_drsEventLastReplayedTime = a}) . mapping _Time

-- | The time stamp of the first event that was last replayed from the archive.
drsEventStartTime :: Lens' DescribeReplayResponse (Maybe UTCTime)
drsEventStartTime = lens _drsEventStartTime (\s a -> s {_drsEventStartTime = a}) . mapping _Time

-- | The name of the replay.
drsReplayName :: Lens' DescribeReplayResponse (Maybe Text)
drsReplayName = lens _drsReplayName (\s a -> s {_drsReplayName = a})

-- | The reason that the replay is in the current state.
drsStateReason :: Lens' DescribeReplayResponse (Maybe Text)
drsStateReason = lens _drsStateReason (\s a -> s {_drsStateReason = a})

-- | The description of the replay.
drsDescription :: Lens' DescribeReplayResponse (Maybe Text)
drsDescription = lens _drsDescription (\s a -> s {_drsDescription = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeReplayResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DescribeReplayResponse
