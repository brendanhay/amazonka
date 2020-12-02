{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.LogStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.LogStream where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a log stream, which is a sequence of log events from a single emitter of logs.
--
--
--
-- /See:/ 'logStream' smart constructor.
data LogStream = LogStream'
  { _lsCreationTime :: !(Maybe Nat),
    _lsUploadSequenceToken :: !(Maybe Text),
    _lsArn :: !(Maybe Text),
    _lsFirstEventTimestamp :: !(Maybe Nat),
    _lsLogStreamName :: !(Maybe Text),
    _lsStoredBytes :: !(Maybe Nat),
    _lsLastIngestionTime :: !(Maybe Nat),
    _lsLastEventTimestamp :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsCreationTime' - The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'lsUploadSequenceToken' - The sequence token.
--
-- * 'lsArn' - The Amazon Resource Name (ARN) of the log stream.
--
-- * 'lsFirstEventTimestamp' - The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'lsLogStreamName' - The name of the log stream.
--
-- * 'lsStoredBytes' - The number of bytes stored. __Important:__ On June 17, 2019, this parameter was deprecated for log streams, and is always reported as zero. This change applies only to log streams. The @storedBytes@ parameter for log groups is not affected.
--
-- * 'lsLastIngestionTime' - The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'lsLastEventTimestamp' - The time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. The @lastEventTime@ value updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
logStream ::
  LogStream
logStream =
  LogStream'
    { _lsCreationTime = Nothing,
      _lsUploadSequenceToken = Nothing,
      _lsArn = Nothing,
      _lsFirstEventTimestamp = Nothing,
      _lsLogStreamName = Nothing,
      _lsStoredBytes = Nothing,
      _lsLastIngestionTime = Nothing,
      _lsLastEventTimestamp = Nothing
    }

-- | The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
lsCreationTime :: Lens' LogStream (Maybe Natural)
lsCreationTime = lens _lsCreationTime (\s a -> s {_lsCreationTime = a}) . mapping _Nat

-- | The sequence token.
lsUploadSequenceToken :: Lens' LogStream (Maybe Text)
lsUploadSequenceToken = lens _lsUploadSequenceToken (\s a -> s {_lsUploadSequenceToken = a})

-- | The Amazon Resource Name (ARN) of the log stream.
lsArn :: Lens' LogStream (Maybe Text)
lsArn = lens _lsArn (\s a -> s {_lsArn = a})

-- | The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
lsFirstEventTimestamp :: Lens' LogStream (Maybe Natural)
lsFirstEventTimestamp = lens _lsFirstEventTimestamp (\s a -> s {_lsFirstEventTimestamp = a}) . mapping _Nat

-- | The name of the log stream.
lsLogStreamName :: Lens' LogStream (Maybe Text)
lsLogStreamName = lens _lsLogStreamName (\s a -> s {_lsLogStreamName = a})

-- | The number of bytes stored. __Important:__ On June 17, 2019, this parameter was deprecated for log streams, and is always reported as zero. This change applies only to log streams. The @storedBytes@ parameter for log groups is not affected.
lsStoredBytes :: Lens' LogStream (Maybe Natural)
lsStoredBytes = lens _lsStoredBytes (\s a -> s {_lsStoredBytes = a}) . mapping _Nat

-- | The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
lsLastIngestionTime :: Lens' LogStream (Maybe Natural)
lsLastIngestionTime = lens _lsLastIngestionTime (\s a -> s {_lsLastIngestionTime = a}) . mapping _Nat

-- | The time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. The @lastEventTime@ value updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
lsLastEventTimestamp :: Lens' LogStream (Maybe Natural)
lsLastEventTimestamp = lens _lsLastEventTimestamp (\s a -> s {_lsLastEventTimestamp = a}) . mapping _Nat

instance FromJSON LogStream where
  parseJSON =
    withObject
      "LogStream"
      ( \x ->
          LogStream'
            <$> (x .:? "creationTime")
            <*> (x .:? "uploadSequenceToken")
            <*> (x .:? "arn")
            <*> (x .:? "firstEventTimestamp")
            <*> (x .:? "logStreamName")
            <*> (x .:? "storedBytes")
            <*> (x .:? "lastIngestionTime")
            <*> (x .:? "lastEventTimestamp")
      )

instance Hashable LogStream

instance NFData LogStream
