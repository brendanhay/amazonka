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
-- Module      : Network.AWS.CloudWatchEvents.DescribeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about an archive.
module Network.AWS.CloudWatchEvents.DescribeArchive
  ( -- * Creating a Request
    describeArchive,
    DescribeArchive,

    -- * Request Lenses
    daArchiveName,

    -- * Destructuring the Response
    describeArchiveResponse,
    DescribeArchiveResponse,

    -- * Response Lenses
    darsCreationTime,
    darsSizeBytes,
    darsEventSourceARN,
    darsEventPattern,
    darsState,
    darsEventCount,
    darsArchiveName,
    darsRetentionDays,
    darsArchiveARN,
    darsStateReason,
    darsDescription,
    darsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeArchive' smart constructor.
newtype DescribeArchive = DescribeArchive' {_daArchiveName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeArchive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daArchiveName' - The name of the archive to retrieve.
describeArchive ::
  -- | 'daArchiveName'
  Text ->
  DescribeArchive
describeArchive pArchiveName_ =
  DescribeArchive' {_daArchiveName = pArchiveName_}

-- | The name of the archive to retrieve.
daArchiveName :: Lens' DescribeArchive Text
daArchiveName = lens _daArchiveName (\s a -> s {_daArchiveName = a})

instance AWSRequest DescribeArchive where
  type Rs DescribeArchive = DescribeArchiveResponse
  request = postJSON cloudWatchEvents
  response =
    receiveJSON
      ( \s h x ->
          DescribeArchiveResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "SizeBytes")
            <*> (x .?> "EventSourceArn")
            <*> (x .?> "EventPattern")
            <*> (x .?> "State")
            <*> (x .?> "EventCount")
            <*> (x .?> "ArchiveName")
            <*> (x .?> "RetentionDays")
            <*> (x .?> "ArchiveArn")
            <*> (x .?> "StateReason")
            <*> (x .?> "Description")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeArchive

instance NFData DescribeArchive

instance ToHeaders DescribeArchive where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.DescribeArchive" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeArchive where
  toJSON DescribeArchive' {..} =
    object (catMaybes [Just ("ArchiveName" .= _daArchiveName)])

instance ToPath DescribeArchive where
  toPath = const "/"

instance ToQuery DescribeArchive where
  toQuery = const mempty

-- | /See:/ 'describeArchiveResponse' smart constructor.
data DescribeArchiveResponse = DescribeArchiveResponse'
  { _darsCreationTime ::
      !(Maybe POSIX),
    _darsSizeBytes :: !(Maybe Integer),
    _darsEventSourceARN :: !(Maybe Text),
    _darsEventPattern :: !(Maybe Text),
    _darsState :: !(Maybe ArchiveState),
    _darsEventCount :: !(Maybe Integer),
    _darsArchiveName :: !(Maybe Text),
    _darsRetentionDays :: !(Maybe Nat),
    _darsArchiveARN :: !(Maybe Text),
    _darsStateReason :: !(Maybe Text),
    _darsDescription :: !(Maybe Text),
    _darsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeArchiveResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsCreationTime' - The time at which the archive was created.
--
-- * 'darsSizeBytes' - The size of the archive in bytes.
--
-- * 'darsEventSourceARN' - The ARN of the event source associated with the archive.
--
-- * 'darsEventPattern' - The event pattern used to filter events sent to the archive.
--
-- * 'darsState' - The state of the archive.
--
-- * 'darsEventCount' - The number of events in the archive.
--
-- * 'darsArchiveName' - The name of the archive.
--
-- * 'darsRetentionDays' - The number of days to retain events for in the archive.
--
-- * 'darsArchiveARN' - The ARN of the archive.
--
-- * 'darsStateReason' - The reason that the archive is in the state.
--
-- * 'darsDescription' - The description of the archive.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeArchiveResponse ::
  -- | 'darsResponseStatus'
  Int ->
  DescribeArchiveResponse
describeArchiveResponse pResponseStatus_ =
  DescribeArchiveResponse'
    { _darsCreationTime = Nothing,
      _darsSizeBytes = Nothing,
      _darsEventSourceARN = Nothing,
      _darsEventPattern = Nothing,
      _darsState = Nothing,
      _darsEventCount = Nothing,
      _darsArchiveName = Nothing,
      _darsRetentionDays = Nothing,
      _darsArchiveARN = Nothing,
      _darsStateReason = Nothing,
      _darsDescription = Nothing,
      _darsResponseStatus = pResponseStatus_
    }

-- | The time at which the archive was created.
darsCreationTime :: Lens' DescribeArchiveResponse (Maybe UTCTime)
darsCreationTime = lens _darsCreationTime (\s a -> s {_darsCreationTime = a}) . mapping _Time

-- | The size of the archive in bytes.
darsSizeBytes :: Lens' DescribeArchiveResponse (Maybe Integer)
darsSizeBytes = lens _darsSizeBytes (\s a -> s {_darsSizeBytes = a})

-- | The ARN of the event source associated with the archive.
darsEventSourceARN :: Lens' DescribeArchiveResponse (Maybe Text)
darsEventSourceARN = lens _darsEventSourceARN (\s a -> s {_darsEventSourceARN = a})

-- | The event pattern used to filter events sent to the archive.
darsEventPattern :: Lens' DescribeArchiveResponse (Maybe Text)
darsEventPattern = lens _darsEventPattern (\s a -> s {_darsEventPattern = a})

-- | The state of the archive.
darsState :: Lens' DescribeArchiveResponse (Maybe ArchiveState)
darsState = lens _darsState (\s a -> s {_darsState = a})

-- | The number of events in the archive.
darsEventCount :: Lens' DescribeArchiveResponse (Maybe Integer)
darsEventCount = lens _darsEventCount (\s a -> s {_darsEventCount = a})

-- | The name of the archive.
darsArchiveName :: Lens' DescribeArchiveResponse (Maybe Text)
darsArchiveName = lens _darsArchiveName (\s a -> s {_darsArchiveName = a})

-- | The number of days to retain events for in the archive.
darsRetentionDays :: Lens' DescribeArchiveResponse (Maybe Natural)
darsRetentionDays = lens _darsRetentionDays (\s a -> s {_darsRetentionDays = a}) . mapping _Nat

-- | The ARN of the archive.
darsArchiveARN :: Lens' DescribeArchiveResponse (Maybe Text)
darsArchiveARN = lens _darsArchiveARN (\s a -> s {_darsArchiveARN = a})

-- | The reason that the archive is in the state.
darsStateReason :: Lens' DescribeArchiveResponse (Maybe Text)
darsStateReason = lens _darsStateReason (\s a -> s {_darsStateReason = a})

-- | The description of the archive.
darsDescription :: Lens' DescribeArchiveResponse (Maybe Text)
darsDescription = lens _darsDescription (\s a -> s {_darsDescription = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeArchiveResponse Int
darsResponseStatus = lens _darsResponseStatus (\s a -> s {_darsResponseStatus = a})

instance NFData DescribeArchiveResponse
