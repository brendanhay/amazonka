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
-- Module      : Network.AWS.CloudWatchEvents.CreateArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an archive of events with the specified settings. When you create an archive, incoming events might not immediately start being sent to the archive. Allow a short period of time for changes to take effect. If you do not specify a pattern to filter events sent to the archive, all events are sent to the archive except replayed events. Replayed events are not sent to an archive.
module Network.AWS.CloudWatchEvents.CreateArchive
  ( -- * Creating a Request
    createArchive,
    CreateArchive,

    -- * Request Lenses
    caEventPattern,
    caRetentionDays,
    caDescription,
    caArchiveName,
    caEventSourceARN,

    -- * Destructuring the Response
    createArchiveResponse,
    CreateArchiveResponse,

    -- * Response Lenses
    carsCreationTime,
    carsState,
    carsArchiveARN,
    carsStateReason,
    carsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createArchive' smart constructor.
data CreateArchive = CreateArchive'
  { _caEventPattern ::
      !(Maybe Text),
    _caRetentionDays :: !(Maybe Nat),
    _caDescription :: !(Maybe Text),
    _caArchiveName :: !Text,
    _caEventSourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateArchive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caEventPattern' - An event pattern to use to filter events sent to the archive.
--
-- * 'caRetentionDays' - The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
--
-- * 'caDescription' - A description for the archive.
--
-- * 'caArchiveName' - The name for the archive to create.
--
-- * 'caEventSourceARN' - The ARN of the event source associated with the archive.
createArchive ::
  -- | 'caArchiveName'
  Text ->
  -- | 'caEventSourceARN'
  Text ->
  CreateArchive
createArchive pArchiveName_ pEventSourceARN_ =
  CreateArchive'
    { _caEventPattern = Nothing,
      _caRetentionDays = Nothing,
      _caDescription = Nothing,
      _caArchiveName = pArchiveName_,
      _caEventSourceARN = pEventSourceARN_
    }

-- | An event pattern to use to filter events sent to the archive.
caEventPattern :: Lens' CreateArchive (Maybe Text)
caEventPattern = lens _caEventPattern (\s a -> s {_caEventPattern = a})

-- | The number of days to retain events for. Default value is 0. If set to 0, events are retained indefinitely
caRetentionDays :: Lens' CreateArchive (Maybe Natural)
caRetentionDays = lens _caRetentionDays (\s a -> s {_caRetentionDays = a}) . mapping _Nat

-- | A description for the archive.
caDescription :: Lens' CreateArchive (Maybe Text)
caDescription = lens _caDescription (\s a -> s {_caDescription = a})

-- | The name for the archive to create.
caArchiveName :: Lens' CreateArchive Text
caArchiveName = lens _caArchiveName (\s a -> s {_caArchiveName = a})

-- | The ARN of the event source associated with the archive.
caEventSourceARN :: Lens' CreateArchive Text
caEventSourceARN = lens _caEventSourceARN (\s a -> s {_caEventSourceARN = a})

instance AWSRequest CreateArchive where
  type Rs CreateArchive = CreateArchiveResponse
  request = postJSON cloudWatchEvents
  response =
    receiveJSON
      ( \s h x ->
          CreateArchiveResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "State")
            <*> (x .?> "ArchiveArn")
            <*> (x .?> "StateReason")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateArchive

instance NFData CreateArchive

instance ToHeaders CreateArchive where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.CreateArchive" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateArchive where
  toJSON CreateArchive' {..} =
    object
      ( catMaybes
          [ ("EventPattern" .=) <$> _caEventPattern,
            ("RetentionDays" .=) <$> _caRetentionDays,
            ("Description" .=) <$> _caDescription,
            Just ("ArchiveName" .= _caArchiveName),
            Just ("EventSourceArn" .= _caEventSourceARN)
          ]
      )

instance ToPath CreateArchive where
  toPath = const "/"

instance ToQuery CreateArchive where
  toQuery = const mempty

-- | /See:/ 'createArchiveResponse' smart constructor.
data CreateArchiveResponse = CreateArchiveResponse'
  { _carsCreationTime ::
      !(Maybe POSIX),
    _carsState :: !(Maybe ArchiveState),
    _carsArchiveARN :: !(Maybe Text),
    _carsStateReason :: !(Maybe Text),
    _carsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateArchiveResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsCreationTime' - The time at which the archive was created.
--
-- * 'carsState' - The state of the archive that was created.
--
-- * 'carsArchiveARN' - The ARN of the archive that was created.
--
-- * 'carsStateReason' - The reason that the archive is in the state.
--
-- * 'carsResponseStatus' - -- | The response status code.
createArchiveResponse ::
  -- | 'carsResponseStatus'
  Int ->
  CreateArchiveResponse
createArchiveResponse pResponseStatus_ =
  CreateArchiveResponse'
    { _carsCreationTime = Nothing,
      _carsState = Nothing,
      _carsArchiveARN = Nothing,
      _carsStateReason = Nothing,
      _carsResponseStatus = pResponseStatus_
    }

-- | The time at which the archive was created.
carsCreationTime :: Lens' CreateArchiveResponse (Maybe UTCTime)
carsCreationTime = lens _carsCreationTime (\s a -> s {_carsCreationTime = a}) . mapping _Time

-- | The state of the archive that was created.
carsState :: Lens' CreateArchiveResponse (Maybe ArchiveState)
carsState = lens _carsState (\s a -> s {_carsState = a})

-- | The ARN of the archive that was created.
carsArchiveARN :: Lens' CreateArchiveResponse (Maybe Text)
carsArchiveARN = lens _carsArchiveARN (\s a -> s {_carsArchiveARN = a})

-- | The reason that the archive is in the state.
carsStateReason :: Lens' CreateArchiveResponse (Maybe Text)
carsStateReason = lens _carsStateReason (\s a -> s {_carsStateReason = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateArchiveResponse Int
carsResponseStatus = lens _carsResponseStatus (\s a -> s {_carsResponseStatus = a})

instance NFData CreateArchiveResponse
