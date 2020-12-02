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
-- Module      : Network.AWS.CloudWatchEvents.UpdateArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified archive.
module Network.AWS.CloudWatchEvents.UpdateArchive
  ( -- * Creating a Request
    updateArchive,
    UpdateArchive,

    -- * Request Lenses
    uaEventPattern,
    uaRetentionDays,
    uaDescription,
    uaArchiveName,

    -- * Destructuring the Response
    updateArchiveResponse,
    UpdateArchiveResponse,

    -- * Response Lenses
    uarsCreationTime,
    uarsState,
    uarsArchiveARN,
    uarsStateReason,
    uarsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateArchive' smart constructor.
data UpdateArchive = UpdateArchive'
  { _uaEventPattern ::
      !(Maybe Text),
    _uaRetentionDays :: !(Maybe Nat),
    _uaDescription :: !(Maybe Text),
    _uaArchiveName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateArchive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaEventPattern' - The event pattern to use to filter events sent to the archive.
--
-- * 'uaRetentionDays' - The number of days to retain events in the archive.
--
-- * 'uaDescription' - The description for the archive.
--
-- * 'uaArchiveName' - The name of the archive to update.
updateArchive ::
  -- | 'uaArchiveName'
  Text ->
  UpdateArchive
updateArchive pArchiveName_ =
  UpdateArchive'
    { _uaEventPattern = Nothing,
      _uaRetentionDays = Nothing,
      _uaDescription = Nothing,
      _uaArchiveName = pArchiveName_
    }

-- | The event pattern to use to filter events sent to the archive.
uaEventPattern :: Lens' UpdateArchive (Maybe Text)
uaEventPattern = lens _uaEventPattern (\s a -> s {_uaEventPattern = a})

-- | The number of days to retain events in the archive.
uaRetentionDays :: Lens' UpdateArchive (Maybe Natural)
uaRetentionDays = lens _uaRetentionDays (\s a -> s {_uaRetentionDays = a}) . mapping _Nat

-- | The description for the archive.
uaDescription :: Lens' UpdateArchive (Maybe Text)
uaDescription = lens _uaDescription (\s a -> s {_uaDescription = a})

-- | The name of the archive to update.
uaArchiveName :: Lens' UpdateArchive Text
uaArchiveName = lens _uaArchiveName (\s a -> s {_uaArchiveName = a})

instance AWSRequest UpdateArchive where
  type Rs UpdateArchive = UpdateArchiveResponse
  request = postJSON cloudWatchEvents
  response =
    receiveJSON
      ( \s h x ->
          UpdateArchiveResponse'
            <$> (x .?> "CreationTime")
            <*> (x .?> "State")
            <*> (x .?> "ArchiveArn")
            <*> (x .?> "StateReason")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateArchive

instance NFData UpdateArchive

instance ToHeaders UpdateArchive where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.UpdateArchive" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateArchive where
  toJSON UpdateArchive' {..} =
    object
      ( catMaybes
          [ ("EventPattern" .=) <$> _uaEventPattern,
            ("RetentionDays" .=) <$> _uaRetentionDays,
            ("Description" .=) <$> _uaDescription,
            Just ("ArchiveName" .= _uaArchiveName)
          ]
      )

instance ToPath UpdateArchive where
  toPath = const "/"

instance ToQuery UpdateArchive where
  toQuery = const mempty

-- | /See:/ 'updateArchiveResponse' smart constructor.
data UpdateArchiveResponse = UpdateArchiveResponse'
  { _uarsCreationTime ::
      !(Maybe POSIX),
    _uarsState :: !(Maybe ArchiveState),
    _uarsArchiveARN :: !(Maybe Text),
    _uarsStateReason :: !(Maybe Text),
    _uarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateArchiveResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsCreationTime' - The time at which the archive was updated.
--
-- * 'uarsState' - The state of the archive.
--
-- * 'uarsArchiveARN' - The ARN of the archive.
--
-- * 'uarsStateReason' - The reason that the archive is in the current state.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateArchiveResponse ::
  -- | 'uarsResponseStatus'
  Int ->
  UpdateArchiveResponse
updateArchiveResponse pResponseStatus_ =
  UpdateArchiveResponse'
    { _uarsCreationTime = Nothing,
      _uarsState = Nothing,
      _uarsArchiveARN = Nothing,
      _uarsStateReason = Nothing,
      _uarsResponseStatus = pResponseStatus_
    }

-- | The time at which the archive was updated.
uarsCreationTime :: Lens' UpdateArchiveResponse (Maybe UTCTime)
uarsCreationTime = lens _uarsCreationTime (\s a -> s {_uarsCreationTime = a}) . mapping _Time

-- | The state of the archive.
uarsState :: Lens' UpdateArchiveResponse (Maybe ArchiveState)
uarsState = lens _uarsState (\s a -> s {_uarsState = a})

-- | The ARN of the archive.
uarsArchiveARN :: Lens' UpdateArchiveResponse (Maybe Text)
uarsArchiveARN = lens _uarsArchiveARN (\s a -> s {_uarsArchiveARN = a})

-- | The reason that the archive is in the current state.
uarsStateReason :: Lens' UpdateArchiveResponse (Maybe Text)
uarsStateReason = lens _uarsStateReason (\s a -> s {_uarsStateReason = a})

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateArchiveResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\s a -> s {_uarsResponseStatus = a})

instance NFData UpdateArchiveResponse
