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
-- Module      : Network.AWS.Connect.StartContactRecording
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API starts recording the contact when the agent joins the call. StartContactRecording is a one-time action. For example, if you use StopContactRecording to stop recording an ongoing call, you can't use StartContactRecording to restart it. For scenarios where the recording has started and you want to suspend and resume it, such as when collecting sensitive information (for example, a credit card number), use SuspendContactRecording and ResumeContactRecording.
--
--
-- You can use this API to override the recording behavior configured in the <https://docs.aws.amazon.com/connect/latest/adminguide/set-recording-behavior.html Set recording behavior> block.
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.StartContactRecording
  ( -- * Creating a Request
    startContactRecording,
    StartContactRecording,

    -- * Request Lenses
    scrInstanceId,
    scrContactId,
    scrInitialContactId,
    scrVoiceRecordingConfiguration,

    -- * Destructuring the Response
    startContactRecordingResponse,
    StartContactRecordingResponse,

    -- * Response Lenses
    starsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startContactRecording' smart constructor.
data StartContactRecording = StartContactRecording'
  { _scrInstanceId ::
      !Text,
    _scrContactId :: !Text,
    _scrInitialContactId :: !Text,
    _scrVoiceRecordingConfiguration ::
      !VoiceRecordingConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartContactRecording' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'scrContactId' - The identifier of the contact.
--
-- * 'scrInitialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- * 'scrVoiceRecordingConfiguration' - Who is being recorded.
startContactRecording ::
  -- | 'scrInstanceId'
  Text ->
  -- | 'scrContactId'
  Text ->
  -- | 'scrInitialContactId'
  Text ->
  -- | 'scrVoiceRecordingConfiguration'
  VoiceRecordingConfiguration ->
  StartContactRecording
startContactRecording
  pInstanceId_
  pContactId_
  pInitialContactId_
  pVoiceRecordingConfiguration_ =
    StartContactRecording'
      { _scrInstanceId = pInstanceId_,
        _scrContactId = pContactId_,
        _scrInitialContactId = pInitialContactId_,
        _scrVoiceRecordingConfiguration = pVoiceRecordingConfiguration_
      }

-- | The identifier of the Amazon Connect instance.
scrInstanceId :: Lens' StartContactRecording Text
scrInstanceId = lens _scrInstanceId (\s a -> s {_scrInstanceId = a})

-- | The identifier of the contact.
scrContactId :: Lens' StartContactRecording Text
scrContactId = lens _scrContactId (\s a -> s {_scrContactId = a})

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
scrInitialContactId :: Lens' StartContactRecording Text
scrInitialContactId = lens _scrInitialContactId (\s a -> s {_scrInitialContactId = a})

-- | Who is being recorded.
scrVoiceRecordingConfiguration :: Lens' StartContactRecording VoiceRecordingConfiguration
scrVoiceRecordingConfiguration = lens _scrVoiceRecordingConfiguration (\s a -> s {_scrVoiceRecordingConfiguration = a})

instance AWSRequest StartContactRecording where
  type Rs StartContactRecording = StartContactRecordingResponse
  request = postJSON connect
  response =
    receiveEmpty
      (\s h x -> StartContactRecordingResponse' <$> (pure (fromEnum s)))

instance Hashable StartContactRecording

instance NFData StartContactRecording

instance ToHeaders StartContactRecording where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StartContactRecording where
  toJSON StartContactRecording' {..} =
    object
      ( catMaybes
          [ Just ("InstanceId" .= _scrInstanceId),
            Just ("ContactId" .= _scrContactId),
            Just ("InitialContactId" .= _scrInitialContactId),
            Just
              ( "VoiceRecordingConfiguration"
                  .= _scrVoiceRecordingConfiguration
              )
          ]
      )

instance ToPath StartContactRecording where
  toPath = const "/contact/start-recording"

instance ToQuery StartContactRecording where
  toQuery = const mempty

-- | /See:/ 'startContactRecordingResponse' smart constructor.
newtype StartContactRecordingResponse = StartContactRecordingResponse'
  { _starsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartContactRecordingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'starsResponseStatus' - -- | The response status code.
startContactRecordingResponse ::
  -- | 'starsResponseStatus'
  Int ->
  StartContactRecordingResponse
startContactRecordingResponse pResponseStatus_ =
  StartContactRecordingResponse'
    { _starsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
starsResponseStatus :: Lens' StartContactRecordingResponse Int
starsResponseStatus = lens _starsResponseStatus (\s a -> s {_starsResponseStatus = a})

instance NFData StartContactRecordingResponse
