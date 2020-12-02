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
-- Module      : Network.AWS.Connect.ResumeContactRecording
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, and the recording has been suspended using SuspendContactRecording, this API resumes recording the call.
--
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.ResumeContactRecording
  ( -- * Creating a Request
    resumeContactRecording,
    ResumeContactRecording,

    -- * Request Lenses
    rcrInstanceId,
    rcrContactId,
    rcrInitialContactId,

    -- * Destructuring the Response
    resumeContactRecordingResponse,
    ResumeContactRecordingResponse,

    -- * Response Lenses
    rcrrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resumeContactRecording' smart constructor.
data ResumeContactRecording = ResumeContactRecording'
  { _rcrInstanceId ::
      !Text,
    _rcrContactId :: !Text,
    _rcrInitialContactId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResumeContactRecording' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'rcrContactId' - The identifier of the contact.
--
-- * 'rcrInitialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
resumeContactRecording ::
  -- | 'rcrInstanceId'
  Text ->
  -- | 'rcrContactId'
  Text ->
  -- | 'rcrInitialContactId'
  Text ->
  ResumeContactRecording
resumeContactRecording pInstanceId_ pContactId_ pInitialContactId_ =
  ResumeContactRecording'
    { _rcrInstanceId = pInstanceId_,
      _rcrContactId = pContactId_,
      _rcrInitialContactId = pInitialContactId_
    }

-- | The identifier of the Amazon Connect instance.
rcrInstanceId :: Lens' ResumeContactRecording Text
rcrInstanceId = lens _rcrInstanceId (\s a -> s {_rcrInstanceId = a})

-- | The identifier of the contact.
rcrContactId :: Lens' ResumeContactRecording Text
rcrContactId = lens _rcrContactId (\s a -> s {_rcrContactId = a})

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
rcrInitialContactId :: Lens' ResumeContactRecording Text
rcrInitialContactId = lens _rcrInitialContactId (\s a -> s {_rcrInitialContactId = a})

instance AWSRequest ResumeContactRecording where
  type Rs ResumeContactRecording = ResumeContactRecordingResponse
  request = postJSON connect
  response =
    receiveEmpty
      ( \s h x ->
          ResumeContactRecordingResponse' <$> (pure (fromEnum s))
      )

instance Hashable ResumeContactRecording

instance NFData ResumeContactRecording

instance ToHeaders ResumeContactRecording where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON ResumeContactRecording where
  toJSON ResumeContactRecording' {..} =
    object
      ( catMaybes
          [ Just ("InstanceId" .= _rcrInstanceId),
            Just ("ContactId" .= _rcrContactId),
            Just ("InitialContactId" .= _rcrInitialContactId)
          ]
      )

instance ToPath ResumeContactRecording where
  toPath = const "/contact/resume-recording"

instance ToQuery ResumeContactRecording where
  toQuery = const mempty

-- | /See:/ 'resumeContactRecordingResponse' smart constructor.
newtype ResumeContactRecordingResponse = ResumeContactRecordingResponse'
  { _rcrrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResumeContactRecordingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrrsResponseStatus' - -- | The response status code.
resumeContactRecordingResponse ::
  -- | 'rcrrsResponseStatus'
  Int ->
  ResumeContactRecordingResponse
resumeContactRecordingResponse pResponseStatus_ =
  ResumeContactRecordingResponse'
    { _rcrrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
rcrrsResponseStatus :: Lens' ResumeContactRecordingResponse Int
rcrrsResponseStatus = lens _rcrrsResponseStatus (\s a -> s {_rcrrsResponseStatus = a})

instance NFData ResumeContactRecordingResponse
