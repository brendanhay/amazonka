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
-- Module      : Network.AWS.Connect.SuspendContactRecording
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, this API suspends recording the call. For example, you might suspend the call recording while collecting sensitive information, such as a credit card number. Then use ResumeContactRecording to restart recording.
--
--
-- The period of time that the recording is suspended is filled with silence in the final recording.
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.SuspendContactRecording
  ( -- * Creating a Request
    suspendContactRecording,
    SuspendContactRecording,

    -- * Request Lenses
    sInstanceId,
    sContactId,
    sInitialContactId,

    -- * Destructuring the Response
    suspendContactRecordingResponse,
    SuspendContactRecordingResponse,

    -- * Response Lenses
    srsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'suspendContactRecording' smart constructor.
data SuspendContactRecording = SuspendContactRecording'
  { _sInstanceId ::
      !Text,
    _sContactId :: !Text,
    _sInitialContactId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuspendContactRecording' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'sContactId' - The identifier of the contact.
--
-- * 'sInitialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
suspendContactRecording ::
  -- | 'sInstanceId'
  Text ->
  -- | 'sContactId'
  Text ->
  -- | 'sInitialContactId'
  Text ->
  SuspendContactRecording
suspendContactRecording pInstanceId_ pContactId_ pInitialContactId_ =
  SuspendContactRecording'
    { _sInstanceId = pInstanceId_,
      _sContactId = pContactId_,
      _sInitialContactId = pInitialContactId_
    }

-- | The identifier of the Amazon Connect instance.
sInstanceId :: Lens' SuspendContactRecording Text
sInstanceId = lens _sInstanceId (\s a -> s {_sInstanceId = a})

-- | The identifier of the contact.
sContactId :: Lens' SuspendContactRecording Text
sContactId = lens _sContactId (\s a -> s {_sContactId = a})

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
sInitialContactId :: Lens' SuspendContactRecording Text
sInitialContactId = lens _sInitialContactId (\s a -> s {_sInitialContactId = a})

instance AWSRequest SuspendContactRecording where
  type Rs SuspendContactRecording = SuspendContactRecordingResponse
  request = postJSON connect
  response =
    receiveEmpty
      ( \s h x ->
          SuspendContactRecordingResponse' <$> (pure (fromEnum s))
      )

instance Hashable SuspendContactRecording

instance NFData SuspendContactRecording

instance ToHeaders SuspendContactRecording where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON SuspendContactRecording where
  toJSON SuspendContactRecording' {..} =
    object
      ( catMaybes
          [ Just ("InstanceId" .= _sInstanceId),
            Just ("ContactId" .= _sContactId),
            Just ("InitialContactId" .= _sInitialContactId)
          ]
      )

instance ToPath SuspendContactRecording where
  toPath = const "/contact/suspend-recording"

instance ToQuery SuspendContactRecording where
  toQuery = const mempty

-- | /See:/ 'suspendContactRecordingResponse' smart constructor.
newtype SuspendContactRecordingResponse = SuspendContactRecordingResponse'
  { _srsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuspendContactRecordingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
suspendContactRecordingResponse ::
  -- | 'srsResponseStatus'
  Int ->
  SuspendContactRecordingResponse
suspendContactRecordingResponse pResponseStatus_ =
  SuspendContactRecordingResponse'
    { _srsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
srsResponseStatus :: Lens' SuspendContactRecordingResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData SuspendContactRecordingResponse
