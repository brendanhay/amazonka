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
-- Module      : Network.AWS.Connect.StopContactRecording
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, this API stops recording the call. StopContactRecording is a one-time action. If you use StopContactRecording to stop recording an ongoing call, you can't use StartContactRecording to restart it. For scenarios where the recording has started and you want to suspend it for sensitive information (for example, to collect a credit card number), and then restart it, use SuspendContactRecording and ResumeContactRecording.
--
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.StopContactRecording
  ( -- * Creating a Request
    stopContactRecording,
    StopContactRecording,

    -- * Request Lenses
    stoInstanceId,
    stoContactId,
    stoInitialContactId,

    -- * Destructuring the Response
    stopContactRecordingResponse,
    StopContactRecordingResponse,

    -- * Response Lenses
    scrrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopContactRecording' smart constructor.
data StopContactRecording = StopContactRecording'
  { _stoInstanceId ::
      !Text,
    _stoContactId :: !Text,
    _stoInitialContactId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopContactRecording' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stoInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'stoContactId' - The identifier of the contact.
--
-- * 'stoInitialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
stopContactRecording ::
  -- | 'stoInstanceId'
  Text ->
  -- | 'stoContactId'
  Text ->
  -- | 'stoInitialContactId'
  Text ->
  StopContactRecording
stopContactRecording pInstanceId_ pContactId_ pInitialContactId_ =
  StopContactRecording'
    { _stoInstanceId = pInstanceId_,
      _stoContactId = pContactId_,
      _stoInitialContactId = pInitialContactId_
    }

-- | The identifier of the Amazon Connect instance.
stoInstanceId :: Lens' StopContactRecording Text
stoInstanceId = lens _stoInstanceId (\s a -> s {_stoInstanceId = a})

-- | The identifier of the contact.
stoContactId :: Lens' StopContactRecording Text
stoContactId = lens _stoContactId (\s a -> s {_stoContactId = a})

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
stoInitialContactId :: Lens' StopContactRecording Text
stoInitialContactId = lens _stoInitialContactId (\s a -> s {_stoInitialContactId = a})

instance AWSRequest StopContactRecording where
  type Rs StopContactRecording = StopContactRecordingResponse
  request = postJSON connect
  response =
    receiveEmpty
      (\s h x -> StopContactRecordingResponse' <$> (pure (fromEnum s)))

instance Hashable StopContactRecording

instance NFData StopContactRecording

instance ToHeaders StopContactRecording where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StopContactRecording where
  toJSON StopContactRecording' {..} =
    object
      ( catMaybes
          [ Just ("InstanceId" .= _stoInstanceId),
            Just ("ContactId" .= _stoContactId),
            Just ("InitialContactId" .= _stoInitialContactId)
          ]
      )

instance ToPath StopContactRecording where
  toPath = const "/contact/stop-recording"

instance ToQuery StopContactRecording where
  toQuery = const mempty

-- | /See:/ 'stopContactRecordingResponse' smart constructor.
newtype StopContactRecordingResponse = StopContactRecordingResponse'
  { _scrrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopContactRecordingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrrsResponseStatus' - -- | The response status code.
stopContactRecordingResponse ::
  -- | 'scrrsResponseStatus'
  Int ->
  StopContactRecordingResponse
stopContactRecordingResponse pResponseStatus_ =
  StopContactRecordingResponse'
    { _scrrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
scrrsResponseStatus :: Lens' StopContactRecordingResponse Int
scrrsResponseStatus = lens _scrrsResponseStatus (\s a -> s {_scrrsResponseStatus = a})

instance NFData StopContactRecordingResponse
