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
-- Module      : Network.AWS.MediaLive.RejectInputDeviceTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reject the transfer of the specified input device to your AWS account.
module Network.AWS.MediaLive.RejectInputDeviceTransfer
  ( -- * Creating a Request
    rejectInputDeviceTransfer,
    RejectInputDeviceTransfer,

    -- * Request Lenses
    ridtInputDeviceId,

    -- * Destructuring the Response
    rejectInputDeviceTransferResponse,
    RejectInputDeviceTransferResponse,

    -- * Response Lenses
    ridtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for RejectInputDeviceTransferRequest
--
-- /See:/ 'rejectInputDeviceTransfer' smart constructor.
newtype RejectInputDeviceTransfer = RejectInputDeviceTransfer'
  { _ridtInputDeviceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RejectInputDeviceTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ridtInputDeviceId' - The unique ID of the input device to reject. For example, hd-123456789abcdef.
rejectInputDeviceTransfer ::
  -- | 'ridtInputDeviceId'
  Text ->
  RejectInputDeviceTransfer
rejectInputDeviceTransfer pInputDeviceId_ =
  RejectInputDeviceTransfer' {_ridtInputDeviceId = pInputDeviceId_}

-- | The unique ID of the input device to reject. For example, hd-123456789abcdef.
ridtInputDeviceId :: Lens' RejectInputDeviceTransfer Text
ridtInputDeviceId = lens _ridtInputDeviceId (\s a -> s {_ridtInputDeviceId = a})

instance AWSRequest RejectInputDeviceTransfer where
  type
    Rs RejectInputDeviceTransfer =
      RejectInputDeviceTransferResponse
  request = postJSON mediaLive
  response =
    receiveEmpty
      ( \s h x ->
          RejectInputDeviceTransferResponse' <$> (pure (fromEnum s))
      )

instance Hashable RejectInputDeviceTransfer

instance NFData RejectInputDeviceTransfer

instance ToHeaders RejectInputDeviceTransfer where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON RejectInputDeviceTransfer where
  toJSON = const (Object mempty)

instance ToPath RejectInputDeviceTransfer where
  toPath RejectInputDeviceTransfer' {..} =
    mconcat
      ["/prod/inputDevices/", toBS _ridtInputDeviceId, "/reject"]

instance ToQuery RejectInputDeviceTransfer where
  toQuery = const mempty

-- | Placeholder documentation for RejectInputDeviceTransferResponse
--
-- /See:/ 'rejectInputDeviceTransferResponse' smart constructor.
newtype RejectInputDeviceTransferResponse = RejectInputDeviceTransferResponse'
  { _ridtrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RejectInputDeviceTransferResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ridtrsResponseStatus' - -- | The response status code.
rejectInputDeviceTransferResponse ::
  -- | 'ridtrsResponseStatus'
  Int ->
  RejectInputDeviceTransferResponse
rejectInputDeviceTransferResponse pResponseStatus_ =
  RejectInputDeviceTransferResponse'
    { _ridtrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ridtrsResponseStatus :: Lens' RejectInputDeviceTransferResponse Int
ridtrsResponseStatus = lens _ridtrsResponseStatus (\s a -> s {_ridtrsResponseStatus = a})

instance NFData RejectInputDeviceTransferResponse
