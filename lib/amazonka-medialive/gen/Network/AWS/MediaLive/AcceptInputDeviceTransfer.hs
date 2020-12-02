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
-- Module      : Network.AWS.MediaLive.AcceptInputDeviceTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept an incoming input device transfer. The ownership of the device will transfer to your AWS account.
module Network.AWS.MediaLive.AcceptInputDeviceTransfer
  ( -- * Creating a Request
    acceptInputDeviceTransfer,
    AcceptInputDeviceTransfer,

    -- * Request Lenses
    aidtInputDeviceId,

    -- * Destructuring the Response
    acceptInputDeviceTransferResponse,
    AcceptInputDeviceTransferResponse,

    -- * Response Lenses
    aidtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for AcceptInputDeviceTransferRequest
--
-- /See:/ 'acceptInputDeviceTransfer' smart constructor.
newtype AcceptInputDeviceTransfer = AcceptInputDeviceTransfer'
  { _aidtInputDeviceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AcceptInputDeviceTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aidtInputDeviceId' - The unique ID of the input device to accept. For example, hd-123456789abcdef.
acceptInputDeviceTransfer ::
  -- | 'aidtInputDeviceId'
  Text ->
  AcceptInputDeviceTransfer
acceptInputDeviceTransfer pInputDeviceId_ =
  AcceptInputDeviceTransfer' {_aidtInputDeviceId = pInputDeviceId_}

-- | The unique ID of the input device to accept. For example, hd-123456789abcdef.
aidtInputDeviceId :: Lens' AcceptInputDeviceTransfer Text
aidtInputDeviceId = lens _aidtInputDeviceId (\s a -> s {_aidtInputDeviceId = a})

instance AWSRequest AcceptInputDeviceTransfer where
  type
    Rs AcceptInputDeviceTransfer =
      AcceptInputDeviceTransferResponse
  request = postJSON mediaLive
  response =
    receiveEmpty
      ( \s h x ->
          AcceptInputDeviceTransferResponse' <$> (pure (fromEnum s))
      )

instance Hashable AcceptInputDeviceTransfer

instance NFData AcceptInputDeviceTransfer

instance ToHeaders AcceptInputDeviceTransfer where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON AcceptInputDeviceTransfer where
  toJSON = const (Object mempty)

instance ToPath AcceptInputDeviceTransfer where
  toPath AcceptInputDeviceTransfer' {..} =
    mconcat
      ["/prod/inputDevices/", toBS _aidtInputDeviceId, "/accept"]

instance ToQuery AcceptInputDeviceTransfer where
  toQuery = const mempty

-- | Placeholder documentation for AcceptInputDeviceTransferResponse
--
-- /See:/ 'acceptInputDeviceTransferResponse' smart constructor.
newtype AcceptInputDeviceTransferResponse = AcceptInputDeviceTransferResponse'
  { _aidtrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AcceptInputDeviceTransferResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aidtrsResponseStatus' - -- | The response status code.
acceptInputDeviceTransferResponse ::
  -- | 'aidtrsResponseStatus'
  Int ->
  AcceptInputDeviceTransferResponse
acceptInputDeviceTransferResponse pResponseStatus_ =
  AcceptInputDeviceTransferResponse'
    { _aidtrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
aidtrsResponseStatus :: Lens' AcceptInputDeviceTransferResponse Int
aidtrsResponseStatus = lens _aidtrsResponseStatus (\s a -> s {_aidtrsResponseStatus = a})

instance NFData AcceptInputDeviceTransferResponse
