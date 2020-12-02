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
-- Module      : Network.AWS.MediaLive.TransferInputDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start an input device transfer to another AWS account. After you make the request, the other account must accept or reject the transfer.
module Network.AWS.MediaLive.TransferInputDevice
  ( -- * Creating a Request
    transferInputDevice,
    TransferInputDevice,

    -- * Request Lenses
    tidTransferMessage,
    tidTargetCustomerId,
    tidInputDeviceId,

    -- * Destructuring the Response
    transferInputDeviceResponse,
    TransferInputDeviceResponse,

    -- * Response Lenses
    tidrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to transfer an input device.
--
-- /See:/ 'transferInputDevice' smart constructor.
data TransferInputDevice = TransferInputDevice'
  { _tidTransferMessage ::
      !(Maybe Text),
    _tidTargetCustomerId :: !(Maybe Text),
    _tidInputDeviceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransferInputDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tidTransferMessage' - An optional message for the recipient. Maximum 280 characters.
--
-- * 'tidTargetCustomerId' - The AWS account ID (12 digits) for the recipient of the device transfer.
--
-- * 'tidInputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
transferInputDevice ::
  -- | 'tidInputDeviceId'
  Text ->
  TransferInputDevice
transferInputDevice pInputDeviceId_ =
  TransferInputDevice'
    { _tidTransferMessage = Nothing,
      _tidTargetCustomerId = Nothing,
      _tidInputDeviceId = pInputDeviceId_
    }

-- | An optional message for the recipient. Maximum 280 characters.
tidTransferMessage :: Lens' TransferInputDevice (Maybe Text)
tidTransferMessage = lens _tidTransferMessage (\s a -> s {_tidTransferMessage = a})

-- | The AWS account ID (12 digits) for the recipient of the device transfer.
tidTargetCustomerId :: Lens' TransferInputDevice (Maybe Text)
tidTargetCustomerId = lens _tidTargetCustomerId (\s a -> s {_tidTargetCustomerId = a})

-- | The unique ID of this input device. For example, hd-123456789abcdef.
tidInputDeviceId :: Lens' TransferInputDevice Text
tidInputDeviceId = lens _tidInputDeviceId (\s a -> s {_tidInputDeviceId = a})

instance AWSRequest TransferInputDevice where
  type Rs TransferInputDevice = TransferInputDeviceResponse
  request = postJSON mediaLive
  response =
    receiveEmpty
      (\s h x -> TransferInputDeviceResponse' <$> (pure (fromEnum s)))

instance Hashable TransferInputDevice

instance NFData TransferInputDevice

instance ToHeaders TransferInputDevice where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON TransferInputDevice where
  toJSON TransferInputDevice' {..} =
    object
      ( catMaybes
          [ ("transferMessage" .=) <$> _tidTransferMessage,
            ("targetCustomerId" .=) <$> _tidTargetCustomerId
          ]
      )

instance ToPath TransferInputDevice where
  toPath TransferInputDevice' {..} =
    mconcat
      ["/prod/inputDevices/", toBS _tidInputDeviceId, "/transfer"]

instance ToQuery TransferInputDevice where
  toQuery = const mempty

-- | Placeholder documentation for TransferInputDeviceResponse
--
-- /See:/ 'transferInputDeviceResponse' smart constructor.
newtype TransferInputDeviceResponse = TransferInputDeviceResponse'
  { _tidrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransferInputDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tidrsResponseStatus' - -- | The response status code.
transferInputDeviceResponse ::
  -- | 'tidrsResponseStatus'
  Int ->
  TransferInputDeviceResponse
transferInputDeviceResponse pResponseStatus_ =
  TransferInputDeviceResponse'
    { _tidrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
tidrsResponseStatus :: Lens' TransferInputDeviceResponse Int
tidrsResponseStatus = lens _tidrsResponseStatus (\s a -> s {_tidrsResponseStatus = a})

instance NFData TransferInputDeviceResponse
