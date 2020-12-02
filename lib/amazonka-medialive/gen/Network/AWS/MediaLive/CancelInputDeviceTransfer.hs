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
-- Module      : Network.AWS.MediaLive.CancelInputDeviceTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancel an input device transfer that you have requested.
module Network.AWS.MediaLive.CancelInputDeviceTransfer
  ( -- * Creating a Request
    cancelInputDeviceTransfer,
    CancelInputDeviceTransfer,

    -- * Request Lenses
    cidtInputDeviceId,

    -- * Destructuring the Response
    cancelInputDeviceTransferResponse,
    CancelInputDeviceTransferResponse,

    -- * Response Lenses
    cidtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for CancelInputDeviceTransferRequest
--
-- /See:/ 'cancelInputDeviceTransfer' smart constructor.
newtype CancelInputDeviceTransfer = CancelInputDeviceTransfer'
  { _cidtInputDeviceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelInputDeviceTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cidtInputDeviceId' - The unique ID of the input device to cancel. For example, hd-123456789abcdef.
cancelInputDeviceTransfer ::
  -- | 'cidtInputDeviceId'
  Text ->
  CancelInputDeviceTransfer
cancelInputDeviceTransfer pInputDeviceId_ =
  CancelInputDeviceTransfer' {_cidtInputDeviceId = pInputDeviceId_}

-- | The unique ID of the input device to cancel. For example, hd-123456789abcdef.
cidtInputDeviceId :: Lens' CancelInputDeviceTransfer Text
cidtInputDeviceId = lens _cidtInputDeviceId (\s a -> s {_cidtInputDeviceId = a})

instance AWSRequest CancelInputDeviceTransfer where
  type
    Rs CancelInputDeviceTransfer =
      CancelInputDeviceTransferResponse
  request = postJSON mediaLive
  response =
    receiveEmpty
      ( \s h x ->
          CancelInputDeviceTransferResponse' <$> (pure (fromEnum s))
      )

instance Hashable CancelInputDeviceTransfer

instance NFData CancelInputDeviceTransfer

instance ToHeaders CancelInputDeviceTransfer where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CancelInputDeviceTransfer where
  toJSON = const (Object mempty)

instance ToPath CancelInputDeviceTransfer where
  toPath CancelInputDeviceTransfer' {..} =
    mconcat
      ["/prod/inputDevices/", toBS _cidtInputDeviceId, "/cancel"]

instance ToQuery CancelInputDeviceTransfer where
  toQuery = const mempty

-- | Placeholder documentation for CancelInputDeviceTransferResponse
--
-- /See:/ 'cancelInputDeviceTransferResponse' smart constructor.
newtype CancelInputDeviceTransferResponse = CancelInputDeviceTransferResponse'
  { _cidtrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelInputDeviceTransferResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cidtrsResponseStatus' - -- | The response status code.
cancelInputDeviceTransferResponse ::
  -- | 'cidtrsResponseStatus'
  Int ->
  CancelInputDeviceTransferResponse
cancelInputDeviceTransferResponse pResponseStatus_ =
  CancelInputDeviceTransferResponse'
    { _cidtrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
cidtrsResponseStatus :: Lens' CancelInputDeviceTransferResponse Int
cidtrsResponseStatus = lens _cidtrsResponseStatus (\s a -> s {_cidtrsResponseStatus = a})

instance NFData CancelInputDeviceTransferResponse
