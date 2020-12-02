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
-- Module      : Network.AWS.MediaLive.ListInputDeviceTransfers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List input devices that are currently being transferred. List input devices that you are transferring from your AWS account or input devices that another AWS account is transferring to you.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputDeviceTransfers
  ( -- * Creating a Request
    listInputDeviceTransfers,
    ListInputDeviceTransfers,

    -- * Request Lenses
    lidtNextToken,
    lidtMaxResults,
    lidtTransferType,

    -- * Destructuring the Response
    listInputDeviceTransfersResponse,
    ListInputDeviceTransfersResponse,

    -- * Response Lenses
    lidtrsNextToken,
    lidtrsInputDeviceTransfers,
    lidtrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for ListInputDeviceTransfersRequest
--
-- /See:/ 'listInputDeviceTransfers' smart constructor.
data ListInputDeviceTransfers = ListInputDeviceTransfers'
  { _lidtNextToken ::
      !(Maybe Text),
    _lidtMaxResults :: !(Maybe Nat),
    _lidtTransferType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListInputDeviceTransfers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lidtNextToken' - Undocumented member.
--
-- * 'lidtMaxResults' - Undocumented member.
--
-- * 'lidtTransferType' - Undocumented member.
listInputDeviceTransfers ::
  -- | 'lidtTransferType'
  Text ->
  ListInputDeviceTransfers
listInputDeviceTransfers pTransferType_ =
  ListInputDeviceTransfers'
    { _lidtNextToken = Nothing,
      _lidtMaxResults = Nothing,
      _lidtTransferType = pTransferType_
    }

-- | Undocumented member.
lidtNextToken :: Lens' ListInputDeviceTransfers (Maybe Text)
lidtNextToken = lens _lidtNextToken (\s a -> s {_lidtNextToken = a})

-- | Undocumented member.
lidtMaxResults :: Lens' ListInputDeviceTransfers (Maybe Natural)
lidtMaxResults = lens _lidtMaxResults (\s a -> s {_lidtMaxResults = a}) . mapping _Nat

-- | Undocumented member.
lidtTransferType :: Lens' ListInputDeviceTransfers Text
lidtTransferType = lens _lidtTransferType (\s a -> s {_lidtTransferType = a})

instance AWSPager ListInputDeviceTransfers where
  page rq rs
    | stop (rs ^. lidtrsNextToken) = Nothing
    | stop (rs ^. lidtrsInputDeviceTransfers) = Nothing
    | otherwise = Just $ rq & lidtNextToken .~ rs ^. lidtrsNextToken

instance AWSRequest ListInputDeviceTransfers where
  type Rs ListInputDeviceTransfers = ListInputDeviceTransfersResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          ListInputDeviceTransfersResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "inputDeviceTransfers" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListInputDeviceTransfers

instance NFData ListInputDeviceTransfers

instance ToHeaders ListInputDeviceTransfers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListInputDeviceTransfers where
  toPath = const "/prod/inputDeviceTransfers"

instance ToQuery ListInputDeviceTransfers where
  toQuery ListInputDeviceTransfers' {..} =
    mconcat
      [ "nextToken" =: _lidtNextToken,
        "maxResults" =: _lidtMaxResults,
        "transferType" =: _lidtTransferType
      ]

-- | Placeholder documentation for ListInputDeviceTransfersResponse
--
-- /See:/ 'listInputDeviceTransfersResponse' smart constructor.
data ListInputDeviceTransfersResponse = ListInputDeviceTransfersResponse'
  { _lidtrsNextToken ::
      !(Maybe Text),
    _lidtrsInputDeviceTransfers ::
      !( Maybe
           [TransferringInputDeviceSummary]
       ),
    _lidtrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListInputDeviceTransfersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lidtrsNextToken' - A token to get additional list results.
--
-- * 'lidtrsInputDeviceTransfers' - The list of devices that you are transferring or are being transferred to you.
--
-- * 'lidtrsResponseStatus' - -- | The response status code.
listInputDeviceTransfersResponse ::
  -- | 'lidtrsResponseStatus'
  Int ->
  ListInputDeviceTransfersResponse
listInputDeviceTransfersResponse pResponseStatus_ =
  ListInputDeviceTransfersResponse'
    { _lidtrsNextToken = Nothing,
      _lidtrsInputDeviceTransfers = Nothing,
      _lidtrsResponseStatus = pResponseStatus_
    }

-- | A token to get additional list results.
lidtrsNextToken :: Lens' ListInputDeviceTransfersResponse (Maybe Text)
lidtrsNextToken = lens _lidtrsNextToken (\s a -> s {_lidtrsNextToken = a})

-- | The list of devices that you are transferring or are being transferred to you.
lidtrsInputDeviceTransfers :: Lens' ListInputDeviceTransfersResponse [TransferringInputDeviceSummary]
lidtrsInputDeviceTransfers = lens _lidtrsInputDeviceTransfers (\s a -> s {_lidtrsInputDeviceTransfers = a}) . _Default . _Coerce

-- | -- | The response status code.
lidtrsResponseStatus :: Lens' ListInputDeviceTransfersResponse Int
lidtrsResponseStatus = lens _lidtrsResponseStatus (\s a -> s {_lidtrsResponseStatus = a})

instance NFData ListInputDeviceTransfersResponse
