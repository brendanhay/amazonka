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
-- Module      : Network.AWS.FMS.GetProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager protocols list.
module Network.AWS.FMS.GetProtocolsList
  ( -- * Creating a Request
    getProtocolsList,
    GetProtocolsList,

    -- * Request Lenses
    gplDefaultList,
    gplListId,

    -- * Destructuring the Response
    getProtocolsListResponse,
    GetProtocolsListResponse,

    -- * Response Lenses
    gplrsProtocolsList,
    gplrsProtocolsListARN,
    gplrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getProtocolsList' smart constructor.
data GetProtocolsList = GetProtocolsList'
  { _gplDefaultList ::
      !(Maybe Bool),
    _gplListId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetProtocolsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gplDefaultList' - Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
--
-- * 'gplListId' - The ID of the AWS Firewall Manager protocols list that you want the details for.
getProtocolsList ::
  -- | 'gplListId'
  Text ->
  GetProtocolsList
getProtocolsList pListId_ =
  GetProtocolsList'
    { _gplDefaultList = Nothing,
      _gplListId = pListId_
    }

-- | Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
gplDefaultList :: Lens' GetProtocolsList (Maybe Bool)
gplDefaultList = lens _gplDefaultList (\s a -> s {_gplDefaultList = a})

-- | The ID of the AWS Firewall Manager protocols list that you want the details for.
gplListId :: Lens' GetProtocolsList Text
gplListId = lens _gplListId (\s a -> s {_gplListId = a})

instance AWSRequest GetProtocolsList where
  type Rs GetProtocolsList = GetProtocolsListResponse
  request = postJSON fms
  response =
    receiveJSON
      ( \s h x ->
          GetProtocolsListResponse'
            <$> (x .?> "ProtocolsList")
            <*> (x .?> "ProtocolsListArn")
            <*> (pure (fromEnum s))
      )

instance Hashable GetProtocolsList

instance NFData GetProtocolsList

instance ToHeaders GetProtocolsList where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSFMS_20180101.GetProtocolsList" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetProtocolsList where
  toJSON GetProtocolsList' {..} =
    object
      ( catMaybes
          [ ("DefaultList" .=) <$> _gplDefaultList,
            Just ("ListId" .= _gplListId)
          ]
      )

instance ToPath GetProtocolsList where
  toPath = const "/"

instance ToQuery GetProtocolsList where
  toQuery = const mempty

-- | /See:/ 'getProtocolsListResponse' smart constructor.
data GetProtocolsListResponse = GetProtocolsListResponse'
  { _gplrsProtocolsList ::
      !(Maybe ProtocolsListData),
    _gplrsProtocolsListARN :: !(Maybe Text),
    _gplrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetProtocolsListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gplrsProtocolsList' - Information about the specified AWS Firewall Manager protocols list.
--
-- * 'gplrsProtocolsListARN' - The Amazon Resource Name (ARN) of the specified protocols list.
--
-- * 'gplrsResponseStatus' - -- | The response status code.
getProtocolsListResponse ::
  -- | 'gplrsResponseStatus'
  Int ->
  GetProtocolsListResponse
getProtocolsListResponse pResponseStatus_ =
  GetProtocolsListResponse'
    { _gplrsProtocolsList = Nothing,
      _gplrsProtocolsListARN = Nothing,
      _gplrsResponseStatus = pResponseStatus_
    }

-- | Information about the specified AWS Firewall Manager protocols list.
gplrsProtocolsList :: Lens' GetProtocolsListResponse (Maybe ProtocolsListData)
gplrsProtocolsList = lens _gplrsProtocolsList (\s a -> s {_gplrsProtocolsList = a})

-- | The Amazon Resource Name (ARN) of the specified protocols list.
gplrsProtocolsListARN :: Lens' GetProtocolsListResponse (Maybe Text)
gplrsProtocolsListARN = lens _gplrsProtocolsListARN (\s a -> s {_gplrsProtocolsListARN = a})

-- | -- | The response status code.
gplrsResponseStatus :: Lens' GetProtocolsListResponse Int
gplrsResponseStatus = lens _gplrsResponseStatus (\s a -> s {_gplrsResponseStatus = a})

instance NFData GetProtocolsListResponse
