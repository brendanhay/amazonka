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
-- Module      : Network.AWS.FMS.PutProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager protocols list.
module Network.AWS.FMS.PutProtocolsList
  ( -- * Creating a Request
    putProtocolsList,
    PutProtocolsList,

    -- * Request Lenses
    pplTagList,
    pplProtocolsList,

    -- * Destructuring the Response
    putProtocolsListResponse,
    PutProtocolsListResponse,

    -- * Response Lenses
    pplrsProtocolsList,
    pplrsProtocolsListARN,
    pplrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putProtocolsList' smart constructor.
data PutProtocolsList = PutProtocolsList'
  { _pplTagList ::
      !(Maybe [Tag]),
    _pplProtocolsList :: !ProtocolsListData
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutProtocolsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pplTagList' - The tags associated with the resource.
--
-- * 'pplProtocolsList' - The details of the AWS Firewall Manager protocols list to be created.
putProtocolsList ::
  -- | 'pplProtocolsList'
  ProtocolsListData ->
  PutProtocolsList
putProtocolsList pProtocolsList_ =
  PutProtocolsList'
    { _pplTagList = Nothing,
      _pplProtocolsList = pProtocolsList_
    }

-- | The tags associated with the resource.
pplTagList :: Lens' PutProtocolsList [Tag]
pplTagList = lens _pplTagList (\s a -> s {_pplTagList = a}) . _Default . _Coerce

-- | The details of the AWS Firewall Manager protocols list to be created.
pplProtocolsList :: Lens' PutProtocolsList ProtocolsListData
pplProtocolsList = lens _pplProtocolsList (\s a -> s {_pplProtocolsList = a})

instance AWSRequest PutProtocolsList where
  type Rs PutProtocolsList = PutProtocolsListResponse
  request = postJSON fms
  response =
    receiveJSON
      ( \s h x ->
          PutProtocolsListResponse'
            <$> (x .?> "ProtocolsList")
            <*> (x .?> "ProtocolsListArn")
            <*> (pure (fromEnum s))
      )

instance Hashable PutProtocolsList

instance NFData PutProtocolsList

instance ToHeaders PutProtocolsList where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSFMS_20180101.PutProtocolsList" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutProtocolsList where
  toJSON PutProtocolsList' {..} =
    object
      ( catMaybes
          [ ("TagList" .=) <$> _pplTagList,
            Just ("ProtocolsList" .= _pplProtocolsList)
          ]
      )

instance ToPath PutProtocolsList where
  toPath = const "/"

instance ToQuery PutProtocolsList where
  toQuery = const mempty

-- | /See:/ 'putProtocolsListResponse' smart constructor.
data PutProtocolsListResponse = PutProtocolsListResponse'
  { _pplrsProtocolsList ::
      !(Maybe ProtocolsListData),
    _pplrsProtocolsListARN :: !(Maybe Text),
    _pplrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutProtocolsListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pplrsProtocolsList' - The details of the AWS Firewall Manager protocols list.
--
-- * 'pplrsProtocolsListARN' - The Amazon Resource Name (ARN) of the protocols list.
--
-- * 'pplrsResponseStatus' - -- | The response status code.
putProtocolsListResponse ::
  -- | 'pplrsResponseStatus'
  Int ->
  PutProtocolsListResponse
putProtocolsListResponse pResponseStatus_ =
  PutProtocolsListResponse'
    { _pplrsProtocolsList = Nothing,
      _pplrsProtocolsListARN = Nothing,
      _pplrsResponseStatus = pResponseStatus_
    }

-- | The details of the AWS Firewall Manager protocols list.
pplrsProtocolsList :: Lens' PutProtocolsListResponse (Maybe ProtocolsListData)
pplrsProtocolsList = lens _pplrsProtocolsList (\s a -> s {_pplrsProtocolsList = a})

-- | The Amazon Resource Name (ARN) of the protocols list.
pplrsProtocolsListARN :: Lens' PutProtocolsListResponse (Maybe Text)
pplrsProtocolsListARN = lens _pplrsProtocolsListARN (\s a -> s {_pplrsProtocolsListARN = a})

-- | -- | The response status code.
pplrsResponseStatus :: Lens' PutProtocolsListResponse Int
pplrsResponseStatus = lens _pplrsResponseStatus (\s a -> s {_pplrsResponseStatus = a})

instance NFData PutProtocolsListResponse
