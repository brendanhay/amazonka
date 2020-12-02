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
-- Module      : Network.AWS.FMS.PutAppsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager applications list.
module Network.AWS.FMS.PutAppsList
  ( -- * Creating a Request
    putAppsList,
    PutAppsList,

    -- * Request Lenses
    palTagList,
    palAppsList,

    -- * Destructuring the Response
    putAppsListResponse,
    PutAppsListResponse,

    -- * Response Lenses
    palrsAppsListARN,
    palrsAppsList,
    palrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putAppsList' smart constructor.
data PutAppsList = PutAppsList'
  { _palTagList :: !(Maybe [Tag]),
    _palAppsList :: !AppsListData
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutAppsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'palTagList' - The tags associated with the resource.
--
-- * 'palAppsList' - The details of the AWS Firewall Manager applications list to be created.
putAppsList ::
  -- | 'palAppsList'
  AppsListData ->
  PutAppsList
putAppsList pAppsList_ =
  PutAppsList' {_palTagList = Nothing, _palAppsList = pAppsList_}

-- | The tags associated with the resource.
palTagList :: Lens' PutAppsList [Tag]
palTagList = lens _palTagList (\s a -> s {_palTagList = a}) . _Default . _Coerce

-- | The details of the AWS Firewall Manager applications list to be created.
palAppsList :: Lens' PutAppsList AppsListData
palAppsList = lens _palAppsList (\s a -> s {_palAppsList = a})

instance AWSRequest PutAppsList where
  type Rs PutAppsList = PutAppsListResponse
  request = postJSON fms
  response =
    receiveJSON
      ( \s h x ->
          PutAppsListResponse'
            <$> (x .?> "AppsListArn")
            <*> (x .?> "AppsList")
            <*> (pure (fromEnum s))
      )

instance Hashable PutAppsList

instance NFData PutAppsList

instance ToHeaders PutAppsList where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSFMS_20180101.PutAppsList" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutAppsList where
  toJSON PutAppsList' {..} =
    object
      ( catMaybes
          [ ("TagList" .=) <$> _palTagList,
            Just ("AppsList" .= _palAppsList)
          ]
      )

instance ToPath PutAppsList where
  toPath = const "/"

instance ToQuery PutAppsList where
  toQuery = const mempty

-- | /See:/ 'putAppsListResponse' smart constructor.
data PutAppsListResponse = PutAppsListResponse'
  { _palrsAppsListARN ::
      !(Maybe Text),
    _palrsAppsList :: !(Maybe AppsListData),
    _palrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutAppsListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'palrsAppsListARN' - The Amazon Resource Name (ARN) of the applications list.
--
-- * 'palrsAppsList' - The details of the AWS Firewall Manager applications list.
--
-- * 'palrsResponseStatus' - -- | The response status code.
putAppsListResponse ::
  -- | 'palrsResponseStatus'
  Int ->
  PutAppsListResponse
putAppsListResponse pResponseStatus_ =
  PutAppsListResponse'
    { _palrsAppsListARN = Nothing,
      _palrsAppsList = Nothing,
      _palrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the applications list.
palrsAppsListARN :: Lens' PutAppsListResponse (Maybe Text)
palrsAppsListARN = lens _palrsAppsListARN (\s a -> s {_palrsAppsListARN = a})

-- | The details of the AWS Firewall Manager applications list.
palrsAppsList :: Lens' PutAppsListResponse (Maybe AppsListData)
palrsAppsList = lens _palrsAppsList (\s a -> s {_palrsAppsList = a})

-- | -- | The response status code.
palrsResponseStatus :: Lens' PutAppsListResponse Int
palrsResponseStatus = lens _palrsResponseStatus (\s a -> s {_palrsResponseStatus = a})

instance NFData PutAppsListResponse
