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
-- Module      : Network.AWS.Connect.DescribeUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified user account. You can find the instance ID in the console (it’s the final part of the ARN). The console does not display the user IDs. Instead, list the users and note the IDs provided in the output.
module Network.AWS.Connect.DescribeUser
  ( -- * Creating a Request
    describeUser,
    DescribeUser,

    -- * Request Lenses
    duUserId,
    duInstanceId,

    -- * Destructuring the Response
    describeUserResponse,
    DescribeUserResponse,

    -- * Response Lenses
    dursUser,
    dursResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUser' smart constructor.
data DescribeUser = DescribeUser'
  { _duUserId :: !Text,
    _duInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duUserId' - The identifier of the user account.
--
-- * 'duInstanceId' - The identifier of the Amazon Connect instance.
describeUser ::
  -- | 'duUserId'
  Text ->
  -- | 'duInstanceId'
  Text ->
  DescribeUser
describeUser pUserId_ pInstanceId_ =
  DescribeUser' {_duUserId = pUserId_, _duInstanceId = pInstanceId_}

-- | The identifier of the user account.
duUserId :: Lens' DescribeUser Text
duUserId = lens _duUserId (\s a -> s {_duUserId = a})

-- | The identifier of the Amazon Connect instance.
duInstanceId :: Lens' DescribeUser Text
duInstanceId = lens _duInstanceId (\s a -> s {_duInstanceId = a})

instance AWSRequest DescribeUser where
  type Rs DescribeUser = DescribeUserResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          DescribeUserResponse' <$> (x .?> "User") <*> (pure (fromEnum s))
      )

instance Hashable DescribeUser

instance NFData DescribeUser

instance ToHeaders DescribeUser where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeUser where
  toPath DescribeUser' {..} =
    mconcat ["/users/", toBS _duInstanceId, "/", toBS _duUserId]

instance ToQuery DescribeUser where
  toQuery = const mempty

-- | /See:/ 'describeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { _dursUser ::
      !(Maybe User),
    _dursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursUser' - Information about the user account and configuration settings.
--
-- * 'dursResponseStatus' - -- | The response status code.
describeUserResponse ::
  -- | 'dursResponseStatus'
  Int ->
  DescribeUserResponse
describeUserResponse pResponseStatus_ =
  DescribeUserResponse'
    { _dursUser = Nothing,
      _dursResponseStatus = pResponseStatus_
    }

-- | Information about the user account and configuration settings.
dursUser :: Lens' DescribeUserResponse (Maybe User)
dursUser = lens _dursUser (\s a -> s {_dursUser = a})

-- | -- | The response status code.
dursResponseStatus :: Lens' DescribeUserResponse Int
dursResponseStatus = lens _dursResponseStatus (\s a -> s {_dursResponseStatus = a})

instance NFData DescribeUserResponse
