{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @User@ object that contains information about the user account specified by the @UserId@ .
--
--
module Network.AWS.Connect.DescribeUser
    (
    -- * Creating a Request
      describeUser
    , DescribeUser
    -- * Request Lenses
    , duUserId
    , duInstanceId

    -- * Destructuring the Response
    , describeUserResponse
    , DescribeUserResponse
    -- * Response Lenses
    , dursUser
    , dursResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUser' smart constructor.
data DescribeUser = DescribeUser'
  { _duUserId     :: !Text
  , _duInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duUserId' - Unique identifier for the user account to return.
--
-- * 'duInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
describeUser
    :: Text -- ^ 'duUserId'
    -> Text -- ^ 'duInstanceId'
    -> DescribeUser
describeUser pUserId_ pInstanceId_ =
  DescribeUser' {_duUserId = pUserId_, _duInstanceId = pInstanceId_}


-- | Unique identifier for the user account to return.
duUserId :: Lens' DescribeUser Text
duUserId = lens _duUserId (\ s a -> s{_duUserId = a})

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
duInstanceId :: Lens' DescribeUser Text
duInstanceId = lens _duInstanceId (\ s a -> s{_duInstanceId = a})

instance AWSRequest DescribeUser where
        type Rs DescribeUser = DescribeUserResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable DescribeUser where

instance NFData DescribeUser where

instance ToHeaders DescribeUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeUser where
        toPath DescribeUser'{..}
          = mconcat
              ["/users/", toBS _duInstanceId, "/", toBS _duUserId]

instance ToQuery DescribeUser where
        toQuery = const mempty

-- | /See:/ 'describeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { _dursUser           :: !(Maybe User)
  , _dursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursUser' - A @User@ object that contains information about the user account and configuration settings.
--
-- * 'dursResponseStatus' - -- | The response status code.
describeUserResponse
    :: Int -- ^ 'dursResponseStatus'
    -> DescribeUserResponse
describeUserResponse pResponseStatus_ =
  DescribeUserResponse'
    {_dursUser = Nothing, _dursResponseStatus = pResponseStatus_}


-- | A @User@ object that contains information about the user account and configuration settings.
dursUser :: Lens' DescribeUserResponse (Maybe User)
dursUser = lens _dursUser (\ s a -> s{_dursUser = a})

-- | -- | The response status code.
dursResponseStatus :: Lens' DescribeUserResponse Int
dursResponseStatus = lens _dursResponseStatus (\ s a -> s{_dursResponseStatus = a})

instance NFData DescribeUserResponse where
