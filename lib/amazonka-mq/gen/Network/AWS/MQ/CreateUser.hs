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
-- Module      : Network.AWS.MQ.CreateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an ActiveMQ user.
module Network.AWS.MQ.CreateUser
    (
    -- * Creating a Request
      createUser
    , CreateUser
    -- * Request Lenses
    , cuGroups
    , cuConsoleAccess
    , cuPassword
    , cuUsername
    , cuBrokerId

    -- * Destructuring the Response
    , createUserResponse
    , CreateUserResponse
    -- * Response Lenses
    , cursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Creates a new ActiveMQ user.
--
-- /See:/ 'createUser' smart constructor.
data CreateUser = CreateUser'
  { _cuGroups        :: !(Maybe [Text])
  , _cuConsoleAccess :: !(Maybe Bool)
  , _cuPassword      :: !(Maybe Text)
  , _cuUsername      :: !Text
  , _cuBrokerId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuGroups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'cuConsoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- * 'cuPassword' - Required. The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
--
-- * 'cuUsername' - The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'cuBrokerId' - The unique ID that Amazon MQ generates for the broker.
createUser
    :: Text -- ^ 'cuUsername'
    -> Text -- ^ 'cuBrokerId'
    -> CreateUser
createUser pUsername_ pBrokerId_ =
  CreateUser'
    { _cuGroups = Nothing
    , _cuConsoleAccess = Nothing
    , _cuPassword = Nothing
    , _cuUsername = pUsername_
    , _cuBrokerId = pBrokerId_
    }


-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
cuGroups :: Lens' CreateUser [Text]
cuGroups = lens _cuGroups (\ s a -> s{_cuGroups = a}) . _Default . _Coerce

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
cuConsoleAccess :: Lens' CreateUser (Maybe Bool)
cuConsoleAccess = lens _cuConsoleAccess (\ s a -> s{_cuConsoleAccess = a})

-- | Required. The password of the user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
cuPassword :: Lens' CreateUser (Maybe Text)
cuPassword = lens _cuPassword (\ s a -> s{_cuPassword = a})

-- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
cuUsername :: Lens' CreateUser Text
cuUsername = lens _cuUsername (\ s a -> s{_cuUsername = a})

-- | The unique ID that Amazon MQ generates for the broker.
cuBrokerId :: Lens' CreateUser Text
cuBrokerId = lens _cuBrokerId (\ s a -> s{_cuBrokerId = a})

instance AWSRequest CreateUser where
        type Rs CreateUser = CreateUserResponse
        request = postJSON mq
        response
          = receiveEmpty
              (\ s h x ->
                 CreateUserResponse' <$> (pure (fromEnum s)))

instance Hashable CreateUser where

instance NFData CreateUser where

instance ToHeaders CreateUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUser where
        toJSON CreateUser'{..}
          = object
              (catMaybes
                 [("groups" .=) <$> _cuGroups,
                  ("consoleAccess" .=) <$> _cuConsoleAccess,
                  ("password" .=) <$> _cuPassword])

instance ToPath CreateUser where
        toPath CreateUser'{..}
          = mconcat
              ["/v1/brokers/", toBS _cuBrokerId, "/users/",
               toBS _cuUsername]

instance ToQuery CreateUser where
        toQuery = const mempty

-- | /See:/ 'createUserResponse' smart constructor.
newtype CreateUserResponse = CreateUserResponse'
  { _cursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cursResponseStatus' - -- | The response status code.
createUserResponse
    :: Int -- ^ 'cursResponseStatus'
    -> CreateUserResponse
createUserResponse pResponseStatus_ =
  CreateUserResponse' {_cursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cursResponseStatus :: Lens' CreateUserResponse Int
cursResponseStatus = lens _cursResponseStatus (\ s a -> s{_cursResponseStatus = a})

instance NFData CreateUserResponse where
