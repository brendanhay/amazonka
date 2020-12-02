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
-- Module      : Network.AWS.AppStream.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user in the user pool.
module Network.AWS.AppStream.CreateUser
  ( -- * Creating a Request
    createUser,
    CreateUser,

    -- * Request Lenses
    cuLastName,
    cuMessageAction,
    cuFirstName,
    cuUserName,
    cuAuthenticationType,

    -- * Destructuring the Response
    createUserResponse,
    CreateUserResponse,

    -- * Response Lenses
    cursResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUser' smart constructor.
data CreateUser = CreateUser'
  { _cuLastName ::
      !(Maybe (Sensitive Text)),
    _cuMessageAction :: !(Maybe MessageAction),
    _cuFirstName :: !(Maybe (Sensitive Text)),
    _cuUserName :: !(Sensitive Text),
    _cuAuthenticationType :: !AuthenticationType
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuLastName' - The last name, or surname, of the user.
--
-- * 'cuMessageAction' - The action to take for the welcome email that is sent to a user after the user is created in the user pool. If you specify SUPPRESS, no email is sent. If you specify RESEND, do not specify the first name or last name of the user. If the value is null, the email is sent.
--
-- * 'cuFirstName' - The first name, or given name, of the user.
--
-- * 'cuUserName' - The email address of the user.
--
-- * 'cuAuthenticationType' - The authentication type for the user. You must specify USERPOOL.
createUser ::
  -- | 'cuUserName'
  Text ->
  -- | 'cuAuthenticationType'
  AuthenticationType ->
  CreateUser
createUser pUserName_ pAuthenticationType_ =
  CreateUser'
    { _cuLastName = Nothing,
      _cuMessageAction = Nothing,
      _cuFirstName = Nothing,
      _cuUserName = _Sensitive # pUserName_,
      _cuAuthenticationType = pAuthenticationType_
    }

-- | The last name, or surname, of the user.
cuLastName :: Lens' CreateUser (Maybe Text)
cuLastName = lens _cuLastName (\s a -> s {_cuLastName = a}) . mapping _Sensitive

-- | The action to take for the welcome email that is sent to a user after the user is created in the user pool. If you specify SUPPRESS, no email is sent. If you specify RESEND, do not specify the first name or last name of the user. If the value is null, the email is sent.
cuMessageAction :: Lens' CreateUser (Maybe MessageAction)
cuMessageAction = lens _cuMessageAction (\s a -> s {_cuMessageAction = a})

-- | The first name, or given name, of the user.
cuFirstName :: Lens' CreateUser (Maybe Text)
cuFirstName = lens _cuFirstName (\s a -> s {_cuFirstName = a}) . mapping _Sensitive

-- | The email address of the user.
cuUserName :: Lens' CreateUser Text
cuUserName = lens _cuUserName (\s a -> s {_cuUserName = a}) . _Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
cuAuthenticationType :: Lens' CreateUser AuthenticationType
cuAuthenticationType = lens _cuAuthenticationType (\s a -> s {_cuAuthenticationType = a})

instance AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = postJSON appStream
  response =
    receiveEmpty
      (\s h x -> CreateUserResponse' <$> (pure (fromEnum s)))

instance Hashable CreateUser

instance NFData CreateUser

instance ToHeaders CreateUser where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.CreateUser" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateUser where
  toJSON CreateUser' {..} =
    object
      ( catMaybes
          [ ("LastName" .=) <$> _cuLastName,
            ("MessageAction" .=) <$> _cuMessageAction,
            ("FirstName" .=) <$> _cuFirstName,
            Just ("UserName" .= _cuUserName),
            Just ("AuthenticationType" .= _cuAuthenticationType)
          ]
      )

instance ToPath CreateUser where
  toPath = const "/"

instance ToQuery CreateUser where
  toQuery = const mempty

-- | /See:/ 'createUserResponse' smart constructor.
newtype CreateUserResponse = CreateUserResponse'
  { _cursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cursResponseStatus' - -- | The response status code.
createUserResponse ::
  -- | 'cursResponseStatus'
  Int ->
  CreateUserResponse
createUserResponse pResponseStatus_ =
  CreateUserResponse' {_cursResponseStatus = pResponseStatus_}

-- | -- | The response status code.
cursResponseStatus :: Lens' CreateUserResponse Int
cursResponseStatus = lens _cursResponseStatus (\s a -> s {_cursResponseStatus = a})

instance NFData CreateUserResponse
