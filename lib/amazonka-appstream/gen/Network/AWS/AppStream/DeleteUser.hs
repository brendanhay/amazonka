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
-- Module      : Network.AWS.AppStream.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from the user pool.
module Network.AWS.AppStream.DeleteUser
  ( -- * Creating a Request
    deleteUser,
    DeleteUser,

    -- * Request Lenses
    delUserName,
    delAuthenticationType,

    -- * Destructuring the Response
    deleteUserResponse,
    DeleteUserResponse,

    -- * Response Lenses
    delrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { _delUserName :: !(Sensitive Text),
    _delAuthenticationType :: !AuthenticationType
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delUserName' - The email address of the user.
--
-- * 'delAuthenticationType' - The authentication type for the user. You must specify USERPOOL.
deleteUser ::
  -- | 'delUserName'
  Text ->
  -- | 'delAuthenticationType'
  AuthenticationType ->
  DeleteUser
deleteUser pUserName_ pAuthenticationType_ =
  DeleteUser'
    { _delUserName = _Sensitive # pUserName_,
      _delAuthenticationType = pAuthenticationType_
    }

-- | The email address of the user.
delUserName :: Lens' DeleteUser Text
delUserName = lens _delUserName (\s a -> s {_delUserName = a}) . _Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
delAuthenticationType :: Lens' DeleteUser AuthenticationType
delAuthenticationType = lens _delAuthenticationType (\s a -> s {_delAuthenticationType = a})

instance AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = postJSON appStream
  response =
    receiveEmpty
      (\s h x -> DeleteUserResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteUser

instance NFData DeleteUser

instance ToHeaders DeleteUser where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.DeleteUser" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    object
      ( catMaybes
          [ Just ("UserName" .= _delUserName),
            Just ("AuthenticationType" .= _delAuthenticationType)
          ]
      )

instance ToPath DeleteUser where
  toPath = const "/"

instance ToQuery DeleteUser where
  toQuery = const mempty

-- | /See:/ 'deleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { _delrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteUserResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteUserResponse
deleteUserResponse pResponseStatus_ =
  DeleteUserResponse' {_delrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteUserResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteUserResponse
