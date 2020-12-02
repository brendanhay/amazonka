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
-- Module      : Network.AWS.Connect.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user account from the specified Amazon Connect instance.
--
--
-- For information about what happens to a user's data when their account is deleted, see <https://docs.aws.amazon.com/connect/latest/adminguide/delete-users.html Delete Users from Your Amazon Connect Instance> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.DeleteUser
  ( -- * Creating a Request
    deleteUser,
    DeleteUser,

    -- * Request Lenses
    delInstanceId,
    delUserId,

    -- * Destructuring the Response
    deleteUserResponse,
    DeleteUserResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { _delInstanceId :: !Text,
    _delUserId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'delUserId' - The identifier of the user.
deleteUser ::
  -- | 'delInstanceId'
  Text ->
  -- | 'delUserId'
  Text ->
  DeleteUser
deleteUser pInstanceId_ pUserId_ =
  DeleteUser' {_delInstanceId = pInstanceId_, _delUserId = pUserId_}

-- | The identifier of the Amazon Connect instance.
delInstanceId :: Lens' DeleteUser Text
delInstanceId = lens _delInstanceId (\s a -> s {_delInstanceId = a})

-- | The identifier of the user.
delUserId :: Lens' DeleteUser Text
delUserId = lens _delUserId (\s a -> s {_delUserId = a})

instance AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = delete connect
  response = receiveNull DeleteUserResponse'

instance Hashable DeleteUser

instance NFData DeleteUser

instance ToHeaders DeleteUser where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteUser where
  toPath DeleteUser' {..} =
    mconcat ["/users/", toBS _delInstanceId, "/", toBS _delUserId]

instance ToQuery DeleteUser where
  toQuery = const mempty

-- | /See:/ 'deleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
deleteUserResponse ::
  DeleteUserResponse
deleteUserResponse = DeleteUserResponse'

instance NFData DeleteUserResponse
