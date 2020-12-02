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
-- Module      : Network.AWS.WorkMail.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from Amazon WorkMail and all subsequent systems. Before you can delete a user, the user state must be @DISABLED@ . Use the 'DescribeUser' action to confirm the user state.
--
--
-- Deleting a user is permanent and cannot be undone. WorkMail archives user mailboxes for 30 days before they are permanently removed.
module Network.AWS.WorkMail.DeleteUser
  ( -- * Creating a Request
    deleteUser,
    DeleteUser,

    -- * Request Lenses
    delOrganizationId,
    delUserId,

    -- * Destructuring the Response
    deleteUserResponse,
    DeleteUserResponse,

    -- * Response Lenses
    duursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'deleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { _delOrganizationId :: !Text,
    _delUserId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delOrganizationId' - The organization that contains the user to be deleted.
--
-- * 'delUserId' - The identifier of the user to be deleted.
deleteUser ::
  -- | 'delOrganizationId'
  Text ->
  -- | 'delUserId'
  Text ->
  DeleteUser
deleteUser pOrganizationId_ pUserId_ =
  DeleteUser'
    { _delOrganizationId = pOrganizationId_,
      _delUserId = pUserId_
    }

-- | The organization that contains the user to be deleted.
delOrganizationId :: Lens' DeleteUser Text
delOrganizationId = lens _delOrganizationId (\s a -> s {_delOrganizationId = a})

-- | The identifier of the user to be deleted.
delUserId :: Lens' DeleteUser Text
delUserId = lens _delUserId (\s a -> s {_delUserId = a})

instance AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = postJSON workMail
  response =
    receiveEmpty
      (\s h x -> DeleteUserResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteUser

instance NFData DeleteUser

instance ToHeaders DeleteUser where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("WorkMailService.DeleteUser" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    object
      ( catMaybes
          [ Just ("OrganizationId" .= _delOrganizationId),
            Just ("UserId" .= _delUserId)
          ]
      )

instance ToPath DeleteUser where
  toPath = const "/"

instance ToQuery DeleteUser where
  toQuery = const mempty

-- | /See:/ 'deleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { _duursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duursResponseStatus' - -- | The response status code.
deleteUserResponse ::
  -- | 'duursResponseStatus'
  Int ->
  DeleteUserResponse
deleteUserResponse pResponseStatus_ =
  DeleteUserResponse' {_duursResponseStatus = pResponseStatus_}

-- | -- | The response status code.
duursResponseStatus :: Lens' DeleteUserResponse Int
duursResponseStatus = lens _duursResponseStatus (\s a -> s {_duursResponseStatus = a})

instance NFData DeleteUserResponse
