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
-- Module      : Network.AWS.IAM.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IAM user. Unlike the AWS Management Console, when you delete a user programmatically, you must delete the items attached to the user manually, or the deletion fails. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_manage.html#id_users_deleting_cli Deleting an IAM User> . Before attempting to delete a user, remove the following items:
--
--
--     * Password ('DeleteLoginProfile' )
--
--     * Access keys ('DeleteAccessKey' )
--
--     * Signing certificate ('DeleteSigningCertificate' )
--
--     * SSH public key ('DeleteSSHPublicKey' )
--
--     * Git credentials ('DeleteServiceSpecificCredential' )
--
--     * Multi-factor authentication (MFA) device ('DeactivateMFADevice' , 'DeleteVirtualMFADevice' )
--
--     * Inline policies ('DeleteUserPolicy' )
--
--     * Attached managed policies ('DetachUserPolicy' )
--
--     * Group memberships ('RemoveUserFromGroup' )
module Network.AWS.IAM.DeleteUser
  ( -- * Creating a Request
    deleteUser,
    DeleteUser,

    -- * Request Lenses
    duUserName,

    -- * Destructuring the Response
    deleteUserResponse,
    DeleteUserResponse,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
newtype DeleteUser = DeleteUser' {_duUserName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duUserName' - The name of the user to delete. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
deleteUser ::
  -- | 'duUserName'
  Text ->
  DeleteUser
deleteUser pUserName_ = DeleteUser' {_duUserName = pUserName_}

-- | The name of the user to delete. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
duUserName :: Lens' DeleteUser Text
duUserName = lens _duUserName (\s a -> s {_duUserName = a})

instance AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = postQuery iam
  response = receiveNull DeleteUserResponse'

instance Hashable DeleteUser

instance NFData DeleteUser

instance ToHeaders DeleteUser where
  toHeaders = const mempty

instance ToPath DeleteUser where
  toPath = const "/"

instance ToQuery DeleteUser where
  toQuery DeleteUser' {..} =
    mconcat
      [ "Action" =: ("DeleteUser" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "UserName" =: _duUserName
      ]

-- | /See:/ 'deleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
deleteUserResponse ::
  DeleteUserResponse
deleteUserResponse = DeleteUserResponse'

instance NFData DeleteUserResponse
