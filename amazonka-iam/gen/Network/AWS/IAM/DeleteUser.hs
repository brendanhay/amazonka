{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IAM user. Unlike the AWS Management Console, when
-- you delete a user programmatically, you must delete the items attached
-- to the user manually, or the deletion fails. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_manage.html#id_users_deleting_cli Deleting an IAM user>.
-- Before attempting to delete a user, remove the following items:
--
-- -   Password (DeleteLoginProfile)
--
-- -   Access keys (DeleteAccessKey)
--
-- -   Signing certificate (DeleteSigningCertificate)
--
-- -   SSH public key (DeleteSSHPublicKey)
--
-- -   Git credentials (DeleteServiceSpecificCredential)
--
-- -   Multi-factor authentication (MFA) device (DeactivateMFADevice,
--     DeleteVirtualMFADevice)
--
-- -   Inline policies (DeleteUserPolicy)
--
-- -   Attached managed policies (DetachUserPolicy)
--
-- -   Group memberships (RemoveUserFromGroup)
module Network.AWS.IAM.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_userName,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The name of the user to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteUser_userName' - The name of the user to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteUser ::
  -- | 'userName'
  Prelude.Text ->
  DeleteUser
newDeleteUser pUserName_ =
  DeleteUser' {userName = pUserName_}

-- | The name of the user to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteUser_userName :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_userName = Lens.lens (\DeleteUser' {userName} -> userName) (\s@DeleteUser' {} a -> s {userName = a} :: DeleteUser)

instance Prelude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteUserResponse'

instance Prelude.Hashable DeleteUser

instance Prelude.NFData DeleteUser

instance Prelude.ToHeaders DeleteUser where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteUser where
  toQuery DeleteUser' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteUser" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName
      ]

-- | /See:/ 'newDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserResponse ::
  DeleteUserResponse
newDeleteUserResponse = DeleteUserResponse'

instance Prelude.NFData DeleteUserResponse
