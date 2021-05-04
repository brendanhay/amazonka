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
-- Module      : Network.AWS.IAM.DeleteLoginProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password for the specified IAM user, which terminates the
-- user\'s ability to access AWS services through the AWS Management
-- Console.
--
-- You can use the AWS CLI, the AWS API, or the __Users__ page in the IAM
-- console to delete a password for any IAM user. You can use
-- ChangePassword to update, but not delete, your own password in the __My
-- Security Credentials__ page in the AWS Management Console.
--
-- Deleting a user\'s password does not prevent a user from accessing AWS
-- through the command line interface or the API. To prevent all user
-- access, you must also either make any access keys inactive or delete
-- them. For more information about making keys inactive or deleting them,
-- see UpdateAccessKey and DeleteAccessKey.
module Network.AWS.IAM.DeleteLoginProfile
  ( -- * Creating a Request
    DeleteLoginProfile (..),
    newDeleteLoginProfile,

    -- * Request Lenses
    deleteLoginProfile_userName,

    -- * Destructuring the Response
    DeleteLoginProfileResponse (..),
    newDeleteLoginProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLoginProfile' smart constructor.
data DeleteLoginProfile = DeleteLoginProfile'
  { -- | The name of the user whose password you want to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoginProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteLoginProfile_userName' - The name of the user whose password you want to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteLoginProfile ::
  -- | 'userName'
  Prelude.Text ->
  DeleteLoginProfile
newDeleteLoginProfile pUserName_ =
  DeleteLoginProfile' {userName = pUserName_}

-- | The name of the user whose password you want to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteLoginProfile_userName :: Lens.Lens' DeleteLoginProfile Prelude.Text
deleteLoginProfile_userName = Lens.lens (\DeleteLoginProfile' {userName} -> userName) (\s@DeleteLoginProfile' {} a -> s {userName = a} :: DeleteLoginProfile)

instance Prelude.AWSRequest DeleteLoginProfile where
  type
    Rs DeleteLoginProfile =
      DeleteLoginProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteLoginProfileResponse'

instance Prelude.Hashable DeleteLoginProfile

instance Prelude.NFData DeleteLoginProfile

instance Prelude.ToHeaders DeleteLoginProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteLoginProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLoginProfile where
  toQuery DeleteLoginProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteLoginProfile" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName
      ]

-- | /See:/ 'newDeleteLoginProfileResponse' smart constructor.
data DeleteLoginProfileResponse = DeleteLoginProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoginProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLoginProfileResponse ::
  DeleteLoginProfileResponse
newDeleteLoginProfileResponse =
  DeleteLoginProfileResponse'

instance Prelude.NFData DeleteLoginProfileResponse
