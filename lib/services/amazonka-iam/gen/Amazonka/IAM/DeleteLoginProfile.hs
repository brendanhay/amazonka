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
-- Module      : Amazonka.IAM.DeleteLoginProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password for the specified IAM user, For more information,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_passwords_admin-change-user.html Managing passwords for IAM users>.
--
-- You can use the CLI, the Amazon Web Services API, or the __Users__ page
-- in the IAM console to delete a password for any IAM user. You can use
-- ChangePassword to update, but not delete, your own password in the __My
-- Security Credentials__ page in the Amazon Web Services Management
-- Console.
--
-- Deleting a user\'s password does not prevent a user from accessing
-- Amazon Web Services through the command line interface or the API. To
-- prevent all user access, you must also either make any access keys
-- inactive or delete them. For more information about making keys inactive
-- or deleting them, see UpdateAccessKey and DeleteAccessKey.
module Amazonka.IAM.DeleteLoginProfile
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteLoginProfile where
  type
    AWSResponse DeleteLoginProfile =
      DeleteLoginProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteLoginProfileResponse'

instance Prelude.Hashable DeleteLoginProfile where
  hashWithSalt _salt DeleteLoginProfile' {..} =
    _salt `Prelude.hashWithSalt` userName

instance Prelude.NFData DeleteLoginProfile where
  rnf DeleteLoginProfile' {..} = Prelude.rnf userName

instance Data.ToHeaders DeleteLoginProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteLoginProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLoginProfile where
  toQuery DeleteLoginProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteLoginProfile" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName
      ]

-- | /See:/ 'newDeleteLoginProfileResponse' smart constructor.
data DeleteLoginProfileResponse = DeleteLoginProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoginProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLoginProfileResponse ::
  DeleteLoginProfileResponse
newDeleteLoginProfileResponse =
  DeleteLoginProfileResponse'

instance Prelude.NFData DeleteLoginProfileResponse where
  rnf _ = ()
