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
-- Module      : Amazonka.IAM.GetLoginProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the user name for the specified IAM user. A login profile is
-- created when you create a password for the user to access the Amazon Web
-- Services Management Console. If the user does not exist or does not have
-- a password, the operation returns a 404 (@NoSuchEntity@) error.
--
-- If you create an IAM user with access to the console, the @CreateDate@
-- reflects the date you created the initial password for the user.
--
-- If you create an IAM user with programmatic access, and then later add a
-- password for the user to access the Amazon Web Services Management
-- Console, the @CreateDate@ reflects the initial password creation date. A
-- user with programmatic access does not have a login profile unless you
-- create a password for the user to access the Amazon Web Services
-- Management Console.
module Amazonka.IAM.GetLoginProfile
  ( -- * Creating a Request
    GetLoginProfile (..),
    newGetLoginProfile,

    -- * Request Lenses
    getLoginProfile_userName,

    -- * Destructuring the Response
    GetLoginProfileResponse (..),
    newGetLoginProfileResponse,

    -- * Response Lenses
    getLoginProfileResponse_httpStatus,
    getLoginProfileResponse_loginProfile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLoginProfile' smart constructor.
data GetLoginProfile = GetLoginProfile'
  { -- | The name of the user whose login profile you want to retrieve.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoginProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'getLoginProfile_userName' - The name of the user whose login profile you want to retrieve.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newGetLoginProfile ::
  -- | 'userName'
  Prelude.Text ->
  GetLoginProfile
newGetLoginProfile pUserName_ =
  GetLoginProfile' {userName = pUserName_}

-- | The name of the user whose login profile you want to retrieve.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getLoginProfile_userName :: Lens.Lens' GetLoginProfile Prelude.Text
getLoginProfile_userName = Lens.lens (\GetLoginProfile' {userName} -> userName) (\s@GetLoginProfile' {} a -> s {userName = a} :: GetLoginProfile)

instance Core.AWSRequest GetLoginProfile where
  type
    AWSResponse GetLoginProfile =
      GetLoginProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetLoginProfileResult"
      ( \s h x ->
          GetLoginProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "LoginProfile")
      )

instance Prelude.Hashable GetLoginProfile where
  hashWithSalt _salt GetLoginProfile' {..} =
    _salt `Prelude.hashWithSalt` userName

instance Prelude.NFData GetLoginProfile where
  rnf GetLoginProfile' {..} = Prelude.rnf userName

instance Data.ToHeaders GetLoginProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetLoginProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLoginProfile where
  toQuery GetLoginProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetLoginProfile" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName
      ]

-- | Contains the response to a successful GetLoginProfile request.
--
-- /See:/ 'newGetLoginProfileResponse' smart constructor.
data GetLoginProfileResponse = GetLoginProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing the user name and the profile creation date for
    -- the user.
    loginProfile :: LoginProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoginProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getLoginProfileResponse_httpStatus' - The response's http status code.
--
-- 'loginProfile', 'getLoginProfileResponse_loginProfile' - A structure containing the user name and the profile creation date for
-- the user.
newGetLoginProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'loginProfile'
  LoginProfile ->
  GetLoginProfileResponse
newGetLoginProfileResponse
  pHttpStatus_
  pLoginProfile_ =
    GetLoginProfileResponse'
      { httpStatus = pHttpStatus_,
        loginProfile = pLoginProfile_
      }

-- | The response's http status code.
getLoginProfileResponse_httpStatus :: Lens.Lens' GetLoginProfileResponse Prelude.Int
getLoginProfileResponse_httpStatus = Lens.lens (\GetLoginProfileResponse' {httpStatus} -> httpStatus) (\s@GetLoginProfileResponse' {} a -> s {httpStatus = a} :: GetLoginProfileResponse)

-- | A structure containing the user name and the profile creation date for
-- the user.
getLoginProfileResponse_loginProfile :: Lens.Lens' GetLoginProfileResponse LoginProfile
getLoginProfileResponse_loginProfile = Lens.lens (\GetLoginProfileResponse' {loginProfile} -> loginProfile) (\s@GetLoginProfileResponse' {} a -> s {loginProfile = a} :: GetLoginProfileResponse)

instance Prelude.NFData GetLoginProfileResponse where
  rnf GetLoginProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf loginProfile
