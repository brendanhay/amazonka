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
-- Module      : Amazonka.IAM.GetUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified IAM user, including the
-- user\'s creation date, path, unique ID, and ARN.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the Amazon Web Services access key ID used to sign
-- the request to this operation.
module Amazonka.IAM.GetUser
  ( -- * Creating a Request
    GetUser (..),
    newGetUser,

    -- * Request Lenses
    getUser_userName,

    -- * Destructuring the Response
    GetUserResponse (..),
    newGetUserResponse,

    -- * Response Lenses
    getUserResponse_httpStatus,
    getUserResponse_user,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUser' smart constructor.
data GetUser = GetUser'
  { -- | The name of the user to get information about.
    --
    -- This parameter is optional. If it is not included, it defaults to the
    -- user making the request. This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'getUser_userName' - The name of the user to get information about.
--
-- This parameter is optional. If it is not included, it defaults to the
-- user making the request. This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newGetUser ::
  GetUser
newGetUser = GetUser' {userName = Prelude.Nothing}

-- | The name of the user to get information about.
--
-- This parameter is optional. If it is not included, it defaults to the
-- user making the request. This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getUser_userName :: Lens.Lens' GetUser (Prelude.Maybe Prelude.Text)
getUser_userName = Lens.lens (\GetUser' {userName} -> userName) (\s@GetUser' {} a -> s {userName = a} :: GetUser)

instance Core.AWSRequest GetUser where
  type AWSResponse GetUser = GetUserResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetUserResult"
      ( \s h x ->
          GetUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "User")
      )

instance Prelude.Hashable GetUser where
  hashWithSalt _salt GetUser' {..} =
    _salt `Prelude.hashWithSalt` userName

instance Prelude.NFData GetUser where
  rnf GetUser' {..} = Prelude.rnf userName

instance Core.ToHeaders GetUser where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetUser where
  toPath = Prelude.const "/"

instance Core.ToQuery GetUser where
  toQuery GetUser' {..} =
    Prelude.mconcat
      [ "Action" Core.=: ("GetUser" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName
      ]

-- | Contains the response to a successful GetUser request.
--
-- /See:/ 'newGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing details about the IAM user.
    --
    -- Due to a service issue, password last used data does not include
    -- password use from May 3, 2018 22:50 PDT to May 23, 2018 14:08 PDT. This
    -- affects
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_finding-unused.html last sign-in>
    -- dates shown in the IAM console and password last used dates in the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html IAM credential report>,
    -- and returned by this operation. If users signed in during the affected
    -- time, the password last used date that is returned is the date the user
    -- last signed in before May 3, 2018. For users that signed in after May
    -- 23, 2018 14:08 PDT, the returned password last used date is accurate.
    --
    -- You can use password last used information to identify unused
    -- credentials for deletion. For example, you might delete users who did
    -- not sign in to Amazon Web Services in the last 90 days. In cases like
    -- this, we recommend that you adjust your evaluation window to include
    -- dates after May 23, 2018. Alternatively, if your users use access keys
    -- to access Amazon Web Services programmatically you can refer to access
    -- key last used information because it is accurate for all dates.
    user :: User
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getUserResponse_httpStatus' - The response's http status code.
--
-- 'user', 'getUserResponse_user' - A structure containing details about the IAM user.
--
-- Due to a service issue, password last used data does not include
-- password use from May 3, 2018 22:50 PDT to May 23, 2018 14:08 PDT. This
-- affects
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_finding-unused.html last sign-in>
-- dates shown in the IAM console and password last used dates in the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html IAM credential report>,
-- and returned by this operation. If users signed in during the affected
-- time, the password last used date that is returned is the date the user
-- last signed in before May 3, 2018. For users that signed in after May
-- 23, 2018 14:08 PDT, the returned password last used date is accurate.
--
-- You can use password last used information to identify unused
-- credentials for deletion. For example, you might delete users who did
-- not sign in to Amazon Web Services in the last 90 days. In cases like
-- this, we recommend that you adjust your evaluation window to include
-- dates after May 23, 2018. Alternatively, if your users use access keys
-- to access Amazon Web Services programmatically you can refer to access
-- key last used information because it is accurate for all dates.
newGetUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'user'
  User ->
  GetUserResponse
newGetUserResponse pHttpStatus_ pUser_ =
  GetUserResponse'
    { httpStatus = pHttpStatus_,
      user = pUser_
    }

-- | The response's http status code.
getUserResponse_httpStatus :: Lens.Lens' GetUserResponse Prelude.Int
getUserResponse_httpStatus = Lens.lens (\GetUserResponse' {httpStatus} -> httpStatus) (\s@GetUserResponse' {} a -> s {httpStatus = a} :: GetUserResponse)

-- | A structure containing details about the IAM user.
--
-- Due to a service issue, password last used data does not include
-- password use from May 3, 2018 22:50 PDT to May 23, 2018 14:08 PDT. This
-- affects
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_finding-unused.html last sign-in>
-- dates shown in the IAM console and password last used dates in the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html IAM credential report>,
-- and returned by this operation. If users signed in during the affected
-- time, the password last used date that is returned is the date the user
-- last signed in before May 3, 2018. For users that signed in after May
-- 23, 2018 14:08 PDT, the returned password last used date is accurate.
--
-- You can use password last used information to identify unused
-- credentials for deletion. For example, you might delete users who did
-- not sign in to Amazon Web Services in the last 90 days. In cases like
-- this, we recommend that you adjust your evaluation window to include
-- dates after May 23, 2018. Alternatively, if your users use access keys
-- to access Amazon Web Services programmatically you can refer to access
-- key last used information because it is accurate for all dates.
getUserResponse_user :: Lens.Lens' GetUserResponse User
getUserResponse_user = Lens.lens (\GetUserResponse' {user} -> user) (\s@GetUserResponse' {} a -> s {user = a} :: GetUserResponse)

instance Prelude.NFData GetUserResponse where
  rnf GetUserResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf user
