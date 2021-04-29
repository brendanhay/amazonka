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
-- Module      : Network.AWS.IAM.GetLoginProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the user name and password creation date for the specified IAM
-- user. If the user has not been assigned a password, the operation
-- returns a 404 (@NoSuchEntity@) error.
module Network.AWS.IAM.GetLoginProfile
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetLoginProfile where
  type Rs GetLoginProfile = GetLoginProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetLoginProfileResult"
      ( \s h x ->
          GetLoginProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "LoginProfile")
      )

instance Prelude.Hashable GetLoginProfile

instance Prelude.NFData GetLoginProfile

instance Prelude.ToHeaders GetLoginProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetLoginProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetLoginProfile where
  toQuery GetLoginProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetLoginProfile" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName
      ]

-- | Contains the response to a successful GetLoginProfile request.
--
-- /See:/ 'newGetLoginProfileResponse' smart constructor.
data GetLoginProfileResponse = GetLoginProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing the user name and password create date for the
    -- user.
    loginProfile :: LoginProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'loginProfile', 'getLoginProfileResponse_loginProfile' - A structure containing the user name and password create date for the
-- user.
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

-- | A structure containing the user name and password create date for the
-- user.
getLoginProfileResponse_loginProfile :: Lens.Lens' GetLoginProfileResponse LoginProfile
getLoginProfileResponse_loginProfile = Lens.lens (\GetLoginProfileResponse' {loginProfile} -> loginProfile) (\s@GetLoginProfileResponse' {} a -> s {loginProfile = a} :: GetLoginProfileResponse)

instance Prelude.NFData GetLoginProfileResponse
