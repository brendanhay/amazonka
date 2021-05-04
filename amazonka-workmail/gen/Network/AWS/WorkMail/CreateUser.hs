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
-- Module      : Network.AWS.WorkMail.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user who can be used in Amazon WorkMail by calling the
-- RegisterToWorkMail operation.
module Network.AWS.WorkMail.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_organizationId,
    createUser_name,
    createUser_displayName,
    createUser_password,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_userId,
    createUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The identifier of the organization for which the user is created.
    organizationId :: Prelude.Text,
    -- | The name for the new user. WorkMail directory user names have a maximum
    -- length of 64. All others have a maximum length of 20.
    name :: Prelude.Text,
    -- | The display name for the new user.
    displayName :: Prelude.Text,
    -- | The password for the new user.
    password :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'createUser_organizationId' - The identifier of the organization for which the user is created.
--
-- 'name', 'createUser_name' - The name for the new user. WorkMail directory user names have a maximum
-- length of 64. All others have a maximum length of 20.
--
-- 'displayName', 'createUser_displayName' - The display name for the new user.
--
-- 'password', 'createUser_password' - The password for the new user.
newCreateUser ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'displayName'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  CreateUser
newCreateUser
  pOrganizationId_
  pName_
  pDisplayName_
  pPassword_ =
    CreateUser'
      { organizationId = pOrganizationId_,
        name = pName_,
        displayName = pDisplayName_,
        password = Prelude._Sensitive Lens.# pPassword_
      }

-- | The identifier of the organization for which the user is created.
createUser_organizationId :: Lens.Lens' CreateUser Prelude.Text
createUser_organizationId = Lens.lens (\CreateUser' {organizationId} -> organizationId) (\s@CreateUser' {} a -> s {organizationId = a} :: CreateUser)

-- | The name for the new user. WorkMail directory user names have a maximum
-- length of 64. All others have a maximum length of 20.
createUser_name :: Lens.Lens' CreateUser Prelude.Text
createUser_name = Lens.lens (\CreateUser' {name} -> name) (\s@CreateUser' {} a -> s {name = a} :: CreateUser)

-- | The display name for the new user.
createUser_displayName :: Lens.Lens' CreateUser Prelude.Text
createUser_displayName = Lens.lens (\CreateUser' {displayName} -> displayName) (\s@CreateUser' {} a -> s {displayName = a} :: CreateUser)

-- | The password for the new user.
createUser_password :: Lens.Lens' CreateUser Prelude.Text
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (x Prelude..?> "UserId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser

instance Prelude.NFData CreateUser

instance Prelude.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("WorkMailService.CreateUser" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("DisplayName" Prelude..= displayName),
            Prelude.Just ("Password" Prelude..= password)
          ]
      )

instance Prelude.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The identifier for the new user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'createUserResponse_userId' - The identifier for the new user.
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { userId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the new user.
createUserResponse_userId :: Lens.Lens' CreateUserResponse (Prelude.Maybe Prelude.Text)
createUserResponse_userId = Lens.lens (\CreateUserResponse' {userId} -> userId) (\s@CreateUserResponse' {} a -> s {userId = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse
