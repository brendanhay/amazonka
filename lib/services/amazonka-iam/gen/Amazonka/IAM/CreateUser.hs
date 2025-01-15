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
-- Module      : Amazonka.IAM.CreateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IAM user for your Amazon Web Services account.
--
-- For information about quotas for the number of IAM users you can create,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
module Amazonka.IAM.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_path,
    createUser_permissionsBoundary,
    createUser_tags,
    createUser_userName,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_user,
    createUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The path for the user name. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/).
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    path :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the policy that is used to set the permissions boundary for
    -- the user.
    permissionsBoundary :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that you want to attach to the new user. Each tag
    -- consists of a key name and an associated value. For more information
    -- about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    --
    -- If any one of the tags is invalid or if you exceed the allowed maximum
    -- number of tags, then the entire request fails and the resource is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the user to create.
    --
    -- IAM user, group, role, and policy names must be unique within the
    -- account. Names are not distinguished by case. For example, you cannot
    -- create resources named both \"MyResource\" and \"myresource\".
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'createUser_path' - The path for the user name. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'permissionsBoundary', 'createUser_permissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for
-- the user.
--
-- 'tags', 'createUser_tags' - A list of tags that you want to attach to the new user. Each tag
-- consists of a key name and an associated value. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
--
-- 'userName', 'createUser_userName' - The name of the user to create.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
newCreateUser ::
  -- | 'userName'
  Prelude.Text ->
  CreateUser
newCreateUser pUserName_ =
  CreateUser'
    { path = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      tags = Prelude.Nothing,
      userName = pUserName_
    }

-- | The path for the user name. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
createUser_path :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_path = Lens.lens (\CreateUser' {path} -> path) (\s@CreateUser' {} a -> s {path = a} :: CreateUser)

-- | The ARN of the policy that is used to set the permissions boundary for
-- the user.
createUser_permissionsBoundary :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_permissionsBoundary = Lens.lens (\CreateUser' {permissionsBoundary} -> permissionsBoundary) (\s@CreateUser' {} a -> s {permissionsBoundary = a} :: CreateUser)

-- | A list of tags that you want to attach to the new user. Each tag
-- consists of a key name and an associated value. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
createUser_tags :: Lens.Lens' CreateUser (Prelude.Maybe [Tag])
createUser_tags = Lens.lens (\CreateUser' {tags} -> tags) (\s@CreateUser' {} a -> s {tags = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | The name of the user to create.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
createUser_userName :: Lens.Lens' CreateUser Prelude.Text
createUser_userName = Lens.lens (\CreateUser' {userName} -> userName) (\s@CreateUser' {} a -> s {userName = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateUserResult"
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (x Data..@? "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userName

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf path `Prelude.seq`
      Prelude.rnf permissionsBoundary `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf userName

instance Data.ToHeaders CreateUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUser where
  toQuery CreateUser' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateUser" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Path" Data.=: path,
        "PermissionsBoundary" Data.=: permissionsBoundary,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "UserName" Data.=: userName
      ]

-- | Contains the response to a successful CreateUser request.
--
-- /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | A structure with details about the new IAM user.
    user :: Prelude.Maybe User,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'createUserResponse_user' - A structure with details about the new IAM user.
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure with details about the new IAM user.
createUserResponse_user :: Lens.Lens' CreateUserResponse (Prelude.Maybe User)
createUserResponse_user = Lens.lens (\CreateUserResponse' {user} -> user) (\s@CreateUserResponse' {} a -> s {user = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} =
    Prelude.rnf user `Prelude.seq`
      Prelude.rnf httpStatus
