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
-- Module      : Amazonka.ElastiCache.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Creates a Redis user. For more
-- information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)>.
module Amazonka.ElastiCache.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_passwords,
    createUser_noPasswordRequired,
    createUser_tags,
    createUser_userId,
    createUser_userName,
    createUser_engine,
    createUser_accessString,

    -- * Destructuring the Response
    User (..),
    newUser,

    -- * Response Lenses
    user_status,
    user_arn,
    user_userGroupIds,
    user_authentication,
    user_engine,
    user_userName,
    user_accessString,
    user_userId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | Passwords used for this user. You can create up to two passwords for
    -- each user.
    passwords :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Indicates a password is not required for this user.
    noPasswordRequired :: Prelude.Maybe Prelude.Bool,
    -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the user.
    userId :: Prelude.Text,
    -- | The username of the user.
    userName :: Prelude.Text,
    -- | The current supported value is Redis.
    engine :: Prelude.Text,
    -- | Access permissions string used for this user.
    accessString :: Prelude.Text
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
-- 'passwords', 'createUser_passwords' - Passwords used for this user. You can create up to two passwords for
-- each user.
--
-- 'noPasswordRequired', 'createUser_noPasswordRequired' - Indicates a password is not required for this user.
--
-- 'tags', 'createUser_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'userId', 'createUser_userId' - The ID of the user.
--
-- 'userName', 'createUser_userName' - The username of the user.
--
-- 'engine', 'createUser_engine' - The current supported value is Redis.
--
-- 'accessString', 'createUser_accessString' - Access permissions string used for this user.
newCreateUser ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  -- | 'accessString'
  Prelude.Text ->
  CreateUser
newCreateUser
  pUserId_
  pUserName_
  pEngine_
  pAccessString_ =
    CreateUser'
      { passwords = Prelude.Nothing,
        noPasswordRequired = Prelude.Nothing,
        tags = Prelude.Nothing,
        userId = pUserId_,
        userName = pUserName_,
        engine = pEngine_,
        accessString = pAccessString_
      }

-- | Passwords used for this user. You can create up to two passwords for
-- each user.
createUser_passwords :: Lens.Lens' CreateUser (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createUser_passwords = Lens.lens (\CreateUser' {passwords} -> passwords) (\s@CreateUser' {} a -> s {passwords = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | Indicates a password is not required for this user.
createUser_noPasswordRequired :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Bool)
createUser_noPasswordRequired = Lens.lens (\CreateUser' {noPasswordRequired} -> noPasswordRequired) (\s@CreateUser' {} a -> s {noPasswordRequired = a} :: CreateUser)

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
createUser_tags :: Lens.Lens' CreateUser (Prelude.Maybe [Tag])
createUser_tags = Lens.lens (\CreateUser' {tags} -> tags) (\s@CreateUser' {} a -> s {tags = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the user.
createUser_userId :: Lens.Lens' CreateUser Prelude.Text
createUser_userId = Lens.lens (\CreateUser' {userId} -> userId) (\s@CreateUser' {} a -> s {userId = a} :: CreateUser)

-- | The username of the user.
createUser_userName :: Lens.Lens' CreateUser Prelude.Text
createUser_userName = Lens.lens (\CreateUser' {userName} -> userName) (\s@CreateUser' {} a -> s {userName = a} :: CreateUser)

-- | The current supported value is Redis.
createUser_engine :: Lens.Lens' CreateUser Prelude.Text
createUser_engine = Lens.lens (\CreateUser' {engine} -> engine) (\s@CreateUser' {} a -> s {engine = a} :: CreateUser)

-- | Access permissions string used for this user.
createUser_accessString :: Lens.Lens' CreateUser Prelude.Text
createUser_accessString = Lens.lens (\CreateUser' {accessString} -> accessString) (\s@CreateUser' {} a -> s {accessString = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = User
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateUserResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt `Prelude.hashWithSalt` passwords
      `Prelude.hashWithSalt` noPasswordRequired
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` accessString

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf passwords
      `Prelude.seq` Prelude.rnf noPasswordRequired
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf accessString

instance Core.ToHeaders CreateUser where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUser where
  toQuery CreateUser' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateUser" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "Passwords"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> passwords),
        "NoPasswordRequired" Core.=: noPasswordRequired,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "UserId" Core.=: userId,
        "UserName" Core.=: userName,
        "Engine" Core.=: engine,
        "AccessString" Core.=: accessString
      ]
