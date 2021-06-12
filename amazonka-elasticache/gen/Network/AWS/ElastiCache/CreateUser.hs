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
-- Module      : Network.AWS.ElastiCache.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Creates a Redis user. For more
-- information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)>.
module Network.AWS.ElastiCache.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_passwords,
    createUser_noPasswordRequired,
    createUser_userId,
    createUser_userName,
    createUser_engine,
    createUser_accessString,

    -- * Destructuring the Response
    User (..),
    newUser,

    -- * Response Lenses
    user_status,
    user_accessString,
    user_userGroupIds,
    user_authentication,
    user_arn,
    user_userId,
    user_engine,
    user_userName,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | Passwords used for this user. You can create up to two passwords for
    -- each user.
    passwords :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Indicates a password is not required for this user.
    noPasswordRequired :: Core.Maybe Core.Bool,
    -- | The ID of the user.
    userId :: Core.Text,
    -- | The username of the user.
    userName :: Core.Text,
    -- | The current supported value is Redis.
    engine :: Core.Text,
    -- | Access permissions string used for this user.
    accessString :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'userId', 'createUser_userId' - The ID of the user.
--
-- 'userName', 'createUser_userName' - The username of the user.
--
-- 'engine', 'createUser_engine' - The current supported value is Redis.
--
-- 'accessString', 'createUser_accessString' - Access permissions string used for this user.
newCreateUser ::
  -- | 'userId'
  Core.Text ->
  -- | 'userName'
  Core.Text ->
  -- | 'engine'
  Core.Text ->
  -- | 'accessString'
  Core.Text ->
  CreateUser
newCreateUser
  pUserId_
  pUserName_
  pEngine_
  pAccessString_ =
    CreateUser'
      { passwords = Core.Nothing,
        noPasswordRequired = Core.Nothing,
        userId = pUserId_,
        userName = pUserName_,
        engine = pEngine_,
        accessString = pAccessString_
      }

-- | Passwords used for this user. You can create up to two passwords for
-- each user.
createUser_passwords :: Lens.Lens' CreateUser (Core.Maybe (Core.NonEmpty Core.Text))
createUser_passwords = Lens.lens (\CreateUser' {passwords} -> passwords) (\s@CreateUser' {} a -> s {passwords = a} :: CreateUser) Core.. Lens.mapping Lens._Coerce

-- | Indicates a password is not required for this user.
createUser_noPasswordRequired :: Lens.Lens' CreateUser (Core.Maybe Core.Bool)
createUser_noPasswordRequired = Lens.lens (\CreateUser' {noPasswordRequired} -> noPasswordRequired) (\s@CreateUser' {} a -> s {noPasswordRequired = a} :: CreateUser)

-- | The ID of the user.
createUser_userId :: Lens.Lens' CreateUser Core.Text
createUser_userId = Lens.lens (\CreateUser' {userId} -> userId) (\s@CreateUser' {} a -> s {userId = a} :: CreateUser)

-- | The username of the user.
createUser_userName :: Lens.Lens' CreateUser Core.Text
createUser_userName = Lens.lens (\CreateUser' {userName} -> userName) (\s@CreateUser' {} a -> s {userName = a} :: CreateUser)

-- | The current supported value is Redis.
createUser_engine :: Lens.Lens' CreateUser Core.Text
createUser_engine = Lens.lens (\CreateUser' {engine} -> engine) (\s@CreateUser' {} a -> s {engine = a} :: CreateUser)

-- | Access permissions string used for this user.
createUser_accessString :: Lens.Lens' CreateUser Core.Text
createUser_accessString = Lens.lens (\CreateUser' {accessString} -> accessString) (\s@CreateUser' {} a -> s {accessString = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = User
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateUserResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable CreateUser

instance Core.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateUser where
  toPath = Core.const "/"

instance Core.ToQuery CreateUser where
  toQuery CreateUser' {..} =
    Core.mconcat
      [ "Action" Core.=: ("CreateUser" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "Passwords"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> passwords),
        "NoPasswordRequired" Core.=: noPasswordRequired,
        "UserId" Core.=: userId,
        "UserName" Core.=: userName,
        "Engine" Core.=: engine,
        "AccessString" Core.=: accessString
      ]
