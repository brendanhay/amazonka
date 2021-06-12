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
-- Module      : Network.AWS.MQ.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an ActiveMQ user.
module Network.AWS.MQ.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_groups,
    createUser_password,
    createUser_consoleAccess,
    createUser_username,
    createUser_brokerId,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a new ActiveMQ user.
--
-- /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Core.Maybe [Core.Text],
    -- | Required. The password of the user. This value must be at least 12
    -- characters long, must contain at least 4 unique characters, and must not
    -- contain commas.
    password :: Core.Maybe Core.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Core.Maybe Core.Bool,
    -- | The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Core.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text
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
-- 'groups', 'createUser_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'password', 'createUser_password' - Required. The password of the user. This value must be at least 12
-- characters long, must contain at least 4 unique characters, and must not
-- contain commas.
--
-- 'consoleAccess', 'createUser_consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- 'username', 'createUser_username' - The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
--
-- 'brokerId', 'createUser_brokerId' - The unique ID that Amazon MQ generates for the broker.
newCreateUser ::
  -- | 'username'
  Core.Text ->
  -- | 'brokerId'
  Core.Text ->
  CreateUser
newCreateUser pUsername_ pBrokerId_ =
  CreateUser'
    { groups = Core.Nothing,
      password = Core.Nothing,
      consoleAccess = Core.Nothing,
      username = pUsername_,
      brokerId = pBrokerId_
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
createUser_groups :: Lens.Lens' CreateUser (Core.Maybe [Core.Text])
createUser_groups = Lens.lens (\CreateUser' {groups} -> groups) (\s@CreateUser' {} a -> s {groups = a} :: CreateUser) Core.. Lens.mapping Lens._Coerce

-- | Required. The password of the user. This value must be at least 12
-- characters long, must contain at least 4 unique characters, and must not
-- contain commas.
createUser_password :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser)

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
createUser_consoleAccess :: Lens.Lens' CreateUser (Core.Maybe Core.Bool)
createUser_consoleAccess = Lens.lens (\CreateUser' {consoleAccess} -> consoleAccess) (\s@CreateUser' {} a -> s {consoleAccess = a} :: CreateUser)

-- | The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
createUser_username :: Lens.Lens' CreateUser Core.Text
createUser_username = Lens.lens (\CreateUser' {username} -> username) (\s@CreateUser' {} a -> s {username = a} :: CreateUser)

-- | The unique ID that Amazon MQ generates for the broker.
createUser_brokerId :: Lens.Lens' CreateUser Core.Text
createUser_brokerId = Lens.lens (\CreateUser' {brokerId} -> brokerId) (\s@CreateUser' {} a -> s {brokerId = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUser

instance Core.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("groups" Core..=) Core.<$> groups,
            ("password" Core..=) Core.<$> password,
            ("consoleAccess" Core..=) Core.<$> consoleAccess
          ]
      )

instance Core.ToPath CreateUser where
  toPath CreateUser' {..} =
    Core.mconcat
      [ "/v1/brokers/",
        Core.toBS brokerId,
        "/users/",
        Core.toBS username
      ]

instance Core.ToQuery CreateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Core.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Core.NFData CreateUserResponse
