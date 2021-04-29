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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
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
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Required. The password of the user. This value must be at least 12
    -- characters long, must contain at least 4 unique characters, and must not
    -- contain commas.
    password :: Prelude.Maybe Prelude.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'brokerId'
  Prelude.Text ->
  CreateUser
newCreateUser pUsername_ pBrokerId_ =
  CreateUser'
    { groups = Prelude.Nothing,
      password = Prelude.Nothing,
      consoleAccess = Prelude.Nothing,
      username = pUsername_,
      brokerId = pBrokerId_
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
createUser_groups :: Lens.Lens' CreateUser (Prelude.Maybe [Prelude.Text])
createUser_groups = Lens.lens (\CreateUser' {groups} -> groups) (\s@CreateUser' {} a -> s {groups = a} :: CreateUser) Prelude.. Lens.mapping Prelude._Coerce

-- | Required. The password of the user. This value must be at least 12
-- characters long, must contain at least 4 unique characters, and must not
-- contain commas.
createUser_password :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser)

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
createUser_consoleAccess :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Bool)
createUser_consoleAccess = Lens.lens (\CreateUser' {consoleAccess} -> consoleAccess) (\s@CreateUser' {} a -> s {consoleAccess = a} :: CreateUser)

-- | The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
createUser_username :: Lens.Lens' CreateUser Prelude.Text
createUser_username = Lens.lens (\CreateUser' {username} -> username) (\s@CreateUser' {} a -> s {username = a} :: CreateUser)

-- | The unique ID that Amazon MQ generates for the broker.
createUser_brokerId :: Lens.Lens' CreateUser Prelude.Text
createUser_brokerId = Lens.lens (\CreateUser' {brokerId} -> brokerId) (\s@CreateUser' {} a -> s {brokerId = a} :: CreateUser)

instance Prelude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser

instance Prelude.NFData CreateUser

instance Prelude.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("groups" Prelude..=) Prelude.<$> groups,
            ("password" Prelude..=) Prelude.<$> password,
            ("consoleAccess" Prelude..=)
              Prelude.<$> consoleAccess
          ]
      )

instance Prelude.ToPath CreateUser where
  toPath CreateUser' {..} =
    Prelude.mconcat
      [ "/v1/brokers/",
        Prelude.toBS brokerId,
        "/users/",
        Prelude.toBS username
      ]

instance Prelude.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse
