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
-- Module      : Amazonka.MQ.CreateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an ActiveMQ user.
module Amazonka.MQ.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_consoleAccess,
    createUser_groups,
    createUser_username,
    createUser_brokerId,
    createUser_password,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new ActiveMQ user.
--
-- /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | Enables access to the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text,
    -- | Required. The password of the user. This value must be at least 12
    -- characters long, must contain at least 4 unique characters, and must not
    -- contain commas, colons, or equal signs (,:=).
    password :: Prelude.Text
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
-- 'consoleAccess', 'createUser_consoleAccess' - Enables access to the ActiveMQ Web Console for the ActiveMQ user.
--
-- 'groups', 'createUser_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'username', 'createUser_username' - The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
--
-- 'brokerId', 'createUser_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'password', 'createUser_password' - Required. The password of the user. This value must be at least 12
-- characters long, must contain at least 4 unique characters, and must not
-- contain commas, colons, or equal signs (,:=).
newCreateUser ::
  -- | 'username'
  Prelude.Text ->
  -- | 'brokerId'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  CreateUser
newCreateUser pUsername_ pBrokerId_ pPassword_ =
  CreateUser'
    { consoleAccess = Prelude.Nothing,
      groups = Prelude.Nothing,
      username = pUsername_,
      brokerId = pBrokerId_,
      password = pPassword_
    }

-- | Enables access to the ActiveMQ Web Console for the ActiveMQ user.
createUser_consoleAccess :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Bool)
createUser_consoleAccess = Lens.lens (\CreateUser' {consoleAccess} -> consoleAccess) (\s@CreateUser' {} a -> s {consoleAccess = a} :: CreateUser)

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
createUser_groups :: Lens.Lens' CreateUser (Prelude.Maybe [Prelude.Text])
createUser_groups = Lens.lens (\CreateUser' {groups} -> groups) (\s@CreateUser' {} a -> s {groups = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
createUser_username :: Lens.Lens' CreateUser Prelude.Text
createUser_username = Lens.lens (\CreateUser' {username} -> username) (\s@CreateUser' {} a -> s {username = a} :: CreateUser)

-- | The unique ID that Amazon MQ generates for the broker.
createUser_brokerId :: Lens.Lens' CreateUser Prelude.Text
createUser_brokerId = Lens.lens (\CreateUser' {brokerId} -> brokerId) (\s@CreateUser' {} a -> s {brokerId = a} :: CreateUser)

-- | Required. The password of the user. This value must be at least 12
-- characters long, must contain at least 4 unique characters, and must not
-- contain commas, colons, or equal signs (,:=).
createUser_password :: Lens.Lens' CreateUser Prelude.Text
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt
      `Prelude.hashWithSalt` consoleAccess
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` brokerId
      `Prelude.hashWithSalt` password

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf consoleAccess
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf password

instance Data.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("consoleAccess" Data..=) Prelude.<$> consoleAccess,
            ("groups" Data..=) Prelude.<$> groups,
            Prelude.Just ("password" Data..= password)
          ]
      )

instance Data.ToPath CreateUser where
  toPath CreateUser' {..} =
    Prelude.mconcat
      [ "/v1/brokers/",
        Data.toBS brokerId,
        "/users/",
        Data.toBS username
      ]

instance Data.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The response's http status code.
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

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} = Prelude.rnf httpStatus
