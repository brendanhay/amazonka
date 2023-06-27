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
-- Module      : Amazonka.MQ.UpdateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the information for an ActiveMQ user.
module Amazonka.MQ.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_consoleAccess,
    updateUser_groups,
    updateUser_password,
    updateUser_replicationUser,
    updateUser_username,
    updateUser_brokerId,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates the information for an ActiveMQ user.
--
-- /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The password of the user. This value must be at least 12 characters
    -- long, must contain at least 4 unique characters, and must not contain
    -- commas, colons, or equal signs (,:=).
    password :: Prelude.Maybe Prelude.Text,
    -- | Defines whether the user is intended for data replication.
    replicationUser :: Prelude.Maybe Prelude.Bool,
    -- | The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consoleAccess', 'updateUser_consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- 'groups', 'updateUser_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'password', 'updateUser_password' - The password of the user. This value must be at least 12 characters
-- long, must contain at least 4 unique characters, and must not contain
-- commas, colons, or equal signs (,:=).
--
-- 'replicationUser', 'updateUser_replicationUser' - Defines whether the user is intended for data replication.
--
-- 'username', 'updateUser_username' - The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
--
-- 'brokerId', 'updateUser_brokerId' - The unique ID that Amazon MQ generates for the broker.
newUpdateUser ::
  -- | 'username'
  Prelude.Text ->
  -- | 'brokerId'
  Prelude.Text ->
  UpdateUser
newUpdateUser pUsername_ pBrokerId_ =
  UpdateUser'
    { consoleAccess = Prelude.Nothing,
      groups = Prelude.Nothing,
      password = Prelude.Nothing,
      replicationUser = Prelude.Nothing,
      username = pUsername_,
      brokerId = pBrokerId_
    }

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
updateUser_consoleAccess :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Bool)
updateUser_consoleAccess = Lens.lens (\UpdateUser' {consoleAccess} -> consoleAccess) (\s@UpdateUser' {} a -> s {consoleAccess = a} :: UpdateUser)

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
updateUser_groups :: Lens.Lens' UpdateUser (Prelude.Maybe [Prelude.Text])
updateUser_groups = Lens.lens (\UpdateUser' {groups} -> groups) (\s@UpdateUser' {} a -> s {groups = a} :: UpdateUser) Prelude.. Lens.mapping Lens.coerced

-- | The password of the user. This value must be at least 12 characters
-- long, must contain at least 4 unique characters, and must not contain
-- commas, colons, or equal signs (,:=).
updateUser_password :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_password = Lens.lens (\UpdateUser' {password} -> password) (\s@UpdateUser' {} a -> s {password = a} :: UpdateUser)

-- | Defines whether the user is intended for data replication.
updateUser_replicationUser :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Bool)
updateUser_replicationUser = Lens.lens (\UpdateUser' {replicationUser} -> replicationUser) (\s@UpdateUser' {} a -> s {replicationUser = a} :: UpdateUser)

-- | The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
updateUser_username :: Lens.Lens' UpdateUser Prelude.Text
updateUser_username = Lens.lens (\UpdateUser' {username} -> username) (\s@UpdateUser' {} a -> s {username = a} :: UpdateUser)

-- | The unique ID that Amazon MQ generates for the broker.
updateUser_brokerId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_brokerId = Lens.lens (\UpdateUser' {brokerId} -> brokerId) (\s@UpdateUser' {} a -> s {brokerId = a} :: UpdateUser)

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUser where
  hashWithSalt _salt UpdateUser' {..} =
    _salt
      `Prelude.hashWithSalt` consoleAccess
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` replicationUser
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` brokerId

instance Prelude.NFData UpdateUser where
  rnf UpdateUser' {..} =
    Prelude.rnf consoleAccess
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf replicationUser
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf brokerId

instance Data.ToHeaders UpdateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("consoleAccess" Data..=) Prelude.<$> consoleAccess,
            ("groups" Data..=) Prelude.<$> groups,
            ("password" Data..=) Prelude.<$> password,
            ("replicationUser" Data..=)
              Prelude.<$> replicationUser
          ]
      )

instance Data.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Prelude.mconcat
      [ "/v1/brokers/",
        Data.toBS brokerId,
        "/users/",
        Data.toBS username
      ]

instance Data.ToQuery UpdateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateUserResponse_httpStatus' - The response's http status code.
newUpdateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserResponse
newUpdateUserResponse pHttpStatus_ =
  UpdateUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateUserResponse_httpStatus :: Lens.Lens' UpdateUserResponse Prelude.Int
updateUserResponse_httpStatus = Lens.lens (\UpdateUserResponse' {httpStatus} -> httpStatus) (\s@UpdateUserResponse' {} a -> s {httpStatus = a} :: UpdateUserResponse)

instance Prelude.NFData UpdateUserResponse where
  rnf UpdateUserResponse' {..} = Prelude.rnf httpStatus
