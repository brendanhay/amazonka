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
-- Module      : Network.AWS.MQ.UpdateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the information for an ActiveMQ user.
module Network.AWS.MQ.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_groups,
    updateUser_password,
    updateUser_consoleAccess,
    updateUser_username,
    updateUser_brokerId,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates the information for an ActiveMQ user.
--
-- /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The password of the user. This value must be at least 12 characters
    -- long, must contain at least 4 unique characters, and must not contain
    -- commas.
    password :: Prelude.Maybe Prelude.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | Required. The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'updateUser_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'password', 'updateUser_password' - The password of the user. This value must be at least 12 characters
-- long, must contain at least 4 unique characters, and must not contain
-- commas.
--
-- 'consoleAccess', 'updateUser_consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- 'username', 'updateUser_username' - Required. The username of the ActiveMQ user. This value can contain only
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
updateUser_groups :: Lens.Lens' UpdateUser (Prelude.Maybe [Prelude.Text])
updateUser_groups = Lens.lens (\UpdateUser' {groups} -> groups) (\s@UpdateUser' {} a -> s {groups = a} :: UpdateUser) Prelude.. Lens.mapping Prelude._Coerce

-- | The password of the user. This value must be at least 12 characters
-- long, must contain at least 4 unique characters, and must not contain
-- commas.
updateUser_password :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_password = Lens.lens (\UpdateUser' {password} -> password) (\s@UpdateUser' {} a -> s {password = a} :: UpdateUser)

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
updateUser_consoleAccess :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Bool)
updateUser_consoleAccess = Lens.lens (\UpdateUser' {consoleAccess} -> consoleAccess) (\s@UpdateUser' {} a -> s {consoleAccess = a} :: UpdateUser)

-- | Required. The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
updateUser_username :: Lens.Lens' UpdateUser Prelude.Text
updateUser_username = Lens.lens (\UpdateUser' {username} -> username) (\s@UpdateUser' {} a -> s {username = a} :: UpdateUser)

-- | The unique ID that Amazon MQ generates for the broker.
updateUser_brokerId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_brokerId = Lens.lens (\UpdateUser' {brokerId} -> brokerId) (\s@UpdateUser' {} a -> s {brokerId = a} :: UpdateUser)

instance Prelude.AWSRequest UpdateUser where
  type Rs UpdateUser = UpdateUserResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUser

instance Prelude.NFData UpdateUser

instance Prelude.ToHeaders UpdateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("groups" Prelude..=) Prelude.<$> groups,
            ("password" Prelude..=) Prelude.<$> password,
            ("consoleAccess" Prelude..=)
              Prelude.<$> consoleAccess
          ]
      )

instance Prelude.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Prelude.mconcat
      [ "/v1/brokers/",
        Prelude.toBS brokerId,
        "/users/",
        Prelude.toBS username
      ]

instance Prelude.ToQuery UpdateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateUserResponse
