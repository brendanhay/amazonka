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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
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
    groups :: Core.Maybe [Core.Text],
    -- | The password of the user. This value must be at least 12 characters
    -- long, must contain at least 4 unique characters, and must not contain
    -- commas.
    password :: Core.Maybe Core.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Core.Maybe Core.Bool,
    -- | Required. The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Core.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'brokerId'
  Core.Text ->
  UpdateUser
newUpdateUser pUsername_ pBrokerId_ =
  UpdateUser'
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
updateUser_groups :: Lens.Lens' UpdateUser (Core.Maybe [Core.Text])
updateUser_groups = Lens.lens (\UpdateUser' {groups} -> groups) (\s@UpdateUser' {} a -> s {groups = a} :: UpdateUser) Core.. Lens.mapping Lens._Coerce

-- | The password of the user. This value must be at least 12 characters
-- long, must contain at least 4 unique characters, and must not contain
-- commas.
updateUser_password :: Lens.Lens' UpdateUser (Core.Maybe Core.Text)
updateUser_password = Lens.lens (\UpdateUser' {password} -> password) (\s@UpdateUser' {} a -> s {password = a} :: UpdateUser)

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
updateUser_consoleAccess :: Lens.Lens' UpdateUser (Core.Maybe Core.Bool)
updateUser_consoleAccess = Lens.lens (\UpdateUser' {consoleAccess} -> consoleAccess) (\s@UpdateUser' {} a -> s {consoleAccess = a} :: UpdateUser)

-- | Required. The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
updateUser_username :: Lens.Lens' UpdateUser Core.Text
updateUser_username = Lens.lens (\UpdateUser' {username} -> username) (\s@UpdateUser' {} a -> s {username = a} :: UpdateUser)

-- | The unique ID that Amazon MQ generates for the broker.
updateUser_brokerId :: Lens.Lens' UpdateUser Core.Text
updateUser_brokerId = Lens.lens (\UpdateUser' {brokerId} -> brokerId) (\s@UpdateUser' {} a -> s {brokerId = a} :: UpdateUser)

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateUserResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateUser

instance Core.NFData UpdateUser

instance Core.ToHeaders UpdateUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("groups" Core..=) Core.<$> groups,
            ("password" Core..=) Core.<$> password,
            ("consoleAccess" Core..=) Core.<$> consoleAccess
          ]
      )

instance Core.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Core.mconcat
      [ "/v1/brokers/",
        Core.toBS brokerId,
        "/users/",
        Core.toBS username
      ]

instance Core.ToQuery UpdateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateUserResponse
newUpdateUserResponse pHttpStatus_ =
  UpdateUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateUserResponse_httpStatus :: Lens.Lens' UpdateUserResponse Core.Int
updateUserResponse_httpStatus = Lens.lens (\UpdateUserResponse' {httpStatus} -> httpStatus) (\s@UpdateUserResponse' {} a -> s {httpStatus = a} :: UpdateUserResponse)

instance Core.NFData UpdateUserResponse
