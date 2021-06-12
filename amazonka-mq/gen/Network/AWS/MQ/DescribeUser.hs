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
-- Module      : Network.AWS.MQ.DescribeUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an ActiveMQ user.
module Network.AWS.MQ.DescribeUser
  ( -- * Creating a Request
    DescribeUser (..),
    newDescribeUser,

    -- * Request Lenses
    describeUser_username,
    describeUser_brokerId,

    -- * Destructuring the Response
    DescribeUserResponse (..),
    newDescribeUserResponse,

    -- * Response Lenses
    describeUserResponse_groups,
    describeUserResponse_brokerId,
    describeUserResponse_pending,
    describeUserResponse_username,
    describeUserResponse_consoleAccess,
    describeUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Core.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'describeUser_username' - The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
--
-- 'brokerId', 'describeUser_brokerId' - The unique ID that Amazon MQ generates for the broker.
newDescribeUser ::
  -- | 'username'
  Core.Text ->
  -- | 'brokerId'
  Core.Text ->
  DescribeUser
newDescribeUser pUsername_ pBrokerId_ =
  DescribeUser'
    { username = pUsername_,
      brokerId = pBrokerId_
    }

-- | The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
describeUser_username :: Lens.Lens' DescribeUser Core.Text
describeUser_username = Lens.lens (\DescribeUser' {username} -> username) (\s@DescribeUser' {} a -> s {username = a} :: DescribeUser)

-- | The unique ID that Amazon MQ generates for the broker.
describeUser_brokerId :: Lens.Lens' DescribeUser Core.Text
describeUser_brokerId = Lens.lens (\DescribeUser' {brokerId} -> brokerId) (\s@DescribeUser' {} a -> s {brokerId = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Core.<$> (x Core..?> "groups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "brokerId")
            Core.<*> (x Core..?> "pending")
            Core.<*> (x Core..?> "username")
            Core.<*> (x Core..?> "consoleAccess")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUser

instance Core.NFData DescribeUser

instance Core.ToHeaders DescribeUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeUser where
  toPath DescribeUser' {..} =
    Core.mconcat
      [ "/v1/brokers/",
        Core.toBS brokerId,
        "/users/",
        Core.toBS username
      ]

instance Core.ToQuery DescribeUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Core.Maybe [Core.Text],
    -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | The status of the changes pending for the ActiveMQ user.
    pending :: Core.Maybe UserPendingChanges,
    -- | Required. The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Core.Maybe Core.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'describeUserResponse_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'brokerId', 'describeUserResponse_brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- 'pending', 'describeUserResponse_pending' - The status of the changes pending for the ActiveMQ user.
--
-- 'username', 'describeUserResponse_username' - Required. The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
--
-- 'consoleAccess', 'describeUserResponse_consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- 'httpStatus', 'describeUserResponse_httpStatus' - The response's http status code.
newDescribeUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserResponse
newDescribeUserResponse pHttpStatus_ =
  DescribeUserResponse'
    { groups = Core.Nothing,
      brokerId = Core.Nothing,
      pending = Core.Nothing,
      username = Core.Nothing,
      consoleAccess = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
describeUserResponse_groups :: Lens.Lens' DescribeUserResponse (Core.Maybe [Core.Text])
describeUserResponse_groups = Lens.lens (\DescribeUserResponse' {groups} -> groups) (\s@DescribeUserResponse' {} a -> s {groups = a} :: DescribeUserResponse) Core.. Lens.mapping Lens._Coerce

-- | Required. The unique ID that Amazon MQ generates for the broker.
describeUserResponse_brokerId :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
describeUserResponse_brokerId = Lens.lens (\DescribeUserResponse' {brokerId} -> brokerId) (\s@DescribeUserResponse' {} a -> s {brokerId = a} :: DescribeUserResponse)

-- | The status of the changes pending for the ActiveMQ user.
describeUserResponse_pending :: Lens.Lens' DescribeUserResponse (Core.Maybe UserPendingChanges)
describeUserResponse_pending = Lens.lens (\DescribeUserResponse' {pending} -> pending) (\s@DescribeUserResponse' {} a -> s {pending = a} :: DescribeUserResponse)

-- | Required. The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
describeUserResponse_username :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
describeUserResponse_username = Lens.lens (\DescribeUserResponse' {username} -> username) (\s@DescribeUserResponse' {} a -> s {username = a} :: DescribeUserResponse)

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
describeUserResponse_consoleAccess :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Bool)
describeUserResponse_consoleAccess = Lens.lens (\DescribeUserResponse' {consoleAccess} -> consoleAccess) (\s@DescribeUserResponse' {} a -> s {consoleAccess = a} :: DescribeUserResponse)

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Core.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

instance Core.NFData DescribeUserResponse
