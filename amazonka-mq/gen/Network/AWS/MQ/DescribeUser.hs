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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'brokerId'
  Prelude.Text ->
  DescribeUser
newDescribeUser pUsername_ pBrokerId_ =
  DescribeUser'
    { username = pUsername_,
      brokerId = pBrokerId_
    }

-- | The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
describeUser_username :: Lens.Lens' DescribeUser Prelude.Text
describeUser_username = Lens.lens (\DescribeUser' {username} -> username) (\s@DescribeUser' {} a -> s {username = a} :: DescribeUser)

-- | The unique ID that Amazon MQ generates for the broker.
describeUser_brokerId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_brokerId = Lens.lens (\DescribeUser' {brokerId} -> brokerId) (\s@DescribeUser' {} a -> s {brokerId = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Prelude.<$> (x Core..?> "groups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "brokerId")
            Prelude.<*> (x Core..?> "pending")
            Prelude.<*> (x Core..?> "username")
            Prelude.<*> (x Core..?> "consoleAccess")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUser

instance Prelude.NFData DescribeUser

instance Core.ToHeaders DescribeUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeUser where
  toPath DescribeUser' {..} =
    Prelude.mconcat
      [ "/v1/brokers/",
        Core.toBS brokerId,
        "/users/",
        Core.toBS username
      ]

instance Core.ToQuery DescribeUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The status of the changes pending for the ActiveMQ user.
    pending :: Prelude.Maybe UserPendingChanges,
    -- | Required. The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Maybe Prelude.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeUserResponse
newDescribeUserResponse pHttpStatus_ =
  DescribeUserResponse'
    { groups = Prelude.Nothing,
      brokerId = Prelude.Nothing,
      pending = Prelude.Nothing,
      username = Prelude.Nothing,
      consoleAccess = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
describeUserResponse_groups :: Lens.Lens' DescribeUserResponse (Prelude.Maybe [Prelude.Text])
describeUserResponse_groups = Lens.lens (\DescribeUserResponse' {groups} -> groups) (\s@DescribeUserResponse' {} a -> s {groups = a} :: DescribeUserResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Required. The unique ID that Amazon MQ generates for the broker.
describeUserResponse_brokerId :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_brokerId = Lens.lens (\DescribeUserResponse' {brokerId} -> brokerId) (\s@DescribeUserResponse' {} a -> s {brokerId = a} :: DescribeUserResponse)

-- | The status of the changes pending for the ActiveMQ user.
describeUserResponse_pending :: Lens.Lens' DescribeUserResponse (Prelude.Maybe UserPendingChanges)
describeUserResponse_pending = Lens.lens (\DescribeUserResponse' {pending} -> pending) (\s@DescribeUserResponse' {} a -> s {pending = a} :: DescribeUserResponse)

-- | Required. The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
describeUserResponse_username :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_username = Lens.lens (\DescribeUserResponse' {username} -> username) (\s@DescribeUserResponse' {} a -> s {username = a} :: DescribeUserResponse)

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
describeUserResponse_consoleAccess :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Bool)
describeUserResponse_consoleAccess = Lens.lens (\DescribeUserResponse' {consoleAccess} -> consoleAccess) (\s@DescribeUserResponse' {} a -> s {consoleAccess = a} :: DescribeUserResponse)

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Prelude.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

instance Prelude.NFData DescribeUserResponse
