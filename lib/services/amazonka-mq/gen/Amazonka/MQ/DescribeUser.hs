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
-- Module      : Amazonka.MQ.DescribeUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an ActiveMQ user.
module Amazonka.MQ.DescribeUser
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
    describeUserResponse_brokerId,
    describeUserResponse_consoleAccess,
    describeUserResponse_groups,
    describeUserResponse_pending,
    describeUserResponse_username,
    describeUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Prelude.<$> (x Data..?> "brokerId")
            Prelude.<*> (x Data..?> "consoleAccess")
            Prelude.<*> (x Data..?> "groups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "pending")
            Prelude.<*> (x Data..?> "username")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUser where
  hashWithSalt _salt DescribeUser' {..} =
    _salt
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` brokerId

instance Prelude.NFData DescribeUser where
  rnf DescribeUser' {..} =
    Prelude.rnf username `Prelude.seq`
      Prelude.rnf brokerId

instance Data.ToHeaders DescribeUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeUser where
  toPath DescribeUser' {..} =
    Prelude.mconcat
      [ "/v1/brokers/",
        Data.toBS brokerId,
        "/users/",
        Data.toBS username
      ]

instance Data.ToQuery DescribeUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The status of the changes pending for the ActiveMQ user.
    pending :: Prelude.Maybe UserPendingChanges,
    -- | Required. The username of the ActiveMQ user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Maybe Prelude.Text,
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
-- 'brokerId', 'describeUserResponse_brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- 'consoleAccess', 'describeUserResponse_consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- 'groups', 'describeUserResponse_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'pending', 'describeUserResponse_pending' - The status of the changes pending for the ActiveMQ user.
--
-- 'username', 'describeUserResponse_username' - Required. The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
--
-- 'httpStatus', 'describeUserResponse_httpStatus' - The response's http status code.
newDescribeUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserResponse
newDescribeUserResponse pHttpStatus_ =
  DescribeUserResponse'
    { brokerId = Prelude.Nothing,
      consoleAccess = Prelude.Nothing,
      groups = Prelude.Nothing,
      pending = Prelude.Nothing,
      username = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Required. The unique ID that Amazon MQ generates for the broker.
describeUserResponse_brokerId :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_brokerId = Lens.lens (\DescribeUserResponse' {brokerId} -> brokerId) (\s@DescribeUserResponse' {} a -> s {brokerId = a} :: DescribeUserResponse)

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
describeUserResponse_consoleAccess :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Bool)
describeUserResponse_consoleAccess = Lens.lens (\DescribeUserResponse' {consoleAccess} -> consoleAccess) (\s@DescribeUserResponse' {} a -> s {consoleAccess = a} :: DescribeUserResponse)

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
describeUserResponse_groups :: Lens.Lens' DescribeUserResponse (Prelude.Maybe [Prelude.Text])
describeUserResponse_groups = Lens.lens (\DescribeUserResponse' {groups} -> groups) (\s@DescribeUserResponse' {} a -> s {groups = a} :: DescribeUserResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the changes pending for the ActiveMQ user.
describeUserResponse_pending :: Lens.Lens' DescribeUserResponse (Prelude.Maybe UserPendingChanges)
describeUserResponse_pending = Lens.lens (\DescribeUserResponse' {pending} -> pending) (\s@DescribeUserResponse' {} a -> s {pending = a} :: DescribeUserResponse)

-- | Required. The username of the ActiveMQ user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
describeUserResponse_username :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_username = Lens.lens (\DescribeUserResponse' {username} -> username) (\s@DescribeUserResponse' {} a -> s {username = a} :: DescribeUserResponse)

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Prelude.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

instance Prelude.NFData DescribeUserResponse where
  rnf DescribeUserResponse' {..} =
    Prelude.rnf brokerId `Prelude.seq`
      Prelude.rnf consoleAccess `Prelude.seq`
        Prelude.rnf groups `Prelude.seq`
          Prelude.rnf pending `Prelude.seq`
            Prelude.rnf username `Prelude.seq`
              Prelude.rnf httpStatus
