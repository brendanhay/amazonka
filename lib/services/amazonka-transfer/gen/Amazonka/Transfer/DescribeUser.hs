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
-- Module      : Amazonka.Transfer.DescribeUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user assigned to the specific file transfer
-- protocol-enabled server, as identified by its @ServerId@ property.
--
-- The response from this call returns the properties of the user
-- associated with the @ServerId@ value that was specified.
module Amazonka.Transfer.DescribeUser
  ( -- * Creating a Request
    DescribeUser (..),
    newDescribeUser,

    -- * Request Lenses
    describeUser_serverId,
    describeUser_userName,

    -- * Destructuring the Response
    DescribeUserResponse (..),
    newDescribeUserResponse,

    -- * Response Lenses
    describeUserResponse_httpStatus,
    describeUserResponse_serverId,
    describeUserResponse_user,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | A system-assigned unique identifier for a server that has this user
    -- assigned.
    serverId :: Prelude.Text,
    -- | The name of the user assigned to one or more servers. User names are
    -- part of the sign-in credentials to use the Transfer Family service and
    -- perform file transfer tasks.
    userName :: Prelude.Text
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
-- 'serverId', 'describeUser_serverId' - A system-assigned unique identifier for a server that has this user
-- assigned.
--
-- 'userName', 'describeUser_userName' - The name of the user assigned to one or more servers. User names are
-- part of the sign-in credentials to use the Transfer Family service and
-- perform file transfer tasks.
newDescribeUser ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  DescribeUser
newDescribeUser pServerId_ pUserName_ =
  DescribeUser'
    { serverId = pServerId_,
      userName = pUserName_
    }

-- | A system-assigned unique identifier for a server that has this user
-- assigned.
describeUser_serverId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_serverId = Lens.lens (\DescribeUser' {serverId} -> serverId) (\s@DescribeUser' {} a -> s {serverId = a} :: DescribeUser)

-- | The name of the user assigned to one or more servers. User names are
-- part of the sign-in credentials to use the Transfer Family service and
-- perform file transfer tasks.
describeUser_userName :: Lens.Lens' DescribeUser Prelude.Text
describeUser_userName = Lens.lens (\DescribeUser' {userName} -> userName) (\s@DescribeUser' {} a -> s {userName = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
            Prelude.<*> (x Data..:> "User")
      )

instance Prelude.Hashable DescribeUser where
  hashWithSalt _salt DescribeUser' {..} =
    _salt
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` userName

instance Prelude.NFData DescribeUser where
  rnf DescribeUser' {..} =
    Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders DescribeUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.DescribeUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeUser where
  toJSON DescribeUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ServerId" Data..= serverId),
            Prelude.Just ("UserName" Data..= userName)
          ]
      )

instance Data.ToPath DescribeUser where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A system-assigned unique identifier for a server that has this user
    -- assigned.
    serverId :: Prelude.Text,
    -- | An array containing the properties of the user account for the
    -- @ServerID@ value that you specified.
    user :: DescribedUser
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
-- 'httpStatus', 'describeUserResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'describeUserResponse_serverId' - A system-assigned unique identifier for a server that has this user
-- assigned.
--
-- 'user', 'describeUserResponse_user' - An array containing the properties of the user account for the
-- @ServerID@ value that you specified.
newDescribeUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'user'
  DescribedUser ->
  DescribeUserResponse
newDescribeUserResponse
  pHttpStatus_
  pServerId_
  pUser_ =
    DescribeUserResponse'
      { httpStatus = pHttpStatus_,
        serverId = pServerId_,
        user = pUser_
      }

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Prelude.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

-- | A system-assigned unique identifier for a server that has this user
-- assigned.
describeUserResponse_serverId :: Lens.Lens' DescribeUserResponse Prelude.Text
describeUserResponse_serverId = Lens.lens (\DescribeUserResponse' {serverId} -> serverId) (\s@DescribeUserResponse' {} a -> s {serverId = a} :: DescribeUserResponse)

-- | An array containing the properties of the user account for the
-- @ServerID@ value that you specified.
describeUserResponse_user :: Lens.Lens' DescribeUserResponse DescribedUser
describeUserResponse_user = Lens.lens (\DescribeUserResponse' {user} -> user) (\s@DescribeUserResponse' {} a -> s {user = a} :: DescribeUserResponse)

instance Prelude.NFData DescribeUserResponse where
  rnf DescribeUserResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf user
