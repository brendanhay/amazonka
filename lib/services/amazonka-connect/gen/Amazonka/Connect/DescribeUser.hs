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
-- Module      : Amazonka.Connect.DescribeUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified user account. You can find the instance ID in
-- the console (it’s the final part of the ARN). The console does not
-- display the user IDs. Instead, list the users and note the IDs provided
-- in the output.
module Amazonka.Connect.DescribeUser
  ( -- * Creating a Request
    DescribeUser (..),
    newDescribeUser,

    -- * Request Lenses
    describeUser_userId,
    describeUser_instanceId,

    -- * Destructuring the Response
    DescribeUserResponse (..),
    newDescribeUserResponse,

    -- * Response Lenses
    describeUserResponse_user,
    describeUserResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The identifier of the user account.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
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
-- 'userId', 'describeUser_userId' - The identifier of the user account.
--
-- 'instanceId', 'describeUser_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newDescribeUser ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  DescribeUser
newDescribeUser pUserId_ pInstanceId_ =
  DescribeUser'
    { userId = pUserId_,
      instanceId = pInstanceId_
    }

-- | The identifier of the user account.
describeUser_userId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_userId = Lens.lens (\DescribeUser' {userId} -> userId) (\s@DescribeUser' {} a -> s {userId = a} :: DescribeUser)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeUser_instanceId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_instanceId = Lens.lens (\DescribeUser' {instanceId} -> instanceId) (\s@DescribeUser' {} a -> s {instanceId = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Prelude.<$> (x Data..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUser where
  hashWithSalt _salt DescribeUser' {..} =
    _salt
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DescribeUser where
  rnf DescribeUser' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf instanceId

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
      [ "/users/",
        Data.toBS instanceId,
        "/",
        Data.toBS userId
      ]

instance Data.ToQuery DescribeUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | Information about the user account and configuration settings.
    user :: Prelude.Maybe User,
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
-- 'user', 'describeUserResponse_user' - Information about the user account and configuration settings.
--
-- 'httpStatus', 'describeUserResponse_httpStatus' - The response's http status code.
newDescribeUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserResponse
newDescribeUserResponse pHttpStatus_ =
  DescribeUserResponse'
    { user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the user account and configuration settings.
describeUserResponse_user :: Lens.Lens' DescribeUserResponse (Prelude.Maybe User)
describeUserResponse_user = Lens.lens (\DescribeUserResponse' {user} -> user) (\s@DescribeUserResponse' {} a -> s {user = a} :: DescribeUserResponse)

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Prelude.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

instance Prelude.NFData DescribeUserResponse where
  rnf DescribeUserResponse' {..} =
    Prelude.rnf user
      `Prelude.seq` Prelude.rnf httpStatus
