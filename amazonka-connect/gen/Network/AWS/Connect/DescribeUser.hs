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
-- Module      : Network.AWS.Connect.DescribeUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified user account. You can find the instance ID in
-- the console (it’s the final part of the ARN). The console does not
-- display the user IDs. Instead, list the users and note the IDs provided
-- in the output.
module Network.AWS.Connect.DescribeUser
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The identifier of the user account.
    userId :: Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
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
-- 'userId', 'describeUser_userId' - The identifier of the user account.
--
-- 'instanceId', 'describeUser_instanceId' - The identifier of the Amazon Connect instance.
newDescribeUser ::
  -- | 'userId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  DescribeUser
newDescribeUser pUserId_ pInstanceId_ =
  DescribeUser'
    { userId = pUserId_,
      instanceId = pInstanceId_
    }

-- | The identifier of the user account.
describeUser_userId :: Lens.Lens' DescribeUser Core.Text
describeUser_userId = Lens.lens (\DescribeUser' {userId} -> userId) (\s@DescribeUser' {} a -> s {userId = a} :: DescribeUser)

-- | The identifier of the Amazon Connect instance.
describeUser_instanceId :: Lens.Lens' DescribeUser Core.Text
describeUser_instanceId = Lens.lens (\DescribeUser' {instanceId} -> instanceId) (\s@DescribeUser' {} a -> s {instanceId = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Core.<$> (x Core..?> "User")
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
      [ "/users/",
        Core.toBS instanceId,
        "/",
        Core.toBS userId
      ]

instance Core.ToQuery DescribeUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | Information about the user account and configuration settings.
    user :: Core.Maybe User,
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
-- 'user', 'describeUserResponse_user' - Information about the user account and configuration settings.
--
-- 'httpStatus', 'describeUserResponse_httpStatus' - The response's http status code.
newDescribeUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserResponse
newDescribeUserResponse pHttpStatus_ =
  DescribeUserResponse'
    { user = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the user account and configuration settings.
describeUserResponse_user :: Lens.Lens' DescribeUserResponse (Core.Maybe User)
describeUserResponse_user = Lens.lens (\DescribeUserResponse' {user} -> user) (\s@DescribeUserResponse' {} a -> s {user = a} :: DescribeUserResponse)

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Core.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

instance Core.NFData DescribeUserResponse
