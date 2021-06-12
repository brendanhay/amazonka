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
-- Module      : Network.AWS.WorkMail.DescribeUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information regarding the user.
module Network.AWS.WorkMail.DescribeUser
  ( -- * Creating a Request
    DescribeUser (..),
    newDescribeUser,

    -- * Request Lenses
    describeUser_organizationId,
    describeUser_userId,

    -- * Destructuring the Response
    DescribeUserResponse (..),
    newDescribeUserResponse,

    -- * Response Lenses
    describeUserResponse_enabledDate,
    describeUserResponse_userRole,
    describeUserResponse_state,
    describeUserResponse_name,
    describeUserResponse_email,
    describeUserResponse_userId,
    describeUserResponse_disabledDate,
    describeUserResponse_displayName,
    describeUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The identifier for the organization under which the user exists.
    organizationId :: Core.Text,
    -- | The identifier for the user to be described.
    userId :: Core.Text
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
-- 'organizationId', 'describeUser_organizationId' - The identifier for the organization under which the user exists.
--
-- 'userId', 'describeUser_userId' - The identifier for the user to be described.
newDescribeUser ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  DescribeUser
newDescribeUser pOrganizationId_ pUserId_ =
  DescribeUser'
    { organizationId = pOrganizationId_,
      userId = pUserId_
    }

-- | The identifier for the organization under which the user exists.
describeUser_organizationId :: Lens.Lens' DescribeUser Core.Text
describeUser_organizationId = Lens.lens (\DescribeUser' {organizationId} -> organizationId) (\s@DescribeUser' {} a -> s {organizationId = a} :: DescribeUser)

-- | The identifier for the user to be described.
describeUser_userId :: Lens.Lens' DescribeUser Core.Text
describeUser_userId = Lens.lens (\DescribeUser' {userId} -> userId) (\s@DescribeUser' {} a -> s {userId = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Core.<$> (x Core..?> "EnabledDate")
            Core.<*> (x Core..?> "UserRole")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "Email")
            Core.<*> (x Core..?> "UserId")
            Core.<*> (x Core..?> "DisabledDate")
            Core.<*> (x Core..?> "DisplayName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUser

instance Core.NFData DescribeUser

instance Core.ToHeaders DescribeUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkMailService.DescribeUser" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUser where
  toJSON DescribeUser' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("UserId" Core..= userId)
          ]
      )

instance Core.ToPath DescribeUser where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The date and time at which the user was enabled for Amazon WorkMail
    -- usage, in UNIX epoch time format.
    enabledDate :: Core.Maybe Core.POSIX,
    -- | In certain cases, other entities are modeled as users. If
    -- interoperability is enabled, resources are imported into Amazon WorkMail
    -- as users. Because different WorkMail organizations rely on different
    -- directory types, administrators can distinguish between an unregistered
    -- user (account is disabled and has a user role) and the directory
    -- administrators. The values are USER, RESOURCE, and SYSTEM_USER.
    userRole :: Core.Maybe UserRole,
    -- | The state of a user: enabled (registered to Amazon WorkMail) or disabled
    -- (deregistered or never registered to WorkMail).
    state :: Core.Maybe EntityState,
    -- | The name for the user.
    name :: Core.Maybe Core.Text,
    -- | The email of the user.
    email :: Core.Maybe Core.Text,
    -- | The identifier for the described user.
    userId :: Core.Maybe Core.Text,
    -- | The date and time at which the user was disabled for Amazon WorkMail
    -- usage, in UNIX epoch time format.
    disabledDate :: Core.Maybe Core.POSIX,
    -- | The display name of the user.
    displayName :: Core.Maybe Core.Text,
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
-- 'enabledDate', 'describeUserResponse_enabledDate' - The date and time at which the user was enabled for Amazon WorkMail
-- usage, in UNIX epoch time format.
--
-- 'userRole', 'describeUserResponse_userRole' - In certain cases, other entities are modeled as users. If
-- interoperability is enabled, resources are imported into Amazon WorkMail
-- as users. Because different WorkMail organizations rely on different
-- directory types, administrators can distinguish between an unregistered
-- user (account is disabled and has a user role) and the directory
-- administrators. The values are USER, RESOURCE, and SYSTEM_USER.
--
-- 'state', 'describeUserResponse_state' - The state of a user: enabled (registered to Amazon WorkMail) or disabled
-- (deregistered or never registered to WorkMail).
--
-- 'name', 'describeUserResponse_name' - The name for the user.
--
-- 'email', 'describeUserResponse_email' - The email of the user.
--
-- 'userId', 'describeUserResponse_userId' - The identifier for the described user.
--
-- 'disabledDate', 'describeUserResponse_disabledDate' - The date and time at which the user was disabled for Amazon WorkMail
-- usage, in UNIX epoch time format.
--
-- 'displayName', 'describeUserResponse_displayName' - The display name of the user.
--
-- 'httpStatus', 'describeUserResponse_httpStatus' - The response's http status code.
newDescribeUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserResponse
newDescribeUserResponse pHttpStatus_ =
  DescribeUserResponse'
    { enabledDate = Core.Nothing,
      userRole = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      email = Core.Nothing,
      userId = Core.Nothing,
      disabledDate = Core.Nothing,
      displayName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time at which the user was enabled for Amazon WorkMail
-- usage, in UNIX epoch time format.
describeUserResponse_enabledDate :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.UTCTime)
describeUserResponse_enabledDate = Lens.lens (\DescribeUserResponse' {enabledDate} -> enabledDate) (\s@DescribeUserResponse' {} a -> s {enabledDate = a} :: DescribeUserResponse) Core.. Lens.mapping Core._Time

-- | In certain cases, other entities are modeled as users. If
-- interoperability is enabled, resources are imported into Amazon WorkMail
-- as users. Because different WorkMail organizations rely on different
-- directory types, administrators can distinguish between an unregistered
-- user (account is disabled and has a user role) and the directory
-- administrators. The values are USER, RESOURCE, and SYSTEM_USER.
describeUserResponse_userRole :: Lens.Lens' DescribeUserResponse (Core.Maybe UserRole)
describeUserResponse_userRole = Lens.lens (\DescribeUserResponse' {userRole} -> userRole) (\s@DescribeUserResponse' {} a -> s {userRole = a} :: DescribeUserResponse)

-- | The state of a user: enabled (registered to Amazon WorkMail) or disabled
-- (deregistered or never registered to WorkMail).
describeUserResponse_state :: Lens.Lens' DescribeUserResponse (Core.Maybe EntityState)
describeUserResponse_state = Lens.lens (\DescribeUserResponse' {state} -> state) (\s@DescribeUserResponse' {} a -> s {state = a} :: DescribeUserResponse)

-- | The name for the user.
describeUserResponse_name :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
describeUserResponse_name = Lens.lens (\DescribeUserResponse' {name} -> name) (\s@DescribeUserResponse' {} a -> s {name = a} :: DescribeUserResponse)

-- | The email of the user.
describeUserResponse_email :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
describeUserResponse_email = Lens.lens (\DescribeUserResponse' {email} -> email) (\s@DescribeUserResponse' {} a -> s {email = a} :: DescribeUserResponse)

-- | The identifier for the described user.
describeUserResponse_userId :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
describeUserResponse_userId = Lens.lens (\DescribeUserResponse' {userId} -> userId) (\s@DescribeUserResponse' {} a -> s {userId = a} :: DescribeUserResponse)

-- | The date and time at which the user was disabled for Amazon WorkMail
-- usage, in UNIX epoch time format.
describeUserResponse_disabledDate :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.UTCTime)
describeUserResponse_disabledDate = Lens.lens (\DescribeUserResponse' {disabledDate} -> disabledDate) (\s@DescribeUserResponse' {} a -> s {disabledDate = a} :: DescribeUserResponse) Core.. Lens.mapping Core._Time

-- | The display name of the user.
describeUserResponse_displayName :: Lens.Lens' DescribeUserResponse (Core.Maybe Core.Text)
describeUserResponse_displayName = Lens.lens (\DescribeUserResponse' {displayName} -> displayName) (\s@DescribeUserResponse' {} a -> s {displayName = a} :: DescribeUserResponse)

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Core.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

instance Core.NFData DescribeUserResponse
