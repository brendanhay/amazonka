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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDescribeUser' smart constructor.
data DescribeUser = DescribeUser'
  { -- | The identifier for the organization under which the user exists.
    organizationId :: Prelude.Text,
    -- | The identifier for the user to be described.
    userId :: Prelude.Text
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
-- 'organizationId', 'describeUser_organizationId' - The identifier for the organization under which the user exists.
--
-- 'userId', 'describeUser_userId' - The identifier for the user to be described.
newDescribeUser ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DescribeUser
newDescribeUser pOrganizationId_ pUserId_ =
  DescribeUser'
    { organizationId = pOrganizationId_,
      userId = pUserId_
    }

-- | The identifier for the organization under which the user exists.
describeUser_organizationId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_organizationId = Lens.lens (\DescribeUser' {organizationId} -> organizationId) (\s@DescribeUser' {} a -> s {organizationId = a} :: DescribeUser)

-- | The identifier for the user to be described.
describeUser_userId :: Lens.Lens' DescribeUser Prelude.Text
describeUser_userId = Lens.lens (\DescribeUser' {userId} -> userId) (\s@DescribeUser' {} a -> s {userId = a} :: DescribeUser)

instance Core.AWSRequest DescribeUser where
  type AWSResponse DescribeUser = DescribeUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Prelude.<$> (x Core..?> "EnabledDate")
            Prelude.<*> (x Core..?> "UserRole")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Email")
            Prelude.<*> (x Core..?> "UserId")
            Prelude.<*> (x Core..?> "DisabledDate")
            Prelude.<*> (x Core..?> "DisplayName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUser

instance Prelude.NFData DescribeUser

instance Core.ToHeaders DescribeUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DescribeUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeUser where
  toJSON DescribeUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("UserId" Core..= userId)
          ]
      )

instance Core.ToPath DescribeUser where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The date and time at which the user was enabled for Amazon WorkMail
    -- usage, in UNIX epoch time format.
    enabledDate :: Prelude.Maybe Core.POSIX,
    -- | In certain cases, other entities are modeled as users. If
    -- interoperability is enabled, resources are imported into Amazon WorkMail
    -- as users. Because different WorkMail organizations rely on different
    -- directory types, administrators can distinguish between an unregistered
    -- user (account is disabled and has a user role) and the directory
    -- administrators. The values are USER, RESOURCE, and SYSTEM_USER.
    userRole :: Prelude.Maybe UserRole,
    -- | The state of a user: enabled (registered to Amazon WorkMail) or disabled
    -- (deregistered or never registered to WorkMail).
    state :: Prelude.Maybe EntityState,
    -- | The name for the user.
    name :: Prelude.Maybe Prelude.Text,
    -- | The email of the user.
    email :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the described user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the user was disabled for Amazon WorkMail
    -- usage, in UNIX epoch time format.
    disabledDate :: Prelude.Maybe Core.POSIX,
    -- | The display name of the user.
    displayName :: Prelude.Maybe Prelude.Text,
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
  Prelude.Int ->
  DescribeUserResponse
newDescribeUserResponse pHttpStatus_ =
  DescribeUserResponse'
    { enabledDate =
        Prelude.Nothing,
      userRole = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      email = Prelude.Nothing,
      userId = Prelude.Nothing,
      disabledDate = Prelude.Nothing,
      displayName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time at which the user was enabled for Amazon WorkMail
-- usage, in UNIX epoch time format.
describeUserResponse_enabledDate :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.UTCTime)
describeUserResponse_enabledDate = Lens.lens (\DescribeUserResponse' {enabledDate} -> enabledDate) (\s@DescribeUserResponse' {} a -> s {enabledDate = a} :: DescribeUserResponse) Prelude.. Lens.mapping Core._Time

-- | In certain cases, other entities are modeled as users. If
-- interoperability is enabled, resources are imported into Amazon WorkMail
-- as users. Because different WorkMail organizations rely on different
-- directory types, administrators can distinguish between an unregistered
-- user (account is disabled and has a user role) and the directory
-- administrators. The values are USER, RESOURCE, and SYSTEM_USER.
describeUserResponse_userRole :: Lens.Lens' DescribeUserResponse (Prelude.Maybe UserRole)
describeUserResponse_userRole = Lens.lens (\DescribeUserResponse' {userRole} -> userRole) (\s@DescribeUserResponse' {} a -> s {userRole = a} :: DescribeUserResponse)

-- | The state of a user: enabled (registered to Amazon WorkMail) or disabled
-- (deregistered or never registered to WorkMail).
describeUserResponse_state :: Lens.Lens' DescribeUserResponse (Prelude.Maybe EntityState)
describeUserResponse_state = Lens.lens (\DescribeUserResponse' {state} -> state) (\s@DescribeUserResponse' {} a -> s {state = a} :: DescribeUserResponse)

-- | The name for the user.
describeUserResponse_name :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_name = Lens.lens (\DescribeUserResponse' {name} -> name) (\s@DescribeUserResponse' {} a -> s {name = a} :: DescribeUserResponse)

-- | The email of the user.
describeUserResponse_email :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_email = Lens.lens (\DescribeUserResponse' {email} -> email) (\s@DescribeUserResponse' {} a -> s {email = a} :: DescribeUserResponse)

-- | The identifier for the described user.
describeUserResponse_userId :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_userId = Lens.lens (\DescribeUserResponse' {userId} -> userId) (\s@DescribeUserResponse' {} a -> s {userId = a} :: DescribeUserResponse)

-- | The date and time at which the user was disabled for Amazon WorkMail
-- usage, in UNIX epoch time format.
describeUserResponse_disabledDate :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.UTCTime)
describeUserResponse_disabledDate = Lens.lens (\DescribeUserResponse' {disabledDate} -> disabledDate) (\s@DescribeUserResponse' {} a -> s {disabledDate = a} :: DescribeUserResponse) Prelude.. Lens.mapping Core._Time

-- | The display name of the user.
describeUserResponse_displayName :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_displayName = Lens.lens (\DescribeUserResponse' {displayName} -> displayName) (\s@DescribeUserResponse' {} a -> s {displayName = a} :: DescribeUserResponse)

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Prelude.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

instance Prelude.NFData DescribeUserResponse
