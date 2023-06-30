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
-- Module      : Amazonka.WorkMail.DescribeUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information regarding the user.
module Amazonka.WorkMail.DescribeUser
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
    describeUserResponse_disabledDate,
    describeUserResponse_displayName,
    describeUserResponse_email,
    describeUserResponse_enabledDate,
    describeUserResponse_name,
    describeUserResponse_state,
    describeUserResponse_userId,
    describeUserResponse_userRole,
    describeUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserResponse'
            Prelude.<$> (x Data..?> "DisabledDate")
            Prelude.<*> (x Data..?> "DisplayName")
            Prelude.<*> (x Data..?> "Email")
            Prelude.<*> (x Data..?> "EnabledDate")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "UserId")
            Prelude.<*> (x Data..?> "UserRole")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUser where
  hashWithSalt _salt DescribeUser' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DescribeUser where
  rnf DescribeUser' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders DescribeUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DescribeUser" ::
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
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("UserId" Data..= userId)
          ]
      )

instance Data.ToPath DescribeUser where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { -- | The date and time at which the user was disabled for WorkMail usage, in
    -- UNIX epoch time format.
    disabledDate :: Prelude.Maybe Data.POSIX,
    -- | The display name of the user.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The email of the user.
    email :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the user was enabled for WorkMailusage, in
    -- UNIX epoch time format.
    enabledDate :: Prelude.Maybe Data.POSIX,
    -- | The name for the user.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of a user: enabled (registered to WorkMail) or disabled
    -- (deregistered or never registered to WorkMail).
    state :: Prelude.Maybe EntityState,
    -- | The identifier for the described user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | In certain cases, other entities are modeled as users. If
    -- interoperability is enabled, resources are imported into WorkMail as
    -- users. Because different WorkMail organizations rely on different
    -- directory types, administrators can distinguish between an unregistered
    -- user (account is disabled and has a user role) and the directory
    -- administrators. The values are USER, RESOURCE, and SYSTEM_USER.
    userRole :: Prelude.Maybe UserRole,
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
-- 'disabledDate', 'describeUserResponse_disabledDate' - The date and time at which the user was disabled for WorkMail usage, in
-- UNIX epoch time format.
--
-- 'displayName', 'describeUserResponse_displayName' - The display name of the user.
--
-- 'email', 'describeUserResponse_email' - The email of the user.
--
-- 'enabledDate', 'describeUserResponse_enabledDate' - The date and time at which the user was enabled for WorkMailusage, in
-- UNIX epoch time format.
--
-- 'name', 'describeUserResponse_name' - The name for the user.
--
-- 'state', 'describeUserResponse_state' - The state of a user: enabled (registered to WorkMail) or disabled
-- (deregistered or never registered to WorkMail).
--
-- 'userId', 'describeUserResponse_userId' - The identifier for the described user.
--
-- 'userRole', 'describeUserResponse_userRole' - In certain cases, other entities are modeled as users. If
-- interoperability is enabled, resources are imported into WorkMail as
-- users. Because different WorkMail organizations rely on different
-- directory types, administrators can distinguish between an unregistered
-- user (account is disabled and has a user role) and the directory
-- administrators. The values are USER, RESOURCE, and SYSTEM_USER.
--
-- 'httpStatus', 'describeUserResponse_httpStatus' - The response's http status code.
newDescribeUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserResponse
newDescribeUserResponse pHttpStatus_ =
  DescribeUserResponse'
    { disabledDate =
        Prelude.Nothing,
      displayName = Prelude.Nothing,
      email = Prelude.Nothing,
      enabledDate = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      userId = Prelude.Nothing,
      userRole = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time at which the user was disabled for WorkMail usage, in
-- UNIX epoch time format.
describeUserResponse_disabledDate :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.UTCTime)
describeUserResponse_disabledDate = Lens.lens (\DescribeUserResponse' {disabledDate} -> disabledDate) (\s@DescribeUserResponse' {} a -> s {disabledDate = a} :: DescribeUserResponse) Prelude.. Lens.mapping Data._Time

-- | The display name of the user.
describeUserResponse_displayName :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_displayName = Lens.lens (\DescribeUserResponse' {displayName} -> displayName) (\s@DescribeUserResponse' {} a -> s {displayName = a} :: DescribeUserResponse)

-- | The email of the user.
describeUserResponse_email :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_email = Lens.lens (\DescribeUserResponse' {email} -> email) (\s@DescribeUserResponse' {} a -> s {email = a} :: DescribeUserResponse)

-- | The date and time at which the user was enabled for WorkMailusage, in
-- UNIX epoch time format.
describeUserResponse_enabledDate :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.UTCTime)
describeUserResponse_enabledDate = Lens.lens (\DescribeUserResponse' {enabledDate} -> enabledDate) (\s@DescribeUserResponse' {} a -> s {enabledDate = a} :: DescribeUserResponse) Prelude.. Lens.mapping Data._Time

-- | The name for the user.
describeUserResponse_name :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_name = Lens.lens (\DescribeUserResponse' {name} -> name) (\s@DescribeUserResponse' {} a -> s {name = a} :: DescribeUserResponse)

-- | The state of a user: enabled (registered to WorkMail) or disabled
-- (deregistered or never registered to WorkMail).
describeUserResponse_state :: Lens.Lens' DescribeUserResponse (Prelude.Maybe EntityState)
describeUserResponse_state = Lens.lens (\DescribeUserResponse' {state} -> state) (\s@DescribeUserResponse' {} a -> s {state = a} :: DescribeUserResponse)

-- | The identifier for the described user.
describeUserResponse_userId :: Lens.Lens' DescribeUserResponse (Prelude.Maybe Prelude.Text)
describeUserResponse_userId = Lens.lens (\DescribeUserResponse' {userId} -> userId) (\s@DescribeUserResponse' {} a -> s {userId = a} :: DescribeUserResponse)

-- | In certain cases, other entities are modeled as users. If
-- interoperability is enabled, resources are imported into WorkMail as
-- users. Because different WorkMail organizations rely on different
-- directory types, administrators can distinguish between an unregistered
-- user (account is disabled and has a user role) and the directory
-- administrators. The values are USER, RESOURCE, and SYSTEM_USER.
describeUserResponse_userRole :: Lens.Lens' DescribeUserResponse (Prelude.Maybe UserRole)
describeUserResponse_userRole = Lens.lens (\DescribeUserResponse' {userRole} -> userRole) (\s@DescribeUserResponse' {} a -> s {userRole = a} :: DescribeUserResponse)

-- | The response's http status code.
describeUserResponse_httpStatus :: Lens.Lens' DescribeUserResponse Prelude.Int
describeUserResponse_httpStatus = Lens.lens (\DescribeUserResponse' {httpStatus} -> httpStatus) (\s@DescribeUserResponse' {} a -> s {httpStatus = a} :: DescribeUserResponse)

instance Prelude.NFData DescribeUserResponse where
  rnf DescribeUserResponse' {..} =
    Prelude.rnf disabledDate
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf enabledDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userRole
      `Prelude.seq` Prelude.rnf httpStatus
