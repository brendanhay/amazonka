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
-- Module      : Amazonka.FinSpaceData.GetUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details for a specific user.
module Amazonka.FinSpaceData.GetUser
  ( -- * Creating a Request
    GetUser (..),
    newGetUser,

    -- * Request Lenses
    getUser_userId,

    -- * Destructuring the Response
    GetUserResponse (..),
    newGetUserResponse,

    -- * Response Lenses
    getUserResponse_type,
    getUserResponse_firstName,
    getUserResponse_status,
    getUserResponse_lastDisabledTime,
    getUserResponse_lastLoginTime,
    getUserResponse_lastName,
    getUserResponse_lastModifiedTime,
    getUserResponse_apiAccess,
    getUserResponse_userId,
    getUserResponse_lastEnabledTime,
    getUserResponse_emailAddress,
    getUserResponse_createTime,
    getUserResponse_apiAccessPrincipalArn,
    getUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUser' smart constructor.
data GetUser = GetUser'
  { -- | The unique identifier of the user to get data for.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'getUser_userId' - The unique identifier of the user to get data for.
newGetUser ::
  -- | 'userId'
  Prelude.Text ->
  GetUser
newGetUser pUserId_ = GetUser' {userId = pUserId_}

-- | The unique identifier of the user to get data for.
getUser_userId :: Lens.Lens' GetUser Prelude.Text
getUser_userId = Lens.lens (\GetUser' {userId} -> userId) (\s@GetUser' {} a -> s {userId = a} :: GetUser)

instance Core.AWSRequest GetUser where
  type AWSResponse GetUser = GetUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserResponse'
            Prelude.<$> (x Data..?> "type")
            Prelude.<*> (x Data..?> "firstName")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "lastDisabledTime")
            Prelude.<*> (x Data..?> "lastLoginTime")
            Prelude.<*> (x Data..?> "lastName")
            Prelude.<*> (x Data..?> "lastModifiedTime")
            Prelude.<*> (x Data..?> "apiAccess")
            Prelude.<*> (x Data..?> "userId")
            Prelude.<*> (x Data..?> "lastEnabledTime")
            Prelude.<*> (x Data..?> "emailAddress")
            Prelude.<*> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "apiAccessPrincipalArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUser where
  hashWithSalt _salt GetUser' {..} =
    _salt `Prelude.hashWithSalt` userId

instance Prelude.NFData GetUser where
  rnf GetUser' {..} = Prelude.rnf userId

instance Data.ToHeaders GetUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetUser where
  toPath GetUser' {..} =
    Prelude.mconcat ["/user/", Data.toBS userId]

instance Data.ToQuery GetUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { -- | Indicates the type of user.
    --
    -- -   @SUPER_USER@ – A user with permission to all the functionality and
    --     data in FinSpace.
    --
    -- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
    --     are assigned permissions by adding them to a permission group.
    type' :: Prelude.Maybe UserType,
    -- | The first name of the user.
    firstName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The current status of the user account.
    --
    -- -   @CREATING@ – The user account creation is in progress.
    --
    -- -   @ENABLED@ – The user account is created and is currently active.
    --
    -- -   @DISABLED@ – The user account is currently inactive.
    status :: Prelude.Maybe UserStatus,
    -- | Describes the last time the user account was disabled. The value is
    -- determined as epoch time in milliseconds.
    lastDisabledTime :: Prelude.Maybe Prelude.Integer,
    -- | Describes the last time that the user logged into their account. The
    -- value is determined as epoch time in milliseconds.
    lastLoginTime :: Prelude.Maybe Prelude.Integer,
    -- | The last name of the user.
    lastName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Describes the last time the user account was updated. The value is
    -- determined as epoch time in milliseconds.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | Indicates whether the user can use the
    -- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
    -- then be used to access other FinSpace Data API operations.
    --
    -- -   @ENABLED@ – The user has permissions to use the APIs.
    --
    -- -   @DISABLED@ – The user does not have permissions to use any APIs.
    apiAccess :: Prelude.Maybe ApiAccess,
    -- | The unique identifier for the user account that is retrieved.
    userId :: Prelude.Maybe Prelude.Text,
    -- | Describes the last time the user account was enabled. The value is
    -- determined as epoch time in milliseconds.
    lastEnabledTime :: Prelude.Maybe Prelude.Integer,
    -- | The email address that is associated with the user.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The timestamp at which the user account was created in FinSpace. The
    -- value is determined as epoch time in milliseconds.
    createTime :: Prelude.Maybe Prelude.Integer,
    -- | The ARN identifier of an AWS user or role that is allowed to call the
    -- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
    -- a specific FinSpace user. This must be an IAM role within your FinSpace
    -- account.
    apiAccessPrincipalArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'getUserResponse_type' - Indicates the type of user.
--
-- -   @SUPER_USER@ – A user with permission to all the functionality and
--     data in FinSpace.
--
-- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
--     are assigned permissions by adding them to a permission group.
--
-- 'firstName', 'getUserResponse_firstName' - The first name of the user.
--
-- 'status', 'getUserResponse_status' - The current status of the user account.
--
-- -   @CREATING@ – The user account creation is in progress.
--
-- -   @ENABLED@ – The user account is created and is currently active.
--
-- -   @DISABLED@ – The user account is currently inactive.
--
-- 'lastDisabledTime', 'getUserResponse_lastDisabledTime' - Describes the last time the user account was disabled. The value is
-- determined as epoch time in milliseconds.
--
-- 'lastLoginTime', 'getUserResponse_lastLoginTime' - Describes the last time that the user logged into their account. The
-- value is determined as epoch time in milliseconds.
--
-- 'lastName', 'getUserResponse_lastName' - The last name of the user.
--
-- 'lastModifiedTime', 'getUserResponse_lastModifiedTime' - Describes the last time the user account was updated. The value is
-- determined as epoch time in milliseconds.
--
-- 'apiAccess', 'getUserResponse_apiAccess' - Indicates whether the user can use the
-- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
-- then be used to access other FinSpace Data API operations.
--
-- -   @ENABLED@ – The user has permissions to use the APIs.
--
-- -   @DISABLED@ – The user does not have permissions to use any APIs.
--
-- 'userId', 'getUserResponse_userId' - The unique identifier for the user account that is retrieved.
--
-- 'lastEnabledTime', 'getUserResponse_lastEnabledTime' - Describes the last time the user account was enabled. The value is
-- determined as epoch time in milliseconds.
--
-- 'emailAddress', 'getUserResponse_emailAddress' - The email address that is associated with the user.
--
-- 'createTime', 'getUserResponse_createTime' - The timestamp at which the user account was created in FinSpace. The
-- value is determined as epoch time in milliseconds.
--
-- 'apiAccessPrincipalArn', 'getUserResponse_apiAccessPrincipalArn' - The ARN identifier of an AWS user or role that is allowed to call the
-- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
-- a specific FinSpace user. This must be an IAM role within your FinSpace
-- account.
--
-- 'httpStatus', 'getUserResponse_httpStatus' - The response's http status code.
newGetUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUserResponse
newGetUserResponse pHttpStatus_ =
  GetUserResponse'
    { type' = Prelude.Nothing,
      firstName = Prelude.Nothing,
      status = Prelude.Nothing,
      lastDisabledTime = Prelude.Nothing,
      lastLoginTime = Prelude.Nothing,
      lastName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      apiAccess = Prelude.Nothing,
      userId = Prelude.Nothing,
      lastEnabledTime = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      createTime = Prelude.Nothing,
      apiAccessPrincipalArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the type of user.
--
-- -   @SUPER_USER@ – A user with permission to all the functionality and
--     data in FinSpace.
--
-- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
--     are assigned permissions by adding them to a permission group.
getUserResponse_type :: Lens.Lens' GetUserResponse (Prelude.Maybe UserType)
getUserResponse_type = Lens.lens (\GetUserResponse' {type'} -> type') (\s@GetUserResponse' {} a -> s {type' = a} :: GetUserResponse)

-- | The first name of the user.
getUserResponse_firstName :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Text)
getUserResponse_firstName = Lens.lens (\GetUserResponse' {firstName} -> firstName) (\s@GetUserResponse' {} a -> s {firstName = a} :: GetUserResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The current status of the user account.
--
-- -   @CREATING@ – The user account creation is in progress.
--
-- -   @ENABLED@ – The user account is created and is currently active.
--
-- -   @DISABLED@ – The user account is currently inactive.
getUserResponse_status :: Lens.Lens' GetUserResponse (Prelude.Maybe UserStatus)
getUserResponse_status = Lens.lens (\GetUserResponse' {status} -> status) (\s@GetUserResponse' {} a -> s {status = a} :: GetUserResponse)

-- | Describes the last time the user account was disabled. The value is
-- determined as epoch time in milliseconds.
getUserResponse_lastDisabledTime :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Integer)
getUserResponse_lastDisabledTime = Lens.lens (\GetUserResponse' {lastDisabledTime} -> lastDisabledTime) (\s@GetUserResponse' {} a -> s {lastDisabledTime = a} :: GetUserResponse)

-- | Describes the last time that the user logged into their account. The
-- value is determined as epoch time in milliseconds.
getUserResponse_lastLoginTime :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Integer)
getUserResponse_lastLoginTime = Lens.lens (\GetUserResponse' {lastLoginTime} -> lastLoginTime) (\s@GetUserResponse' {} a -> s {lastLoginTime = a} :: GetUserResponse)

-- | The last name of the user.
getUserResponse_lastName :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Text)
getUserResponse_lastName = Lens.lens (\GetUserResponse' {lastName} -> lastName) (\s@GetUserResponse' {} a -> s {lastName = a} :: GetUserResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Describes the last time the user account was updated. The value is
-- determined as epoch time in milliseconds.
getUserResponse_lastModifiedTime :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Integer)
getUserResponse_lastModifiedTime = Lens.lens (\GetUserResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetUserResponse' {} a -> s {lastModifiedTime = a} :: GetUserResponse)

-- | Indicates whether the user can use the
-- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
-- then be used to access other FinSpace Data API operations.
--
-- -   @ENABLED@ – The user has permissions to use the APIs.
--
-- -   @DISABLED@ – The user does not have permissions to use any APIs.
getUserResponse_apiAccess :: Lens.Lens' GetUserResponse (Prelude.Maybe ApiAccess)
getUserResponse_apiAccess = Lens.lens (\GetUserResponse' {apiAccess} -> apiAccess) (\s@GetUserResponse' {} a -> s {apiAccess = a} :: GetUserResponse)

-- | The unique identifier for the user account that is retrieved.
getUserResponse_userId :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Text)
getUserResponse_userId = Lens.lens (\GetUserResponse' {userId} -> userId) (\s@GetUserResponse' {} a -> s {userId = a} :: GetUserResponse)

-- | Describes the last time the user account was enabled. The value is
-- determined as epoch time in milliseconds.
getUserResponse_lastEnabledTime :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Integer)
getUserResponse_lastEnabledTime = Lens.lens (\GetUserResponse' {lastEnabledTime} -> lastEnabledTime) (\s@GetUserResponse' {} a -> s {lastEnabledTime = a} :: GetUserResponse)

-- | The email address that is associated with the user.
getUserResponse_emailAddress :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Text)
getUserResponse_emailAddress = Lens.lens (\GetUserResponse' {emailAddress} -> emailAddress) (\s@GetUserResponse' {} a -> s {emailAddress = a} :: GetUserResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The timestamp at which the user account was created in FinSpace. The
-- value is determined as epoch time in milliseconds.
getUserResponse_createTime :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Integer)
getUserResponse_createTime = Lens.lens (\GetUserResponse' {createTime} -> createTime) (\s@GetUserResponse' {} a -> s {createTime = a} :: GetUserResponse)

-- | The ARN identifier of an AWS user or role that is allowed to call the
-- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
-- a specific FinSpace user. This must be an IAM role within your FinSpace
-- account.
getUserResponse_apiAccessPrincipalArn :: Lens.Lens' GetUserResponse (Prelude.Maybe Prelude.Text)
getUserResponse_apiAccessPrincipalArn = Lens.lens (\GetUserResponse' {apiAccessPrincipalArn} -> apiAccessPrincipalArn) (\s@GetUserResponse' {} a -> s {apiAccessPrincipalArn = a} :: GetUserResponse)

-- | The response's http status code.
getUserResponse_httpStatus :: Lens.Lens' GetUserResponse Prelude.Int
getUserResponse_httpStatus = Lens.lens (\GetUserResponse' {httpStatus} -> httpStatus) (\s@GetUserResponse' {} a -> s {httpStatus = a} :: GetUserResponse)

instance Prelude.NFData GetUserResponse where
  rnf GetUserResponse' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastDisabledTime
      `Prelude.seq` Prelude.rnf lastLoginTime
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf apiAccess
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf lastEnabledTime
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf apiAccessPrincipalArn
      `Prelude.seq` Prelude.rnf httpStatus
