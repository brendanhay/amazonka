{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpaceData.Types.User
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ApiAccess
import Amazonka.FinSpaceData.Types.UserStatus
import Amazonka.FinSpaceData.Types.UserType
import qualified Amazonka.Prelude as Prelude

-- | The details of the user account.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | Indicates whether the user can use the
    -- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
    -- then be used to access other FinSpace Data API operations.
    --
    -- -   @ENABLED@ – The user has permissions to use the APIs.
    --
    -- -   @DISABLED@ – The user does not have permissions to use any APIs.
    apiAccess :: Prelude.Maybe ApiAccess,
    -- | The ARN identifier of an AWS user or role that is allowed to call the
    -- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
    -- a specific FinSpace user. This must be an IAM role within your FinSpace
    -- account.
    apiAccessPrincipalArn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the user account was created in FinSpace. The
    -- value is determined as epoch time in milliseconds.
    createTime :: Prelude.Maybe Prelude.Integer,
    -- | The email address of the user. The email address serves as a uniquer
    -- identifier for each user and cannot be changed after it\'s created.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The first name of the user.
    firstName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Describes the last time the user account was disabled. The value is
    -- determined as epoch time in milliseconds.
    lastDisabledTime :: Prelude.Maybe Prelude.Integer,
    -- | Describes the last time the user account was enabled. The value is
    -- determined as epoch time in milliseconds.
    lastEnabledTime :: Prelude.Maybe Prelude.Integer,
    -- | Describes the last time that the user logged into their account. The
    -- value is determined as epoch time in milliseconds.
    lastLoginTime :: Prelude.Maybe Prelude.Integer,
    -- | Describes the last time the user account was updated. The value is
    -- determined as epoch time in milliseconds.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The last name of the user.
    lastName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The current status of the user account.
    --
    -- -   @CREATING@ – The user account creation is in progress.
    --
    -- -   @ENABLED@ – The user account is created and is currently active.
    --
    -- -   @DISABLED@ – The user account is currently inactive.
    status :: Prelude.Maybe UserStatus,
    -- | Indicates the type of user.
    --
    -- -   @SUPER_USER@ – A user with permission to all the functionality and
    --     data in FinSpace.
    --
    -- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
    --     are assigned permissions by adding them to a permission group.
    type' :: Prelude.Maybe UserType,
    -- | The unique identifier for the user.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiAccess', 'user_apiAccess' - Indicates whether the user can use the
-- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
-- then be used to access other FinSpace Data API operations.
--
-- -   @ENABLED@ – The user has permissions to use the APIs.
--
-- -   @DISABLED@ – The user does not have permissions to use any APIs.
--
-- 'apiAccessPrincipalArn', 'user_apiAccessPrincipalArn' - The ARN identifier of an AWS user or role that is allowed to call the
-- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
-- a specific FinSpace user. This must be an IAM role within your FinSpace
-- account.
--
-- 'createTime', 'user_createTime' - The timestamp at which the user account was created in FinSpace. The
-- value is determined as epoch time in milliseconds.
--
-- 'emailAddress', 'user_emailAddress' - The email address of the user. The email address serves as a uniquer
-- identifier for each user and cannot be changed after it\'s created.
--
-- 'firstName', 'user_firstName' - The first name of the user.
--
-- 'lastDisabledTime', 'user_lastDisabledTime' - Describes the last time the user account was disabled. The value is
-- determined as epoch time in milliseconds.
--
-- 'lastEnabledTime', 'user_lastEnabledTime' - Describes the last time the user account was enabled. The value is
-- determined as epoch time in milliseconds.
--
-- 'lastLoginTime', 'user_lastLoginTime' - Describes the last time that the user logged into their account. The
-- value is determined as epoch time in milliseconds.
--
-- 'lastModifiedTime', 'user_lastModifiedTime' - Describes the last time the user account was updated. The value is
-- determined as epoch time in milliseconds.
--
-- 'lastName', 'user_lastName' - The last name of the user.
--
-- 'status', 'user_status' - The current status of the user account.
--
-- -   @CREATING@ – The user account creation is in progress.
--
-- -   @ENABLED@ – The user account is created and is currently active.
--
-- -   @DISABLED@ – The user account is currently inactive.
--
-- 'type'', 'user_type' - Indicates the type of user.
--
-- -   @SUPER_USER@ – A user with permission to all the functionality and
--     data in FinSpace.
--
-- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
--     are assigned permissions by adding them to a permission group.
--
-- 'userId', 'user_userId' - The unique identifier for the user.
newUser ::
  User
newUser =
  User'
    { apiAccess = Prelude.Nothing,
      apiAccessPrincipalArn = Prelude.Nothing,
      createTime = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastDisabledTime = Prelude.Nothing,
      lastEnabledTime = Prelude.Nothing,
      lastLoginTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastName = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | Indicates whether the user can use the
-- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
-- then be used to access other FinSpace Data API operations.
--
-- -   @ENABLED@ – The user has permissions to use the APIs.
--
-- -   @DISABLED@ – The user does not have permissions to use any APIs.
user_apiAccess :: Lens.Lens' User (Prelude.Maybe ApiAccess)
user_apiAccess = Lens.lens (\User' {apiAccess} -> apiAccess) (\s@User' {} a -> s {apiAccess = a} :: User)

-- | The ARN identifier of an AWS user or role that is allowed to call the
-- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
-- a specific FinSpace user. This must be an IAM role within your FinSpace
-- account.
user_apiAccessPrincipalArn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_apiAccessPrincipalArn = Lens.lens (\User' {apiAccessPrincipalArn} -> apiAccessPrincipalArn) (\s@User' {} a -> s {apiAccessPrincipalArn = a} :: User)

-- | The timestamp at which the user account was created in FinSpace. The
-- value is determined as epoch time in milliseconds.
user_createTime :: Lens.Lens' User (Prelude.Maybe Prelude.Integer)
user_createTime = Lens.lens (\User' {createTime} -> createTime) (\s@User' {} a -> s {createTime = a} :: User)

-- | The email address of the user. The email address serves as a uniquer
-- identifier for each user and cannot be changed after it\'s created.
user_emailAddress :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_emailAddress = Lens.lens (\User' {emailAddress} -> emailAddress) (\s@User' {} a -> s {emailAddress = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | The first name of the user.
user_firstName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_firstName = Lens.lens (\User' {firstName} -> firstName) (\s@User' {} a -> s {firstName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | Describes the last time the user account was disabled. The value is
-- determined as epoch time in milliseconds.
user_lastDisabledTime :: Lens.Lens' User (Prelude.Maybe Prelude.Integer)
user_lastDisabledTime = Lens.lens (\User' {lastDisabledTime} -> lastDisabledTime) (\s@User' {} a -> s {lastDisabledTime = a} :: User)

-- | Describes the last time the user account was enabled. The value is
-- determined as epoch time in milliseconds.
user_lastEnabledTime :: Lens.Lens' User (Prelude.Maybe Prelude.Integer)
user_lastEnabledTime = Lens.lens (\User' {lastEnabledTime} -> lastEnabledTime) (\s@User' {} a -> s {lastEnabledTime = a} :: User)

-- | Describes the last time that the user logged into their account. The
-- value is determined as epoch time in milliseconds.
user_lastLoginTime :: Lens.Lens' User (Prelude.Maybe Prelude.Integer)
user_lastLoginTime = Lens.lens (\User' {lastLoginTime} -> lastLoginTime) (\s@User' {} a -> s {lastLoginTime = a} :: User)

-- | Describes the last time the user account was updated. The value is
-- determined as epoch time in milliseconds.
user_lastModifiedTime :: Lens.Lens' User (Prelude.Maybe Prelude.Integer)
user_lastModifiedTime = Lens.lens (\User' {lastModifiedTime} -> lastModifiedTime) (\s@User' {} a -> s {lastModifiedTime = a} :: User)

-- | The last name of the user.
user_lastName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_lastName = Lens.lens (\User' {lastName} -> lastName) (\s@User' {} a -> s {lastName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | The current status of the user account.
--
-- -   @CREATING@ – The user account creation is in progress.
--
-- -   @ENABLED@ – The user account is created and is currently active.
--
-- -   @DISABLED@ – The user account is currently inactive.
user_status :: Lens.Lens' User (Prelude.Maybe UserStatus)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | Indicates the type of user.
--
-- -   @SUPER_USER@ – A user with permission to all the functionality and
--     data in FinSpace.
--
-- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
--     are assigned permissions by adding them to a permission group.
user_type :: Lens.Lens' User (Prelude.Maybe UserType)
user_type = Lens.lens (\User' {type'} -> type') (\s@User' {} a -> s {type' = a} :: User)

-- | The unique identifier for the user.
user_userId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "apiAccess")
            Prelude.<*> (x Data..:? "apiAccessPrincipalArn")
            Prelude.<*> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "emailAddress")
            Prelude.<*> (x Data..:? "firstName")
            Prelude.<*> (x Data..:? "lastDisabledTime")
            Prelude.<*> (x Data..:? "lastEnabledTime")
            Prelude.<*> (x Data..:? "lastLoginTime")
            Prelude.<*> (x Data..:? "lastModifiedTime")
            Prelude.<*> (x Data..:? "lastName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "userId")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt `Prelude.hashWithSalt` apiAccess
      `Prelude.hashWithSalt` apiAccessPrincipalArn
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastDisabledTime
      `Prelude.hashWithSalt` lastEnabledTime
      `Prelude.hashWithSalt` lastLoginTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` userId

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf apiAccess
      `Prelude.seq` Prelude.rnf apiAccessPrincipalArn
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf lastDisabledTime
      `Prelude.seq` Prelude.rnf lastEnabledTime
      `Prelude.seq` Prelude.rnf lastLoginTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf userId
