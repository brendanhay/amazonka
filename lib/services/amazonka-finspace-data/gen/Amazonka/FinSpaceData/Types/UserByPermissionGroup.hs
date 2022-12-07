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
-- Module      : Amazonka.FinSpaceData.Types.UserByPermissionGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.UserByPermissionGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ApiAccess
import Amazonka.FinSpaceData.Types.PermissionGroupMembershipStatus
import Amazonka.FinSpaceData.Types.UserStatus
import Amazonka.FinSpaceData.Types.UserType
import qualified Amazonka.Prelude as Prelude

-- | The structure of a user account associated with a permission group.
--
-- /See:/ 'newUserByPermissionGroup' smart constructor.
data UserByPermissionGroup = UserByPermissionGroup'
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
    -- | The last name of the user.
    lastName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Indicates whether the user can access FinSpace API operations.
    --
    -- -   @ENABLED@ – The user has permissions to use the API operations.
    --
    -- -   @DISABLED@ – The user does not have permissions to use any API
    --     operations.
    apiAccess :: Prelude.Maybe ApiAccess,
    -- | The unique identifier for the user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the user account within a permission group.
    --
    -- -   @ADDITION_IN_PROGRESS@ – The user account is currently being added
    --     to the permission group.
    --
    -- -   @ADDITION_SUCCESS@ – The user account is successfully added to the
    --     permission group.
    --
    -- -   @REMOVAL_IN_PROGRESS@ – The user is currently being removed from the
    --     permission group.
    membershipStatus :: Prelude.Maybe PermissionGroupMembershipStatus,
    -- | The email address of the user. The email address serves as a unique
    -- identifier for each user and cannot be changed after it\'s created.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The IAM ARN identifier that is attached to FinSpace API calls.
    apiAccessPrincipalArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserByPermissionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'userByPermissionGroup_type' - Indicates the type of user.
--
-- -   @SUPER_USER@ – A user with permission to all the functionality and
--     data in FinSpace.
--
-- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
--     are assigned permissions by adding them to a permission group.
--
-- 'firstName', 'userByPermissionGroup_firstName' - The first name of the user.
--
-- 'status', 'userByPermissionGroup_status' - The current status of the user account.
--
-- -   @CREATING@ – The user account creation is in progress.
--
-- -   @ENABLED@ – The user account is created and is currently active.
--
-- -   @DISABLED@ – The user account is currently inactive.
--
-- 'lastName', 'userByPermissionGroup_lastName' - The last name of the user.
--
-- 'apiAccess', 'userByPermissionGroup_apiAccess' - Indicates whether the user can access FinSpace API operations.
--
-- -   @ENABLED@ – The user has permissions to use the API operations.
--
-- -   @DISABLED@ – The user does not have permissions to use any API
--     operations.
--
-- 'userId', 'userByPermissionGroup_userId' - The unique identifier for the user.
--
-- 'membershipStatus', 'userByPermissionGroup_membershipStatus' - Indicates the status of the user account within a permission group.
--
-- -   @ADDITION_IN_PROGRESS@ – The user account is currently being added
--     to the permission group.
--
-- -   @ADDITION_SUCCESS@ – The user account is successfully added to the
--     permission group.
--
-- -   @REMOVAL_IN_PROGRESS@ – The user is currently being removed from the
--     permission group.
--
-- 'emailAddress', 'userByPermissionGroup_emailAddress' - The email address of the user. The email address serves as a unique
-- identifier for each user and cannot be changed after it\'s created.
--
-- 'apiAccessPrincipalArn', 'userByPermissionGroup_apiAccessPrincipalArn' - The IAM ARN identifier that is attached to FinSpace API calls.
newUserByPermissionGroup ::
  UserByPermissionGroup
newUserByPermissionGroup =
  UserByPermissionGroup'
    { type' = Prelude.Nothing,
      firstName = Prelude.Nothing,
      status = Prelude.Nothing,
      lastName = Prelude.Nothing,
      apiAccess = Prelude.Nothing,
      userId = Prelude.Nothing,
      membershipStatus = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      apiAccessPrincipalArn = Prelude.Nothing
    }

-- | Indicates the type of user.
--
-- -   @SUPER_USER@ – A user with permission to all the functionality and
--     data in FinSpace.
--
-- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
--     are assigned permissions by adding them to a permission group.
userByPermissionGroup_type :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe UserType)
userByPermissionGroup_type = Lens.lens (\UserByPermissionGroup' {type'} -> type') (\s@UserByPermissionGroup' {} a -> s {type' = a} :: UserByPermissionGroup)

-- | The first name of the user.
userByPermissionGroup_firstName :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe Prelude.Text)
userByPermissionGroup_firstName = Lens.lens (\UserByPermissionGroup' {firstName} -> firstName) (\s@UserByPermissionGroup' {} a -> s {firstName = a} :: UserByPermissionGroup) Prelude.. Lens.mapping Data._Sensitive

-- | The current status of the user account.
--
-- -   @CREATING@ – The user account creation is in progress.
--
-- -   @ENABLED@ – The user account is created and is currently active.
--
-- -   @DISABLED@ – The user account is currently inactive.
userByPermissionGroup_status :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe UserStatus)
userByPermissionGroup_status = Lens.lens (\UserByPermissionGroup' {status} -> status) (\s@UserByPermissionGroup' {} a -> s {status = a} :: UserByPermissionGroup)

-- | The last name of the user.
userByPermissionGroup_lastName :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe Prelude.Text)
userByPermissionGroup_lastName = Lens.lens (\UserByPermissionGroup' {lastName} -> lastName) (\s@UserByPermissionGroup' {} a -> s {lastName = a} :: UserByPermissionGroup) Prelude.. Lens.mapping Data._Sensitive

-- | Indicates whether the user can access FinSpace API operations.
--
-- -   @ENABLED@ – The user has permissions to use the API operations.
--
-- -   @DISABLED@ – The user does not have permissions to use any API
--     operations.
userByPermissionGroup_apiAccess :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe ApiAccess)
userByPermissionGroup_apiAccess = Lens.lens (\UserByPermissionGroup' {apiAccess} -> apiAccess) (\s@UserByPermissionGroup' {} a -> s {apiAccess = a} :: UserByPermissionGroup)

-- | The unique identifier for the user.
userByPermissionGroup_userId :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe Prelude.Text)
userByPermissionGroup_userId = Lens.lens (\UserByPermissionGroup' {userId} -> userId) (\s@UserByPermissionGroup' {} a -> s {userId = a} :: UserByPermissionGroup)

-- | Indicates the status of the user account within a permission group.
--
-- -   @ADDITION_IN_PROGRESS@ – The user account is currently being added
--     to the permission group.
--
-- -   @ADDITION_SUCCESS@ – The user account is successfully added to the
--     permission group.
--
-- -   @REMOVAL_IN_PROGRESS@ – The user is currently being removed from the
--     permission group.
userByPermissionGroup_membershipStatus :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe PermissionGroupMembershipStatus)
userByPermissionGroup_membershipStatus = Lens.lens (\UserByPermissionGroup' {membershipStatus} -> membershipStatus) (\s@UserByPermissionGroup' {} a -> s {membershipStatus = a} :: UserByPermissionGroup)

-- | The email address of the user. The email address serves as a unique
-- identifier for each user and cannot be changed after it\'s created.
userByPermissionGroup_emailAddress :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe Prelude.Text)
userByPermissionGroup_emailAddress = Lens.lens (\UserByPermissionGroup' {emailAddress} -> emailAddress) (\s@UserByPermissionGroup' {} a -> s {emailAddress = a} :: UserByPermissionGroup) Prelude.. Lens.mapping Data._Sensitive

-- | The IAM ARN identifier that is attached to FinSpace API calls.
userByPermissionGroup_apiAccessPrincipalArn :: Lens.Lens' UserByPermissionGroup (Prelude.Maybe Prelude.Text)
userByPermissionGroup_apiAccessPrincipalArn = Lens.lens (\UserByPermissionGroup' {apiAccessPrincipalArn} -> apiAccessPrincipalArn) (\s@UserByPermissionGroup' {} a -> s {apiAccessPrincipalArn = a} :: UserByPermissionGroup)

instance Data.FromJSON UserByPermissionGroup where
  parseJSON =
    Data.withObject
      "UserByPermissionGroup"
      ( \x ->
          UserByPermissionGroup'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "firstName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "lastName")
            Prelude.<*> (x Data..:? "apiAccess")
            Prelude.<*> (x Data..:? "userId")
            Prelude.<*> (x Data..:? "membershipStatus")
            Prelude.<*> (x Data..:? "emailAddress")
            Prelude.<*> (x Data..:? "apiAccessPrincipalArn")
      )

instance Prelude.Hashable UserByPermissionGroup where
  hashWithSalt _salt UserByPermissionGroup' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` apiAccess
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` membershipStatus
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` apiAccessPrincipalArn

instance Prelude.NFData UserByPermissionGroup where
  rnf UserByPermissionGroup' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf apiAccess
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf membershipStatus
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf apiAccessPrincipalArn
