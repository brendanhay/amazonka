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
-- Module      : Amazonka.SSOAdmin.Types.AccountAssignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.AccountAssignment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSOAdmin.Types.PrincipalType

-- | The assignment that indicates a principal\'s limited access to a
-- specified Amazon Web Services account with a specified permission set.
--
-- The term /principal/ here refers to a user or group that is defined in
-- Amazon Web Services SSO.
--
-- /See:/ 'newAccountAssignment' smart constructor.
data AccountAssignment = AccountAssignment'
  { -- | An identifier for an object in Amazon Web Services SSO, such as a user
    -- or group. PrincipalIds are GUIDs (For example,
    -- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
    -- PrincipalIds in Amazon Web Services SSO, see the
    -- </singlesignon/latest/IdentityStoreAPIReference/welcome.html Amazon Web Services SSO Identity Store API Reference>.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The entity type for which the assignment will be created.
    principalType :: Prelude.Maybe PrincipalType,
    -- | The identifier of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the permission set. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
    permissionSetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'accountAssignment_principalId' - An identifier for an object in Amazon Web Services SSO, such as a user
-- or group. PrincipalIds are GUIDs (For example,
-- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
-- PrincipalIds in Amazon Web Services SSO, see the
-- </singlesignon/latest/IdentityStoreAPIReference/welcome.html Amazon Web Services SSO Identity Store API Reference>.
--
-- 'principalType', 'accountAssignment_principalType' - The entity type for which the assignment will be created.
--
-- 'accountId', 'accountAssignment_accountId' - The identifier of the Amazon Web Services account.
--
-- 'permissionSetArn', 'accountAssignment_permissionSetArn' - The ARN of the permission set. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
newAccountAssignment ::
  AccountAssignment
newAccountAssignment =
  AccountAssignment'
    { principalId = Prelude.Nothing,
      principalType = Prelude.Nothing,
      accountId = Prelude.Nothing,
      permissionSetArn = Prelude.Nothing
    }

-- | An identifier for an object in Amazon Web Services SSO, such as a user
-- or group. PrincipalIds are GUIDs (For example,
-- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
-- PrincipalIds in Amazon Web Services SSO, see the
-- </singlesignon/latest/IdentityStoreAPIReference/welcome.html Amazon Web Services SSO Identity Store API Reference>.
accountAssignment_principalId :: Lens.Lens' AccountAssignment (Prelude.Maybe Prelude.Text)
accountAssignment_principalId = Lens.lens (\AccountAssignment' {principalId} -> principalId) (\s@AccountAssignment' {} a -> s {principalId = a} :: AccountAssignment)

-- | The entity type for which the assignment will be created.
accountAssignment_principalType :: Lens.Lens' AccountAssignment (Prelude.Maybe PrincipalType)
accountAssignment_principalType = Lens.lens (\AccountAssignment' {principalType} -> principalType) (\s@AccountAssignment' {} a -> s {principalType = a} :: AccountAssignment)

-- | The identifier of the Amazon Web Services account.
accountAssignment_accountId :: Lens.Lens' AccountAssignment (Prelude.Maybe Prelude.Text)
accountAssignment_accountId = Lens.lens (\AccountAssignment' {accountId} -> accountId) (\s@AccountAssignment' {} a -> s {accountId = a} :: AccountAssignment)

-- | The ARN of the permission set. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
accountAssignment_permissionSetArn :: Lens.Lens' AccountAssignment (Prelude.Maybe Prelude.Text)
accountAssignment_permissionSetArn = Lens.lens (\AccountAssignment' {permissionSetArn} -> permissionSetArn) (\s@AccountAssignment' {} a -> s {permissionSetArn = a} :: AccountAssignment)

instance Core.FromJSON AccountAssignment where
  parseJSON =
    Core.withObject
      "AccountAssignment"
      ( \x ->
          AccountAssignment'
            Prelude.<$> (x Core..:? "PrincipalId")
            Prelude.<*> (x Core..:? "PrincipalType")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "PermissionSetArn")
      )

instance Prelude.Hashable AccountAssignment

instance Prelude.NFData AccountAssignment
