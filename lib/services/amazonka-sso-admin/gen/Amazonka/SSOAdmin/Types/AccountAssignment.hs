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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.AccountAssignment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSOAdmin.Types.PrincipalType

-- | The assignment that indicates a principal\'s limited access to a
-- specified AWS account with a specified permission set.
--
-- The term /principal/ here refers to a user or group that is defined in
-- IAM Identity Center.
--
-- /See:/ 'newAccountAssignment' smart constructor.
data AccountAssignment = AccountAssignment'
  { -- | The identifier of the AWS account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the permission set. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    permissionSetArn :: Prelude.Maybe Prelude.Text,
    -- | An identifier for an object in IAM Identity Center, such as a user or
    -- group. PrincipalIds are GUIDs (For example,
    -- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
    -- PrincipalIds in IAM Identity Center, see the
    -- </singlesignon/latest/IdentityStoreAPIReference/welcome.html IAM Identity Center Identity Store API Reference>.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The entity type for which the assignment will be created.
    principalType :: Prelude.Maybe PrincipalType
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
-- 'accountId', 'accountAssignment_accountId' - The identifier of the AWS account.
--
-- 'permissionSetArn', 'accountAssignment_permissionSetArn' - The ARN of the permission set. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'principalId', 'accountAssignment_principalId' - An identifier for an object in IAM Identity Center, such as a user or
-- group. PrincipalIds are GUIDs (For example,
-- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
-- PrincipalIds in IAM Identity Center, see the
-- </singlesignon/latest/IdentityStoreAPIReference/welcome.html IAM Identity Center Identity Store API Reference>.
--
-- 'principalType', 'accountAssignment_principalType' - The entity type for which the assignment will be created.
newAccountAssignment ::
  AccountAssignment
newAccountAssignment =
  AccountAssignment'
    { accountId = Prelude.Nothing,
      permissionSetArn = Prelude.Nothing,
      principalId = Prelude.Nothing,
      principalType = Prelude.Nothing
    }

-- | The identifier of the AWS account.
accountAssignment_accountId :: Lens.Lens' AccountAssignment (Prelude.Maybe Prelude.Text)
accountAssignment_accountId = Lens.lens (\AccountAssignment' {accountId} -> accountId) (\s@AccountAssignment' {} a -> s {accountId = a} :: AccountAssignment)

-- | The ARN of the permission set. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
accountAssignment_permissionSetArn :: Lens.Lens' AccountAssignment (Prelude.Maybe Prelude.Text)
accountAssignment_permissionSetArn = Lens.lens (\AccountAssignment' {permissionSetArn} -> permissionSetArn) (\s@AccountAssignment' {} a -> s {permissionSetArn = a} :: AccountAssignment)

-- | An identifier for an object in IAM Identity Center, such as a user or
-- group. PrincipalIds are GUIDs (For example,
-- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
-- PrincipalIds in IAM Identity Center, see the
-- </singlesignon/latest/IdentityStoreAPIReference/welcome.html IAM Identity Center Identity Store API Reference>.
accountAssignment_principalId :: Lens.Lens' AccountAssignment (Prelude.Maybe Prelude.Text)
accountAssignment_principalId = Lens.lens (\AccountAssignment' {principalId} -> principalId) (\s@AccountAssignment' {} a -> s {principalId = a} :: AccountAssignment)

-- | The entity type for which the assignment will be created.
accountAssignment_principalType :: Lens.Lens' AccountAssignment (Prelude.Maybe PrincipalType)
accountAssignment_principalType = Lens.lens (\AccountAssignment' {principalType} -> principalType) (\s@AccountAssignment' {} a -> s {principalType = a} :: AccountAssignment)

instance Data.FromJSON AccountAssignment where
  parseJSON =
    Data.withObject
      "AccountAssignment"
      ( \x ->
          AccountAssignment'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "PermissionSetArn")
            Prelude.<*> (x Data..:? "PrincipalId")
            Prelude.<*> (x Data..:? "PrincipalType")
      )

instance Prelude.Hashable AccountAssignment where
  hashWithSalt _salt AccountAssignment' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` permissionSetArn
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` principalType

instance Prelude.NFData AccountAssignment where
  rnf AccountAssignment' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf permissionSetArn
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf principalType
