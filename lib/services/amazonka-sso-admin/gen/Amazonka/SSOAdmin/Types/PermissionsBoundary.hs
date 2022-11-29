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
-- Module      : Amazonka.SSOAdmin.Types.PermissionsBoundary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.PermissionsBoundary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSOAdmin.Types.CustomerManagedPolicyReference

-- | Specifies the configuration of the AWS managed or customer managed
-- policy that you want to set as a permissions boundary. Specify either
-- @CustomerManagedPolicyReference@ to use the name and path of a customer
-- managed policy, or @ManagedPolicyArn@ to use the ARN of an AWS managed
-- policy. A permissions boundary represents the maximum permissions that
-- any policy can grant your role. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM entities>
-- in the /IAM User Guide/.
--
-- Policies used as permissions boundaries don\'t provide permissions. You
-- must also attach an IAM policy to the role. To learn how the effective
-- permissions for a role are evaluated, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON policy evaluation logic>
-- in the /IAM User Guide/.
--
-- /See:/ 'newPermissionsBoundary' smart constructor.
data PermissionsBoundary = PermissionsBoundary'
  { -- | The AWS managed policy ARN that you want to attach to a permission set
    -- as a permissions boundary.
    managedPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name and path of a customer managed policy. You must have
    -- an IAM policy that matches the name and path in each AWS account where
    -- you want to deploy your permission set.
    customerManagedPolicyReference :: Prelude.Maybe CustomerManagedPolicyReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PermissionsBoundary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedPolicyArn', 'permissionsBoundary_managedPolicyArn' - The AWS managed policy ARN that you want to attach to a permission set
-- as a permissions boundary.
--
-- 'customerManagedPolicyReference', 'permissionsBoundary_customerManagedPolicyReference' - Specifies the name and path of a customer managed policy. You must have
-- an IAM policy that matches the name and path in each AWS account where
-- you want to deploy your permission set.
newPermissionsBoundary ::
  PermissionsBoundary
newPermissionsBoundary =
  PermissionsBoundary'
    { managedPolicyArn =
        Prelude.Nothing,
      customerManagedPolicyReference = Prelude.Nothing
    }

-- | The AWS managed policy ARN that you want to attach to a permission set
-- as a permissions boundary.
permissionsBoundary_managedPolicyArn :: Lens.Lens' PermissionsBoundary (Prelude.Maybe Prelude.Text)
permissionsBoundary_managedPolicyArn = Lens.lens (\PermissionsBoundary' {managedPolicyArn} -> managedPolicyArn) (\s@PermissionsBoundary' {} a -> s {managedPolicyArn = a} :: PermissionsBoundary)

-- | Specifies the name and path of a customer managed policy. You must have
-- an IAM policy that matches the name and path in each AWS account where
-- you want to deploy your permission set.
permissionsBoundary_customerManagedPolicyReference :: Lens.Lens' PermissionsBoundary (Prelude.Maybe CustomerManagedPolicyReference)
permissionsBoundary_customerManagedPolicyReference = Lens.lens (\PermissionsBoundary' {customerManagedPolicyReference} -> customerManagedPolicyReference) (\s@PermissionsBoundary' {} a -> s {customerManagedPolicyReference = a} :: PermissionsBoundary)

instance Core.FromJSON PermissionsBoundary where
  parseJSON =
    Core.withObject
      "PermissionsBoundary"
      ( \x ->
          PermissionsBoundary'
            Prelude.<$> (x Core..:? "ManagedPolicyArn")
            Prelude.<*> (x Core..:? "CustomerManagedPolicyReference")
      )

instance Prelude.Hashable PermissionsBoundary where
  hashWithSalt _salt PermissionsBoundary' {..} =
    _salt `Prelude.hashWithSalt` managedPolicyArn
      `Prelude.hashWithSalt` customerManagedPolicyReference

instance Prelude.NFData PermissionsBoundary where
  rnf PermissionsBoundary' {..} =
    Prelude.rnf managedPolicyArn
      `Prelude.seq` Prelude.rnf customerManagedPolicyReference

instance Core.ToJSON PermissionsBoundary where
  toJSON PermissionsBoundary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ManagedPolicyArn" Core..=)
              Prelude.<$> managedPolicyArn,
            ("CustomerManagedPolicyReference" Core..=)
              Prelude.<$> customerManagedPolicyReference
          ]
      )
