{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.Types.PolicyRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyRole where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a role that a managed policy is attached to.
--
-- This data type is used as a response element in the
-- ListEntitiesForPolicy operation.
--
-- For more information about managed policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- /See:/ 'newPolicyRole' smart constructor.
data PolicyRole = PolicyRole'
  { -- | The stable and unique string identifying the role. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    roleId :: Prelude.Maybe Prelude.Text,
    -- | The name (friendly name, not ARN) identifying the role.
    roleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleId', 'policyRole_roleId' - The stable and unique string identifying the role. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'roleName', 'policyRole_roleName' - The name (friendly name, not ARN) identifying the role.
newPolicyRole ::
  PolicyRole
newPolicyRole =
  PolicyRole'
    { roleId = Prelude.Nothing,
      roleName = Prelude.Nothing
    }

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
policyRole_roleId :: Lens.Lens' PolicyRole (Prelude.Maybe Prelude.Text)
policyRole_roleId = Lens.lens (\PolicyRole' {roleId} -> roleId) (\s@PolicyRole' {} a -> s {roleId = a} :: PolicyRole)

-- | The name (friendly name, not ARN) identifying the role.
policyRole_roleName :: Lens.Lens' PolicyRole (Prelude.Maybe Prelude.Text)
policyRole_roleName = Lens.lens (\PolicyRole' {roleName} -> roleName) (\s@PolicyRole' {} a -> s {roleName = a} :: PolicyRole)

instance Prelude.FromXML PolicyRole where
  parseXML x =
    PolicyRole'
      Prelude.<$> (x Prelude..@? "RoleId")
      Prelude.<*> (x Prelude..@? "RoleName")

instance Prelude.Hashable PolicyRole

instance Prelude.NFData PolicyRole
