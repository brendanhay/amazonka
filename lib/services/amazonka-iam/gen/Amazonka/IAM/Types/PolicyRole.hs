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
-- Module      : Amazonka.IAM.Types.PolicyRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.PolicyRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML PolicyRole where
  parseXML x =
    PolicyRole'
      Prelude.<$> (x Data..@? "RoleId")
      Prelude.<*> (x Data..@? "RoleName")

instance Prelude.Hashable PolicyRole where
  hashWithSalt _salt PolicyRole' {..} =
    _salt
      `Prelude.hashWithSalt` roleId
      `Prelude.hashWithSalt` roleName

instance Prelude.NFData PolicyRole where
  rnf PolicyRole' {..} =
    Prelude.rnf roleId `Prelude.seq`
      Prelude.rnf roleName
