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
-- Module      : Amazonka.IAM.Types.PolicyGrantingServiceAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.PolicyGrantingServiceAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.PolicyOwnerEntityType
import Amazonka.IAM.Types.PolicyType
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the permissions policies that are attached to the
-- specified identity (user, group, or role).
--
-- This data type is an element of the
-- ListPoliciesGrantingServiceAccessEntry object.
--
-- /See:/ 'newPolicyGrantingServiceAccess' smart constructor.
data PolicyGrantingServiceAccess = PolicyGrantingServiceAccess'
  { -- | The name of the entity (user or role) to which the inline policy is
    -- attached.
    --
    -- This field is null for managed policies. For more information about
    -- these policy types, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
    -- in the /IAM User Guide/.
    entityName :: Prelude.Maybe Prelude.Text,
    -- | The type of entity (user or role) that used the policy to access the
    -- service to which the inline policy is attached.
    --
    -- This field is null for managed policies. For more information about
    -- these policy types, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
    -- in the /IAM User Guide/.
    entityType :: Prelude.Maybe PolicyOwnerEntityType,
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The policy name.
    policyName :: Prelude.Text,
    -- | The policy type. For more information about these policy types, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
    -- in the /IAM User Guide/.
    policyType :: PolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyGrantingServiceAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityName', 'policyGrantingServiceAccess_entityName' - The name of the entity (user or role) to which the inline policy is
-- attached.
--
-- This field is null for managed policies. For more information about
-- these policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- 'entityType', 'policyGrantingServiceAccess_entityType' - The type of entity (user or role) that used the policy to access the
-- service to which the inline policy is attached.
--
-- This field is null for managed policies. For more information about
-- these policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- 'policyArn', 'policyGrantingServiceAccess_policyArn' - Undocumented member.
--
-- 'policyName', 'policyGrantingServiceAccess_policyName' - The policy name.
--
-- 'policyType', 'policyGrantingServiceAccess_policyType' - The policy type. For more information about these policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
newPolicyGrantingServiceAccess ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyType'
  PolicyType ->
  PolicyGrantingServiceAccess
newPolicyGrantingServiceAccess
  pPolicyName_
  pPolicyType_ =
    PolicyGrantingServiceAccess'
      { entityName =
          Prelude.Nothing,
        entityType = Prelude.Nothing,
        policyArn = Prelude.Nothing,
        policyName = pPolicyName_,
        policyType = pPolicyType_
      }

-- | The name of the entity (user or role) to which the inline policy is
-- attached.
--
-- This field is null for managed policies. For more information about
-- these policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
policyGrantingServiceAccess_entityName :: Lens.Lens' PolicyGrantingServiceAccess (Prelude.Maybe Prelude.Text)
policyGrantingServiceAccess_entityName = Lens.lens (\PolicyGrantingServiceAccess' {entityName} -> entityName) (\s@PolicyGrantingServiceAccess' {} a -> s {entityName = a} :: PolicyGrantingServiceAccess)

-- | The type of entity (user or role) that used the policy to access the
-- service to which the inline policy is attached.
--
-- This field is null for managed policies. For more information about
-- these policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
policyGrantingServiceAccess_entityType :: Lens.Lens' PolicyGrantingServiceAccess (Prelude.Maybe PolicyOwnerEntityType)
policyGrantingServiceAccess_entityType = Lens.lens (\PolicyGrantingServiceAccess' {entityType} -> entityType) (\s@PolicyGrantingServiceAccess' {} a -> s {entityType = a} :: PolicyGrantingServiceAccess)

-- | Undocumented member.
policyGrantingServiceAccess_policyArn :: Lens.Lens' PolicyGrantingServiceAccess (Prelude.Maybe Prelude.Text)
policyGrantingServiceAccess_policyArn = Lens.lens (\PolicyGrantingServiceAccess' {policyArn} -> policyArn) (\s@PolicyGrantingServiceAccess' {} a -> s {policyArn = a} :: PolicyGrantingServiceAccess)

-- | The policy name.
policyGrantingServiceAccess_policyName :: Lens.Lens' PolicyGrantingServiceAccess Prelude.Text
policyGrantingServiceAccess_policyName = Lens.lens (\PolicyGrantingServiceAccess' {policyName} -> policyName) (\s@PolicyGrantingServiceAccess' {} a -> s {policyName = a} :: PolicyGrantingServiceAccess)

-- | The policy type. For more information about these policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
policyGrantingServiceAccess_policyType :: Lens.Lens' PolicyGrantingServiceAccess PolicyType
policyGrantingServiceAccess_policyType = Lens.lens (\PolicyGrantingServiceAccess' {policyType} -> policyType) (\s@PolicyGrantingServiceAccess' {} a -> s {policyType = a} :: PolicyGrantingServiceAccess)

instance Data.FromXML PolicyGrantingServiceAccess where
  parseXML x =
    PolicyGrantingServiceAccess'
      Prelude.<$> (x Data..@? "EntityName")
      Prelude.<*> (x Data..@? "EntityType")
      Prelude.<*> (x Data..@? "PolicyArn")
      Prelude.<*> (x Data..@ "PolicyName")
      Prelude.<*> (x Data..@ "PolicyType")

instance Prelude.Hashable PolicyGrantingServiceAccess where
  hashWithSalt _salt PolicyGrantingServiceAccess' {..} =
    _salt
      `Prelude.hashWithSalt` entityName
      `Prelude.hashWithSalt` entityType
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData PolicyGrantingServiceAccess where
  rnf PolicyGrantingServiceAccess' {..} =
    Prelude.rnf entityName
      `Prelude.seq` Prelude.rnf entityType
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyType
