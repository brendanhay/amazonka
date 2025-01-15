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
-- Module      : Amazonka.Budgets.Types.IamActionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.IamActionDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Identity and Access Management (IAM) action definition details.
--
-- /See:/ 'newIamActionDefinition' smart constructor.
data IamActionDefinition = IamActionDefinition'
  { -- | A list of groups to be attached. There must be at least one group.
    groups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of roles to be attached. There must be at least one role.
    roles :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of users to be attached. There must be at least one user.
    users :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the policy to be attached.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IamActionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'iamActionDefinition_groups' - A list of groups to be attached. There must be at least one group.
--
-- 'roles', 'iamActionDefinition_roles' - A list of roles to be attached. There must be at least one role.
--
-- 'users', 'iamActionDefinition_users' - A list of users to be attached. There must be at least one user.
--
-- 'policyArn', 'iamActionDefinition_policyArn' - The Amazon Resource Name (ARN) of the policy to be attached.
newIamActionDefinition ::
  -- | 'policyArn'
  Prelude.Text ->
  IamActionDefinition
newIamActionDefinition pPolicyArn_ =
  IamActionDefinition'
    { groups = Prelude.Nothing,
      roles = Prelude.Nothing,
      users = Prelude.Nothing,
      policyArn = pPolicyArn_
    }

-- | A list of groups to be attached. There must be at least one group.
iamActionDefinition_groups :: Lens.Lens' IamActionDefinition (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
iamActionDefinition_groups = Lens.lens (\IamActionDefinition' {groups} -> groups) (\s@IamActionDefinition' {} a -> s {groups = a} :: IamActionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | A list of roles to be attached. There must be at least one role.
iamActionDefinition_roles :: Lens.Lens' IamActionDefinition (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
iamActionDefinition_roles = Lens.lens (\IamActionDefinition' {roles} -> roles) (\s@IamActionDefinition' {} a -> s {roles = a} :: IamActionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | A list of users to be attached. There must be at least one user.
iamActionDefinition_users :: Lens.Lens' IamActionDefinition (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
iamActionDefinition_users = Lens.lens (\IamActionDefinition' {users} -> users) (\s@IamActionDefinition' {} a -> s {users = a} :: IamActionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the policy to be attached.
iamActionDefinition_policyArn :: Lens.Lens' IamActionDefinition Prelude.Text
iamActionDefinition_policyArn = Lens.lens (\IamActionDefinition' {policyArn} -> policyArn) (\s@IamActionDefinition' {} a -> s {policyArn = a} :: IamActionDefinition)

instance Data.FromJSON IamActionDefinition where
  parseJSON =
    Data.withObject
      "IamActionDefinition"
      ( \x ->
          IamActionDefinition'
            Prelude.<$> (x Data..:? "Groups")
            Prelude.<*> (x Data..:? "Roles")
            Prelude.<*> (x Data..:? "Users")
            Prelude.<*> (x Data..: "PolicyArn")
      )

instance Prelude.Hashable IamActionDefinition where
  hashWithSalt _salt IamActionDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` roles
      `Prelude.hashWithSalt` users
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData IamActionDefinition where
  rnf IamActionDefinition' {..} =
    Prelude.rnf groups `Prelude.seq`
      Prelude.rnf roles `Prelude.seq`
        Prelude.rnf users `Prelude.seq`
          Prelude.rnf policyArn

instance Data.ToJSON IamActionDefinition where
  toJSON IamActionDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Groups" Data..=) Prelude.<$> groups,
            ("Roles" Data..=) Prelude.<$> roles,
            ("Users" Data..=) Prelude.<$> users,
            Prelude.Just ("PolicyArn" Data..= policyArn)
          ]
      )
