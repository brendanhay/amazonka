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
-- Module      : Network.AWS.Budgets.Types.IamActionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.IamActionDefinition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The AWS Identity and Access Management (IAM) action definition details.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
iamActionDefinition_groups = Lens.lens (\IamActionDefinition' {groups} -> groups) (\s@IamActionDefinition' {} a -> s {groups = a} :: IamActionDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of roles to be attached. There must be at least one role.
iamActionDefinition_roles :: Lens.Lens' IamActionDefinition (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
iamActionDefinition_roles = Lens.lens (\IamActionDefinition' {roles} -> roles) (\s@IamActionDefinition' {} a -> s {roles = a} :: IamActionDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of users to be attached. There must be at least one user.
iamActionDefinition_users :: Lens.Lens' IamActionDefinition (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
iamActionDefinition_users = Lens.lens (\IamActionDefinition' {users} -> users) (\s@IamActionDefinition' {} a -> s {users = a} :: IamActionDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the policy to be attached.
iamActionDefinition_policyArn :: Lens.Lens' IamActionDefinition Prelude.Text
iamActionDefinition_policyArn = Lens.lens (\IamActionDefinition' {policyArn} -> policyArn) (\s@IamActionDefinition' {} a -> s {policyArn = a} :: IamActionDefinition)

instance Prelude.FromJSON IamActionDefinition where
  parseJSON =
    Prelude.withObject
      "IamActionDefinition"
      ( \x ->
          IamActionDefinition'
            Prelude.<$> (x Prelude..:? "Groups")
            Prelude.<*> (x Prelude..:? "Roles")
            Prelude.<*> (x Prelude..:? "Users")
            Prelude.<*> (x Prelude..: "PolicyArn")
      )

instance Prelude.Hashable IamActionDefinition

instance Prelude.NFData IamActionDefinition

instance Prelude.ToJSON IamActionDefinition where
  toJSON IamActionDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Groups" Prelude..=) Prelude.<$> groups,
            ("Roles" Prelude..=) Prelude.<$> roles,
            ("Users" Prelude..=) Prelude.<$> users,
            Prelude.Just ("PolicyArn" Prelude..= policyArn)
          ]
      )
