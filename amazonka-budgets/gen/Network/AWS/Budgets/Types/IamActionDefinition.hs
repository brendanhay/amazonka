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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The AWS Identity and Access Management (IAM) action definition details.
--
-- /See:/ 'newIamActionDefinition' smart constructor.
data IamActionDefinition = IamActionDefinition'
  { -- | A list of groups to be attached. There must be at least one group.
    groups :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of roles to be attached. There must be at least one role.
    roles :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of users to be attached. There must be at least one user.
    users :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The Amazon Resource Name (ARN) of the policy to be attached.
    policyArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  IamActionDefinition
newIamActionDefinition pPolicyArn_ =
  IamActionDefinition'
    { groups = Core.Nothing,
      roles = Core.Nothing,
      users = Core.Nothing,
      policyArn = pPolicyArn_
    }

-- | A list of groups to be attached. There must be at least one group.
iamActionDefinition_groups :: Lens.Lens' IamActionDefinition (Core.Maybe (Core.NonEmpty Core.Text))
iamActionDefinition_groups = Lens.lens (\IamActionDefinition' {groups} -> groups) (\s@IamActionDefinition' {} a -> s {groups = a} :: IamActionDefinition) Core.. Lens.mapping Lens._Coerce

-- | A list of roles to be attached. There must be at least one role.
iamActionDefinition_roles :: Lens.Lens' IamActionDefinition (Core.Maybe (Core.NonEmpty Core.Text))
iamActionDefinition_roles = Lens.lens (\IamActionDefinition' {roles} -> roles) (\s@IamActionDefinition' {} a -> s {roles = a} :: IamActionDefinition) Core.. Lens.mapping Lens._Coerce

-- | A list of users to be attached. There must be at least one user.
iamActionDefinition_users :: Lens.Lens' IamActionDefinition (Core.Maybe (Core.NonEmpty Core.Text))
iamActionDefinition_users = Lens.lens (\IamActionDefinition' {users} -> users) (\s@IamActionDefinition' {} a -> s {users = a} :: IamActionDefinition) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the policy to be attached.
iamActionDefinition_policyArn :: Lens.Lens' IamActionDefinition Core.Text
iamActionDefinition_policyArn = Lens.lens (\IamActionDefinition' {policyArn} -> policyArn) (\s@IamActionDefinition' {} a -> s {policyArn = a} :: IamActionDefinition)

instance Core.FromJSON IamActionDefinition where
  parseJSON =
    Core.withObject
      "IamActionDefinition"
      ( \x ->
          IamActionDefinition'
            Core.<$> (x Core..:? "Groups")
            Core.<*> (x Core..:? "Roles")
            Core.<*> (x Core..:? "Users")
            Core.<*> (x Core..: "PolicyArn")
      )

instance Core.Hashable IamActionDefinition

instance Core.NFData IamActionDefinition

instance Core.ToJSON IamActionDefinition where
  toJSON IamActionDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Groups" Core..=) Core.<$> groups,
            ("Roles" Core..=) Core.<$> roles,
            ("Users" Core..=) Core.<$> users,
            Core.Just ("PolicyArn" Core..= policyArn)
          ]
      )
