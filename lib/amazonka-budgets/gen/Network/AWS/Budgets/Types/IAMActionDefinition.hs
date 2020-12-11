-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.IAMActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.IAMActionDefinition
  ( IAMActionDefinition (..),

    -- * Smart constructor
    mkIAMActionDefinition,

    -- * Lenses
    iadGroups,
    iadRoles,
    iadUsers,
    iadPolicyARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AWS Identity and Access Management (IAM) action definition details.
--
-- /See:/ 'mkIAMActionDefinition' smart constructor.
data IAMActionDefinition = IAMActionDefinition'
  { groups ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    roles :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    users :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    policyARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IAMActionDefinition' with the minimum fields required to make a request.
--
-- * 'groups' - A list of groups to be attached. There must be at least one group.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the policy to be attached.
-- * 'roles' - A list of roles to be attached. There must be at least one role.
-- * 'users' - A list of users to be attached. There must be at least one user.
mkIAMActionDefinition ::
  -- | 'policyARN'
  Lude.Text ->
  IAMActionDefinition
mkIAMActionDefinition pPolicyARN_ =
  IAMActionDefinition'
    { groups = Lude.Nothing,
      roles = Lude.Nothing,
      users = Lude.Nothing,
      policyARN = pPolicyARN_
    }

-- | A list of groups to be attached. There must be at least one group.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadGroups :: Lens.Lens' IAMActionDefinition (Lude.Maybe (Lude.NonEmpty Lude.Text))
iadGroups = Lens.lens (groups :: IAMActionDefinition -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {groups = a} :: IAMActionDefinition)
{-# DEPRECATED iadGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | A list of roles to be attached. There must be at least one role.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadRoles :: Lens.Lens' IAMActionDefinition (Lude.Maybe (Lude.NonEmpty Lude.Text))
iadRoles = Lens.lens (roles :: IAMActionDefinition -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {roles = a} :: IAMActionDefinition)
{-# DEPRECATED iadRoles "Use generic-lens or generic-optics with 'roles' instead." #-}

-- | A list of users to be attached. There must be at least one user.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadUsers :: Lens.Lens' IAMActionDefinition (Lude.Maybe (Lude.NonEmpty Lude.Text))
iadUsers = Lens.lens (users :: IAMActionDefinition -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {users = a} :: IAMActionDefinition)
{-# DEPRECATED iadUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The Amazon Resource Name (ARN) of the policy to be attached.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadPolicyARN :: Lens.Lens' IAMActionDefinition Lude.Text
iadPolicyARN = Lens.lens (policyARN :: IAMActionDefinition -> Lude.Text) (\s a -> s {policyARN = a} :: IAMActionDefinition)
{-# DEPRECATED iadPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.FromJSON IAMActionDefinition where
  parseJSON =
    Lude.withObject
      "IAMActionDefinition"
      ( \x ->
          IAMActionDefinition'
            Lude.<$> (x Lude..:? "Groups")
            Lude.<*> (x Lude..:? "Roles")
            Lude.<*> (x Lude..:? "Users")
            Lude.<*> (x Lude..: "PolicyArn")
      )

instance Lude.ToJSON IAMActionDefinition where
  toJSON IAMActionDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Groups" Lude..=) Lude.<$> groups,
            ("Roles" Lude..=) Lude.<$> roles,
            ("Users" Lude..=) Lude.<$> users,
            Lude.Just ("PolicyArn" Lude..= policyARN)
          ]
      )
