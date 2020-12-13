{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyRole
  ( PolicyRole (..),

    -- * Smart constructor
    mkPolicyRole,

    -- * Lenses
    prRoleName,
    prRoleId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a role that a managed policy is attached to.
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkPolicyRole' smart constructor.
data PolicyRole = PolicyRole'
  { -- | The name (friendly name, not ARN) identifying the role.
    roleName :: Lude.Maybe Lude.Text,
    -- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    roleId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyRole' with the minimum fields required to make a request.
--
-- * 'roleName' - The name (friendly name, not ARN) identifying the role.
-- * 'roleId' - The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
mkPolicyRole ::
  PolicyRole
mkPolicyRole =
  PolicyRole' {roleName = Lude.Nothing, roleId = Lude.Nothing}

-- | The name (friendly name, not ARN) identifying the role.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prRoleName :: Lens.Lens' PolicyRole (Lude.Maybe Lude.Text)
prRoleName = Lens.lens (roleName :: PolicyRole -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: PolicyRole)
{-# DEPRECATED prRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prRoleId :: Lens.Lens' PolicyRole (Lude.Maybe Lude.Text)
prRoleId = Lens.lens (roleId :: PolicyRole -> Lude.Maybe Lude.Text) (\s a -> s {roleId = a} :: PolicyRole)
{-# DEPRECATED prRoleId "Use generic-lens or generic-optics with 'roleId' instead." #-}

instance Lude.FromXML PolicyRole where
  parseXML x =
    PolicyRole'
      Lude.<$> (x Lude..@? "RoleName") Lude.<*> (x Lude..@? "RoleId")
