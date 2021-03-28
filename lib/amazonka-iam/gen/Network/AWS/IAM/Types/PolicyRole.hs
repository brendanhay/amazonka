{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.PolicyRole
  ( PolicyRole (..)
  -- * Smart constructor
  , mkPolicyRole
  -- * Lenses
  , prRoleId
  , prRoleName
  ) where

import qualified Network.AWS.IAM.Types.RoleId as Types
import qualified Network.AWS.IAM.Types.RoleName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a role that a managed policy is attached to.
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation. 
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ . 
--
-- /See:/ 'mkPolicyRole' smart constructor.
data PolicyRole = PolicyRole'
  { roleId :: Core.Maybe Types.RoleId
    -- ^ The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
  , roleName :: Core.Maybe Types.RoleName
    -- ^ The name (friendly name, not ARN) identifying the role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyRole' value with any optional fields omitted.
mkPolicyRole
    :: PolicyRole
mkPolicyRole
  = PolicyRole'{roleId = Core.Nothing, roleName = Core.Nothing}

-- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prRoleId :: Lens.Lens' PolicyRole (Core.Maybe Types.RoleId)
prRoleId = Lens.field @"roleId"
{-# INLINEABLE prRoleId #-}
{-# DEPRECATED roleId "Use generic-lens or generic-optics with 'roleId' instead"  #-}

-- | The name (friendly name, not ARN) identifying the role.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prRoleName :: Lens.Lens' PolicyRole (Core.Maybe Types.RoleName)
prRoleName = Lens.field @"roleName"
{-# INLINEABLE prRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

instance Core.FromXML PolicyRole where
        parseXML x
          = PolicyRole' Core.<$>
              (x Core..@? "RoleId") Core.<*> x Core..@? "RoleName"
