{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyGrantingServiceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyGrantingServiceAccess
  ( PolicyGrantingServiceAccess (..),

    -- * Smart constructor
    mkPolicyGrantingServiceAccess,

    -- * Lenses
    pgsaPolicyName,
    pgsaPolicyType,
    pgsaEntityName,
    pgsaEntityType,
    pgsaPolicyArn,
  )
where

import qualified Network.AWS.IAM.Types.EntityName as Types
import qualified Network.AWS.IAM.Types.PolicyArn as Types
import qualified Network.AWS.IAM.Types.PolicyName as Types
import qualified Network.AWS.IAM.Types.PolicyOwnerEntityType as Types
import qualified Network.AWS.IAM.Types.PolicyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the permissions policies that are attached to the specified identity (user, group, or role).
--
-- This data type is an element of the 'ListPoliciesGrantingServiceAccessEntry' object.
--
-- /See:/ 'mkPolicyGrantingServiceAccess' smart constructor.
data PolicyGrantingServiceAccess = PolicyGrantingServiceAccess'
  { -- | The policy name.
    policyName :: Types.PolicyName,
    -- | The policy type. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
    policyType :: Types.PolicyType,
    -- | The name of the entity (user or role) to which the inline policy is attached.
    --
    -- This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
    entityName :: Core.Maybe Types.EntityName,
    -- | The type of entity (user or role) that used the policy to access the service to which the inline policy is attached.
    --
    -- This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
    entityType :: Core.Maybe Types.PolicyOwnerEntityType,
    policyArn :: Core.Maybe Types.PolicyArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyGrantingServiceAccess' value with any optional fields omitted.
mkPolicyGrantingServiceAccess ::
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyType'
  Types.PolicyType ->
  PolicyGrantingServiceAccess
mkPolicyGrantingServiceAccess policyName policyType =
  PolicyGrantingServiceAccess'
    { policyName,
      policyType,
      entityName = Core.Nothing,
      entityType = Core.Nothing,
      policyArn = Core.Nothing
    }

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaPolicyName :: Lens.Lens' PolicyGrantingServiceAccess Types.PolicyName
pgsaPolicyName = Lens.field @"policyName"
{-# DEPRECATED pgsaPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy type. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaPolicyType :: Lens.Lens' PolicyGrantingServiceAccess Types.PolicyType
pgsaPolicyType = Lens.field @"policyType"
{-# DEPRECATED pgsaPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | The name of the entity (user or role) to which the inline policy is attached.
--
-- This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'entityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaEntityName :: Lens.Lens' PolicyGrantingServiceAccess (Core.Maybe Types.EntityName)
pgsaEntityName = Lens.field @"entityName"
{-# DEPRECATED pgsaEntityName "Use generic-lens or generic-optics with 'entityName' instead." #-}

-- | The type of entity (user or role) that used the policy to access the service to which the inline policy is attached.
--
-- This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'entityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaEntityType :: Lens.Lens' PolicyGrantingServiceAccess (Core.Maybe Types.PolicyOwnerEntityType)
pgsaEntityType = Lens.field @"entityType"
{-# DEPRECATED pgsaEntityType "Use generic-lens or generic-optics with 'entityType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaPolicyArn :: Lens.Lens' PolicyGrantingServiceAccess (Core.Maybe Types.PolicyArn)
pgsaPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED pgsaPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

instance Core.FromXML PolicyGrantingServiceAccess where
  parseXML x =
    PolicyGrantingServiceAccess'
      Core.<$> (x Core..@ "PolicyName")
      Core.<*> (x Core..@ "PolicyType")
      Core.<*> (x Core..@? "EntityName")
      Core.<*> (x Core..@? "EntityType")
      Core.<*> (x Core..@? "PolicyArn")
