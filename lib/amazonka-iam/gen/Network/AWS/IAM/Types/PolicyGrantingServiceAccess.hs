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
    pgsaEntityName,
    pgsaEntityType,
    pgsaPolicyARN,
    pgsaPolicyName,
    pgsaPolicyType,
  )
where

import Network.AWS.IAM.Types.PolicyOwnerEntityType
import Network.AWS.IAM.Types.PolicyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the permissions policies that are attached to the specified identity (user, group, or role).
--
-- This data type is an element of the 'ListPoliciesGrantingServiceAccessEntry' object.
--
-- /See:/ 'mkPolicyGrantingServiceAccess' smart constructor.
data PolicyGrantingServiceAccess = PolicyGrantingServiceAccess'
  { entityName ::
      Lude.Maybe Lude.Text,
    entityType ::
      Lude.Maybe PolicyOwnerEntityType,
    policyARN :: Lude.Maybe Lude.Text,
    policyName :: Lude.Text,
    policyType :: PolicyType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyGrantingServiceAccess' with the minimum fields required to make a request.
--
-- * 'entityName' - The name of the entity (user or role) to which the inline policy is attached.
--
-- This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- * 'entityType' - The type of entity (user or role) that used the policy to access the service to which the inline policy is attached.
--
-- This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- * 'policyARN' - Undocumented field.
-- * 'policyName' - The policy name.
-- * 'policyType' - The policy type. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
mkPolicyGrantingServiceAccess ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyType'
  PolicyType ->
  PolicyGrantingServiceAccess
mkPolicyGrantingServiceAccess pPolicyName_ pPolicyType_ =
  PolicyGrantingServiceAccess'
    { entityName = Lude.Nothing,
      entityType = Lude.Nothing,
      policyARN = Lude.Nothing,
      policyName = pPolicyName_,
      policyType = pPolicyType_
    }

-- | The name of the entity (user or role) to which the inline policy is attached.
--
-- This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'entityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaEntityName :: Lens.Lens' PolicyGrantingServiceAccess (Lude.Maybe Lude.Text)
pgsaEntityName = Lens.lens (entityName :: PolicyGrantingServiceAccess -> Lude.Maybe Lude.Text) (\s a -> s {entityName = a} :: PolicyGrantingServiceAccess)
{-# DEPRECATED pgsaEntityName "Use generic-lens or generic-optics with 'entityName' instead." #-}

-- | The type of entity (user or role) that used the policy to access the service to which the inline policy is attached.
--
-- This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'entityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaEntityType :: Lens.Lens' PolicyGrantingServiceAccess (Lude.Maybe PolicyOwnerEntityType)
pgsaEntityType = Lens.lens (entityType :: PolicyGrantingServiceAccess -> Lude.Maybe PolicyOwnerEntityType) (\s a -> s {entityType = a} :: PolicyGrantingServiceAccess)
{-# DEPRECATED pgsaEntityType "Use generic-lens or generic-optics with 'entityType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaPolicyARN :: Lens.Lens' PolicyGrantingServiceAccess (Lude.Maybe Lude.Text)
pgsaPolicyARN = Lens.lens (policyARN :: PolicyGrantingServiceAccess -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: PolicyGrantingServiceAccess)
{-# DEPRECATED pgsaPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaPolicyName :: Lens.Lens' PolicyGrantingServiceAccess Lude.Text
pgsaPolicyName = Lens.lens (policyName :: PolicyGrantingServiceAccess -> Lude.Text) (\s a -> s {policyName = a} :: PolicyGrantingServiceAccess)
{-# DEPRECATED pgsaPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy type. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsaPolicyType :: Lens.Lens' PolicyGrantingServiceAccess PolicyType
pgsaPolicyType = Lens.lens (policyType :: PolicyGrantingServiceAccess -> PolicyType) (\s a -> s {policyType = a} :: PolicyGrantingServiceAccess)
{-# DEPRECATED pgsaPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

instance Lude.FromXML PolicyGrantingServiceAccess where
  parseXML x =
    PolicyGrantingServiceAccess'
      Lude.<$> (x Lude..@? "EntityName")
      Lude.<*> (x Lude..@? "EntityType")
      Lude.<*> (x Lude..@? "PolicyArn")
      Lude.<*> (x Lude..@ "PolicyName")
      Lude.<*> (x Lude..@ "PolicyType")
