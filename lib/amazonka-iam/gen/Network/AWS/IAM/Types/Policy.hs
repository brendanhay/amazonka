{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Policy
  ( Policy (..),

    -- * Smart constructor
    mkPolicy,

    -- * Lenses
    pPolicyName,
    pARN,
    pUpdateDate,
    pPolicyId,
    pPath,
    pCreateDate,
    pIsAttachable,
    pPermissionsBoundaryUsageCount,
    pDefaultVersionId,
    pAttachmentCount,
    pDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a managed policy.
--
-- This data type is used as a response element in the 'CreatePolicy' , 'GetPolicy' , and 'ListPolicies' operations.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkPolicy' smart constructor.
data Policy = Policy'
  { -- | The friendly name (not ARN) identifying the policy.
    policyName :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
    --
    -- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
    updateDate :: Lude.Maybe Lude.DateTime,
    -- | The stable and unique string identifying the policy.
    --
    -- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    policyId :: Lude.Maybe Lude.Text,
    -- | The path to the policy.
    --
    -- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Lude.Maybe Lude.Text,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
    createDate :: Lude.Maybe Lude.DateTime,
    -- | Specifies whether the policy can be attached to an IAM user, group, or role.
    isAttachable :: Lude.Maybe Lude.Bool,
    -- | The number of entities (users and roles) for which the policy is used to set the permissions boundary.
    --
    -- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
    permissionsBoundaryUsageCount :: Lude.Maybe Lude.Int,
    -- | The identifier for the version of the policy that is set as the default version.
    defaultVersionId :: Lude.Maybe Lude.Text,
    -- | The number of entities (users, groups, and roles) that the policy is attached to.
    attachmentCount :: Lude.Maybe Lude.Int,
    -- | A friendly description of the policy.
    --
    -- This element is included in the response to the 'GetPolicy' operation. It is not included in the response to the 'ListPolicies' operation.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Policy' with the minimum fields required to make a request.
--
-- * 'policyName' - The friendly name (not ARN) identifying the policy.
-- * 'arn' -
-- * 'updateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
--
-- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
-- * 'policyId' - The stable and unique string identifying the policy.
--
-- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'path' - The path to the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
-- * 'isAttachable' - Specifies whether the policy can be attached to an IAM user, group, or role.
-- * 'permissionsBoundaryUsageCount' - The number of entities (users and roles) for which the policy is used to set the permissions boundary.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
-- * 'defaultVersionId' - The identifier for the version of the policy that is set as the default version.
-- * 'attachmentCount' - The number of entities (users, groups, and roles) that the policy is attached to.
-- * 'description' - A friendly description of the policy.
--
-- This element is included in the response to the 'GetPolicy' operation. It is not included in the response to the 'ListPolicies' operation.
mkPolicy ::
  Policy
mkPolicy =
  Policy'
    { policyName = Lude.Nothing,
      arn = Lude.Nothing,
      updateDate = Lude.Nothing,
      policyId = Lude.Nothing,
      path = Lude.Nothing,
      createDate = Lude.Nothing,
      isAttachable = Lude.Nothing,
      permissionsBoundaryUsageCount = Lude.Nothing,
      defaultVersionId = Lude.Nothing,
      attachmentCount = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyName :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pPolicyName = Lens.lens (policyName :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: Policy)
{-# DEPRECATED pPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pARN :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pARN = Lens.lens (arn :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Policy)
{-# DEPRECATED pARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
--
-- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pUpdateDate :: Lens.Lens' Policy (Lude.Maybe Lude.DateTime)
pUpdateDate = Lens.lens (updateDate :: Policy -> Lude.Maybe Lude.DateTime) (\s a -> s {updateDate = a} :: Policy)
{-# DEPRECATED pUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyId :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pPolicyId = Lens.lens (policyId :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {policyId = a} :: Policy)
{-# DEPRECATED pPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The path to the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPath :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pPath = Lens.lens (path :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: Policy)
{-# DEPRECATED pPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreateDate :: Lens.Lens' Policy (Lude.Maybe Lude.DateTime)
pCreateDate = Lens.lens (createDate :: Policy -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: Policy)
{-# DEPRECATED pCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | Specifies whether the policy can be attached to an IAM user, group, or role.
--
-- /Note:/ Consider using 'isAttachable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsAttachable :: Lens.Lens' Policy (Lude.Maybe Lude.Bool)
pIsAttachable = Lens.lens (isAttachable :: Policy -> Lude.Maybe Lude.Bool) (\s a -> s {isAttachable = a} :: Policy)
{-# DEPRECATED pIsAttachable "Use generic-lens or generic-optics with 'isAttachable' instead." #-}

-- | The number of entities (users and roles) for which the policy is used to set the permissions boundary.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundaryUsageCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPermissionsBoundaryUsageCount :: Lens.Lens' Policy (Lude.Maybe Lude.Int)
pPermissionsBoundaryUsageCount = Lens.lens (permissionsBoundaryUsageCount :: Policy -> Lude.Maybe Lude.Int) (\s a -> s {permissionsBoundaryUsageCount = a} :: Policy)
{-# DEPRECATED pPermissionsBoundaryUsageCount "Use generic-lens or generic-optics with 'permissionsBoundaryUsageCount' instead." #-}

-- | The identifier for the version of the policy that is set as the default version.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDefaultVersionId :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pDefaultVersionId = Lens.lens (defaultVersionId :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersionId = a} :: Policy)
{-# DEPRECATED pDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The number of entities (users, groups, and roles) that the policy is attached to.
--
-- /Note:/ Consider using 'attachmentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAttachmentCount :: Lens.Lens' Policy (Lude.Maybe Lude.Int)
pAttachmentCount = Lens.lens (attachmentCount :: Policy -> Lude.Maybe Lude.Int) (\s a -> s {attachmentCount = a} :: Policy)
{-# DEPRECATED pAttachmentCount "Use generic-lens or generic-optics with 'attachmentCount' instead." #-}

-- | A friendly description of the policy.
--
-- This element is included in the response to the 'GetPolicy' operation. It is not included in the response to the 'ListPolicies' operation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Policy (Lude.Maybe Lude.Text)
pDescription = Lens.lens (description :: Policy -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Policy)
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML Policy where
  parseXML x =
    Policy'
      Lude.<$> (x Lude..@? "PolicyName")
      Lude.<*> (x Lude..@? "Arn")
      Lude.<*> (x Lude..@? "UpdateDate")
      Lude.<*> (x Lude..@? "PolicyId")
      Lude.<*> (x Lude..@? "Path")
      Lude.<*> (x Lude..@? "CreateDate")
      Lude.<*> (x Lude..@? "IsAttachable")
      Lude.<*> (x Lude..@? "PermissionsBoundaryUsageCount")
      Lude.<*> (x Lude..@? "DefaultVersionId")
      Lude.<*> (x Lude..@? "AttachmentCount")
      Lude.<*> (x Lude..@? "Description")
