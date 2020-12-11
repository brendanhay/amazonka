-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ManagedPolicyDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ManagedPolicyDetail
  ( ManagedPolicyDetail (..),

    -- * Smart constructor
    mkManagedPolicyDetail,

    -- * Lenses
    mpdPolicyName,
    mpdARN,
    mpdUpdateDate,
    mpdPolicyId,
    mpdPath,
    mpdPolicyVersionList,
    mpdCreateDate,
    mpdIsAttachable,
    mpdPermissionsBoundaryUsageCount,
    mpdDefaultVersionId,
    mpdAttachmentCount,
    mpdDescription,
  )
where

import Network.AWS.IAM.Types.PolicyVersion
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a managed policy, including the policy's ARN, versions, and the number of principal entities (users, groups, and roles) that the policy is attached to.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
-- For more information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkManagedPolicyDetail' smart constructor.
data ManagedPolicyDetail = ManagedPolicyDetail'
  { policyName ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    updateDate :: Lude.Maybe Lude.ISO8601,
    policyId :: Lude.Maybe Lude.Text,
    path :: Lude.Maybe Lude.Text,
    policyVersionList :: Lude.Maybe [PolicyVersion],
    createDate :: Lude.Maybe Lude.ISO8601,
    isAttachable :: Lude.Maybe Lude.Bool,
    permissionsBoundaryUsageCount ::
      Lude.Maybe Lude.Int,
    defaultVersionId :: Lude.Maybe Lude.Text,
    attachmentCount :: Lude.Maybe Lude.Int,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ManagedPolicyDetail' with the minimum fields required to make a request.
--
-- * 'arn' - Undocumented field.
-- * 'attachmentCount' - The number of principal entities (users, groups, and roles) that the policy is attached to.
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
-- * 'defaultVersionId' - The identifier for the version of the policy that is set as the default (operative) version.
--
-- For more information about policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
-- * 'description' - A friendly description of the policy.
-- * 'isAttachable' - Specifies whether the policy can be attached to an IAM user, group, or role.
-- * 'path' - The path to the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'permissionsBoundaryUsageCount' - The number of entities (users and roles) for which the policy is used as the permissions boundary.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
-- * 'policyId' - The stable and unique string identifying the policy.
--
-- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'policyName' - The friendly name (not ARN) identifying the policy.
-- * 'policyVersionList' - A list containing information about the versions of the policy.
-- * 'updateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
--
-- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
mkManagedPolicyDetail ::
  ManagedPolicyDetail
mkManagedPolicyDetail =
  ManagedPolicyDetail'
    { policyName = Lude.Nothing,
      arn = Lude.Nothing,
      updateDate = Lude.Nothing,
      policyId = Lude.Nothing,
      path = Lude.Nothing,
      policyVersionList = Lude.Nothing,
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
mpdPolicyName :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Text)
mpdPolicyName = Lens.lens (policyName :: ManagedPolicyDetail -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdARN :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Text)
mpdARN = Lens.lens (arn :: ManagedPolicyDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
--
-- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdUpdateDate :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.ISO8601)
mpdUpdateDate = Lens.lens (updateDate :: ManagedPolicyDetail -> Lude.Maybe Lude.ISO8601) (\s a -> s {updateDate = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPolicyId :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Text)
mpdPolicyId = Lens.lens (policyId :: ManagedPolicyDetail -> Lude.Maybe Lude.Text) (\s a -> s {policyId = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The path to the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPath :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Text)
mpdPath = Lens.lens (path :: ManagedPolicyDetail -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | A list containing information about the versions of the policy.
--
-- /Note:/ Consider using 'policyVersionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPolicyVersionList :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe [PolicyVersion])
mpdPolicyVersionList = Lens.lens (policyVersionList :: ManagedPolicyDetail -> Lude.Maybe [PolicyVersion]) (\s a -> s {policyVersionList = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdPolicyVersionList "Use generic-lens or generic-optics with 'policyVersionList' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdCreateDate :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.ISO8601)
mpdCreateDate = Lens.lens (createDate :: ManagedPolicyDetail -> Lude.Maybe Lude.ISO8601) (\s a -> s {createDate = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | Specifies whether the policy can be attached to an IAM user, group, or role.
--
-- /Note:/ Consider using 'isAttachable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdIsAttachable :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Bool)
mpdIsAttachable = Lens.lens (isAttachable :: ManagedPolicyDetail -> Lude.Maybe Lude.Bool) (\s a -> s {isAttachable = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdIsAttachable "Use generic-lens or generic-optics with 'isAttachable' instead." #-}

-- | The number of entities (users and roles) for which the policy is used as the permissions boundary.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundaryUsageCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPermissionsBoundaryUsageCount :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Int)
mpdPermissionsBoundaryUsageCount = Lens.lens (permissionsBoundaryUsageCount :: ManagedPolicyDetail -> Lude.Maybe Lude.Int) (\s a -> s {permissionsBoundaryUsageCount = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdPermissionsBoundaryUsageCount "Use generic-lens or generic-optics with 'permissionsBoundaryUsageCount' instead." #-}

-- | The identifier for the version of the policy that is set as the default (operative) version.
--
-- For more information about policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdDefaultVersionId :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Text)
mpdDefaultVersionId = Lens.lens (defaultVersionId :: ManagedPolicyDetail -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersionId = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The number of principal entities (users, groups, and roles) that the policy is attached to.
--
-- /Note:/ Consider using 'attachmentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdAttachmentCount :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Int)
mpdAttachmentCount = Lens.lens (attachmentCount :: ManagedPolicyDetail -> Lude.Maybe Lude.Int) (\s a -> s {attachmentCount = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdAttachmentCount "Use generic-lens or generic-optics with 'attachmentCount' instead." #-}

-- | A friendly description of the policy.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdDescription :: Lens.Lens' ManagedPolicyDetail (Lude.Maybe Lude.Text)
mpdDescription = Lens.lens (description :: ManagedPolicyDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ManagedPolicyDetail)
{-# DEPRECATED mpdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ManagedPolicyDetail where
  parseXML x =
    ManagedPolicyDetail'
      Lude.<$> (x Lude..@? "PolicyName")
      Lude.<*> (x Lude..@? "Arn")
      Lude.<*> (x Lude..@? "UpdateDate")
      Lude.<*> (x Lude..@? "PolicyId")
      Lude.<*> (x Lude..@? "Path")
      Lude.<*> ( x Lude..@? "PolicyVersionList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "CreateDate")
      Lude.<*> (x Lude..@? "IsAttachable")
      Lude.<*> (x Lude..@? "PermissionsBoundaryUsageCount")
      Lude.<*> (x Lude..@? "DefaultVersionId")
      Lude.<*> (x Lude..@? "AttachmentCount")
      Lude.<*> (x Lude..@? "Description")
