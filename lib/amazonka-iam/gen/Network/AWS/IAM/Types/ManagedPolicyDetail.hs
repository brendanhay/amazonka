{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    mpdArn,
    mpdAttachmentCount,
    mpdCreateDate,
    mpdDefaultVersionId,
    mpdDescription,
    mpdIsAttachable,
    mpdPath,
    mpdPermissionsBoundaryUsageCount,
    mpdPolicyId,
    mpdPolicyName,
    mpdPolicyVersionList,
    mpdUpdateDate,
  )
where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.IAM.Types.DefaultVersionId as Types
import qualified Network.AWS.IAM.Types.Description as Types
import qualified Network.AWS.IAM.Types.Path as Types
import qualified Network.AWS.IAM.Types.PolicyId as Types
import qualified Network.AWS.IAM.Types.PolicyName as Types
import qualified Network.AWS.IAM.Types.PolicyVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a managed policy, including the policy's ARN, versions, and the number of principal entities (users, groups, and roles) that the policy is attached to.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
-- For more information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkManagedPolicyDetail' smart constructor.
data ManagedPolicyDetail = ManagedPolicyDetail'
  { arn :: Core.Maybe Types.Arn,
    -- | The number of principal entities (users, groups, and roles) that the policy is attached to.
    attachmentCount :: Core.Maybe Core.Int,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | The identifier for the version of the policy that is set as the default (operative) version.
    --
    -- For more information about policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
    defaultVersionId :: Core.Maybe Types.DefaultVersionId,
    -- | A friendly description of the policy.
    description :: Core.Maybe Types.Description,
    -- | Specifies whether the policy can be attached to an IAM user, group, or role.
    isAttachable :: Core.Maybe Core.Bool,
    -- | The path to the policy.
    --
    -- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Core.Maybe Types.Path,
    -- | The number of entities (users and roles) for which the policy is used as the permissions boundary.
    --
    -- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
    permissionsBoundaryUsageCount :: Core.Maybe Core.Int,
    -- | The stable and unique string identifying the policy.
    --
    -- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    policyId :: Core.Maybe Types.PolicyId,
    -- | The friendly name (not ARN) identifying the policy.
    policyName :: Core.Maybe Types.PolicyName,
    -- | A list containing information about the versions of the policy.
    policyVersionList :: Core.Maybe [Types.PolicyVersion],
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
    --
    -- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
    updateDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ManagedPolicyDetail' value with any optional fields omitted.
mkManagedPolicyDetail ::
  ManagedPolicyDetail
mkManagedPolicyDetail =
  ManagedPolicyDetail'
    { arn = Core.Nothing,
      attachmentCount = Core.Nothing,
      createDate = Core.Nothing,
      defaultVersionId = Core.Nothing,
      description = Core.Nothing,
      isAttachable = Core.Nothing,
      path = Core.Nothing,
      permissionsBoundaryUsageCount = Core.Nothing,
      policyId = Core.Nothing,
      policyName = Core.Nothing,
      policyVersionList = Core.Nothing,
      updateDate = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdArn :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Types.Arn)
mpdArn = Lens.field @"arn"
{-# DEPRECATED mpdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of principal entities (users, groups, and roles) that the policy is attached to.
--
-- /Note:/ Consider using 'attachmentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdAttachmentCount :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Int)
mpdAttachmentCount = Lens.field @"attachmentCount"
{-# DEPRECATED mpdAttachmentCount "Use generic-lens or generic-optics with 'attachmentCount' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdCreateDate :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.UTCTime)
mpdCreateDate = Lens.field @"createDate"
{-# DEPRECATED mpdCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The identifier for the version of the policy that is set as the default (operative) version.
--
-- For more information about policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdDefaultVersionId :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Types.DefaultVersionId)
mpdDefaultVersionId = Lens.field @"defaultVersionId"
{-# DEPRECATED mpdDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | A friendly description of the policy.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdDescription :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Types.Description)
mpdDescription = Lens.field @"description"
{-# DEPRECATED mpdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the policy can be attached to an IAM user, group, or role.
--
-- /Note:/ Consider using 'isAttachable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdIsAttachable :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Bool)
mpdIsAttachable = Lens.field @"isAttachable"
{-# DEPRECATED mpdIsAttachable "Use generic-lens or generic-optics with 'isAttachable' instead." #-}

-- | The path to the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPath :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Types.Path)
mpdPath = Lens.field @"path"
{-# DEPRECATED mpdPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The number of entities (users and roles) for which the policy is used as the permissions boundary.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundaryUsageCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPermissionsBoundaryUsageCount :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Int)
mpdPermissionsBoundaryUsageCount = Lens.field @"permissionsBoundaryUsageCount"
{-# DEPRECATED mpdPermissionsBoundaryUsageCount "Use generic-lens or generic-optics with 'permissionsBoundaryUsageCount' instead." #-}

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPolicyId :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Types.PolicyId)
mpdPolicyId = Lens.field @"policyId"
{-# DEPRECATED mpdPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The friendly name (not ARN) identifying the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPolicyName :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Types.PolicyName)
mpdPolicyName = Lens.field @"policyName"
{-# DEPRECATED mpdPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | A list containing information about the versions of the policy.
--
-- /Note:/ Consider using 'policyVersionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdPolicyVersionList :: Lens.Lens' ManagedPolicyDetail (Core.Maybe [Types.PolicyVersion])
mpdPolicyVersionList = Lens.field @"policyVersionList"
{-# DEPRECATED mpdPolicyVersionList "Use generic-lens or generic-optics with 'policyVersionList' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
--
-- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdUpdateDate :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.UTCTime)
mpdUpdateDate = Lens.field @"updateDate"
{-# DEPRECATED mpdUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

instance Core.FromXML ManagedPolicyDetail where
  parseXML x =
    ManagedPolicyDetail'
      Core.<$> (x Core..@? "Arn")
      Core.<*> (x Core..@? "AttachmentCount")
      Core.<*> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "DefaultVersionId")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IsAttachable")
      Core.<*> (x Core..@? "Path")
      Core.<*> (x Core..@? "PermissionsBoundaryUsageCount")
      Core.<*> (x Core..@? "PolicyId")
      Core.<*> (x Core..@? "PolicyName")
      Core.<*> ( x Core..@? "PolicyVersionList"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "UpdateDate")
