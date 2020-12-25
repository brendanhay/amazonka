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
    pArn,
    pAttachmentCount,
    pCreateDate,
    pDefaultVersionId,
    pDescription,
    pIsAttachable,
    pPath,
    pPermissionsBoundaryUsageCount,
    pPolicyId,
    pPolicyName,
    pUpdateDate,
  )
where

import qualified Network.AWS.IAM.Types.ArnType as Types
import qualified Network.AWS.IAM.Types.Description as Types
import qualified Network.AWS.IAM.Types.IdType as Types
import qualified Network.AWS.IAM.Types.PolicyNameType as Types
import qualified Network.AWS.IAM.Types.PolicyPathType as Types
import qualified Network.AWS.IAM.Types.PolicyVersionIdType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a managed policy.
--
-- This data type is used as a response element in the 'CreatePolicy' , 'GetPolicy' , and 'ListPolicies' operations.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkPolicy' smart constructor.
data Policy = Policy'
  { arn :: Core.Maybe Types.ArnType,
    -- | The number of entities (users, groups, and roles) that the policy is attached to.
    attachmentCount :: Core.Maybe Core.Int,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | The identifier for the version of the policy that is set as the default version.
    defaultVersionId :: Core.Maybe Types.PolicyVersionIdType,
    -- | A friendly description of the policy.
    --
    -- This element is included in the response to the 'GetPolicy' operation. It is not included in the response to the 'ListPolicies' operation.
    description :: Core.Maybe Types.Description,
    -- | Specifies whether the policy can be attached to an IAM user, group, or role.
    isAttachable :: Core.Maybe Core.Bool,
    -- | The path to the policy.
    --
    -- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Core.Maybe Types.PolicyPathType,
    -- | The number of entities (users and roles) for which the policy is used to set the permissions boundary.
    --
    -- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
    permissionsBoundaryUsageCount :: Core.Maybe Core.Int,
    -- | The stable and unique string identifying the policy.
    --
    -- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    policyId :: Core.Maybe Types.IdType,
    -- | The friendly name (not ARN) identifying the policy.
    policyName :: Core.Maybe Types.PolicyNameType,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
    --
    -- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
    updateDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Policy' value with any optional fields omitted.
mkPolicy ::
  Policy
mkPolicy =
  Policy'
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
      updateDate = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArn :: Lens.Lens' Policy (Core.Maybe Types.ArnType)
pArn = Lens.field @"arn"
{-# DEPRECATED pArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of entities (users, groups, and roles) that the policy is attached to.
--
-- /Note:/ Consider using 'attachmentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAttachmentCount :: Lens.Lens' Policy (Core.Maybe Core.Int)
pAttachmentCount = Lens.field @"attachmentCount"
{-# DEPRECATED pAttachmentCount "Use generic-lens or generic-optics with 'attachmentCount' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreateDate :: Lens.Lens' Policy (Core.Maybe Core.UTCTime)
pCreateDate = Lens.field @"createDate"
{-# DEPRECATED pCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The identifier for the version of the policy that is set as the default version.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDefaultVersionId :: Lens.Lens' Policy (Core.Maybe Types.PolicyVersionIdType)
pDefaultVersionId = Lens.field @"defaultVersionId"
{-# DEPRECATED pDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | A friendly description of the policy.
--
-- This element is included in the response to the 'GetPolicy' operation. It is not included in the response to the 'ListPolicies' operation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Policy (Core.Maybe Types.Description)
pDescription = Lens.field @"description"
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the policy can be attached to an IAM user, group, or role.
--
-- /Note:/ Consider using 'isAttachable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsAttachable :: Lens.Lens' Policy (Core.Maybe Core.Bool)
pIsAttachable = Lens.field @"isAttachable"
{-# DEPRECATED pIsAttachable "Use generic-lens or generic-optics with 'isAttachable' instead." #-}

-- | The path to the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPath :: Lens.Lens' Policy (Core.Maybe Types.PolicyPathType)
pPath = Lens.field @"path"
{-# DEPRECATED pPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The number of entities (users and roles) for which the policy is used to set the permissions boundary.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundaryUsageCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPermissionsBoundaryUsageCount :: Lens.Lens' Policy (Core.Maybe Core.Int)
pPermissionsBoundaryUsageCount = Lens.field @"permissionsBoundaryUsageCount"
{-# DEPRECATED pPermissionsBoundaryUsageCount "Use generic-lens or generic-optics with 'permissionsBoundaryUsageCount' instead." #-}

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyId :: Lens.Lens' Policy (Core.Maybe Types.IdType)
pPolicyId = Lens.field @"policyId"
{-# DEPRECATED pPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The friendly name (not ARN) identifying the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyName :: Lens.Lens' Policy (Core.Maybe Types.PolicyNameType)
pPolicyName = Lens.field @"policyName"
{-# DEPRECATED pPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated.
--
-- When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pUpdateDate :: Lens.Lens' Policy (Core.Maybe Core.UTCTime)
pUpdateDate = Lens.field @"updateDate"
{-# DEPRECATED pUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

instance Core.FromXML Policy where
  parseXML x =
    Policy'
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
      Core.<*> (x Core..@? "UpdateDate")
