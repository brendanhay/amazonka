{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.RoleDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.RoleDetail
  ( RoleDetail (..)
  -- * Smart constructor
  , mkRoleDetail
  -- * Lenses
  , rdArn
  , rdAssumeRolePolicyDocument
  , rdAttachedManagedPolicies
  , rdCreateDate
  , rdInstanceProfileList
  , rdPath
  , rdPermissionsBoundary
  , rdRoleId
  , rdRoleLastUsed
  , rdRoleName
  , rdRolePolicyList
  , rdTags
  ) where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.IAM.Types.AssumeRolePolicyDocument as Types
import qualified Network.AWS.IAM.Types.AttachedPermissionsBoundary as Types
import qualified Network.AWS.IAM.Types.AttachedPolicy as Types
import qualified Network.AWS.IAM.Types.InstanceProfile as Types
import qualified Network.AWS.IAM.Types.Path as Types
import qualified Network.AWS.IAM.Types.PolicyDetail as Types
import qualified Network.AWS.IAM.Types.RoleId as Types
import qualified Network.AWS.IAM.Types.RoleLastUsed as Types
import qualified Network.AWS.IAM.Types.RoleName as Types
import qualified Network.AWS.IAM.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an IAM role, including all of the role's policies.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- /See:/ 'mkRoleDetail' smart constructor.
data RoleDetail = RoleDetail'
  { arn :: Core.Maybe Types.Arn
  , assumeRolePolicyDocument :: Core.Maybe Types.AssumeRolePolicyDocument
    -- ^ The trust policy that grants permission to assume the role.
  , attachedManagedPolicies :: Core.Maybe [Types.AttachedPolicy]
    -- ^ A list of managed policies attached to the role. These policies are the role's access (permissions) policies.
  , createDate :: Core.Maybe Core.UTCTime
    -- ^ The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
  , instanceProfileList :: Core.Maybe [Types.InstanceProfile]
    -- ^ A list of instance profiles that contain this role.
  , path :: Core.Maybe Types.Path
    -- ^ The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
  , permissionsBoundary :: Core.Maybe Types.AttachedPermissionsBoundary
    -- ^ The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
  , roleId :: Core.Maybe Types.RoleId
    -- ^ The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
  , roleLastUsed :: Core.Maybe Types.RoleLastUsed
    -- ^ Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
  , roleName :: Core.Maybe Types.RoleName
    -- ^ The friendly name that identifies the role.
  , rolePolicyList :: Core.Maybe [Types.PolicyDetail]
    -- ^ A list of inline policies embedded in the role. These policies are the role's access (permissions) policies.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RoleDetail' value with any optional fields omitted.
mkRoleDetail
    :: RoleDetail
mkRoleDetail
  = RoleDetail'{arn = Core.Nothing,
                assumeRolePolicyDocument = Core.Nothing,
                attachedManagedPolicies = Core.Nothing, createDate = Core.Nothing,
                instanceProfileList = Core.Nothing, path = Core.Nothing,
                permissionsBoundary = Core.Nothing, roleId = Core.Nothing,
                roleLastUsed = Core.Nothing, roleName = Core.Nothing,
                rolePolicyList = Core.Nothing, tags = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdArn :: Lens.Lens' RoleDetail (Core.Maybe Types.Arn)
rdArn = Lens.field @"arn"
{-# INLINEABLE rdArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The trust policy that grants permission to assume the role.
--
-- /Note:/ Consider using 'assumeRolePolicyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAssumeRolePolicyDocument :: Lens.Lens' RoleDetail (Core.Maybe Types.AssumeRolePolicyDocument)
rdAssumeRolePolicyDocument = Lens.field @"assumeRolePolicyDocument"
{-# INLINEABLE rdAssumeRolePolicyDocument #-}
{-# DEPRECATED assumeRolePolicyDocument "Use generic-lens or generic-optics with 'assumeRolePolicyDocument' instead"  #-}

-- | A list of managed policies attached to the role. These policies are the role's access (permissions) policies.
--
-- /Note:/ Consider using 'attachedManagedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAttachedManagedPolicies :: Lens.Lens' RoleDetail (Core.Maybe [Types.AttachedPolicy])
rdAttachedManagedPolicies = Lens.field @"attachedManagedPolicies"
{-# INLINEABLE rdAttachedManagedPolicies #-}
{-# DEPRECATED attachedManagedPolicies "Use generic-lens or generic-optics with 'attachedManagedPolicies' instead"  #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCreateDate :: Lens.Lens' RoleDetail (Core.Maybe Core.UTCTime)
rdCreateDate = Lens.field @"createDate"
{-# INLINEABLE rdCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | A list of instance profiles that contain this role.
--
-- /Note:/ Consider using 'instanceProfileList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdInstanceProfileList :: Lens.Lens' RoleDetail (Core.Maybe [Types.InstanceProfile])
rdInstanceProfileList = Lens.field @"instanceProfileList"
{-# INLINEABLE rdInstanceProfileList #-}
{-# DEPRECATED instanceProfileList "Use generic-lens or generic-optics with 'instanceProfileList' instead"  #-}

-- | The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPath :: Lens.Lens' RoleDetail (Core.Maybe Types.Path)
rdPath = Lens.field @"path"
{-# INLINEABLE rdPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPermissionsBoundary :: Lens.Lens' RoleDetail (Core.Maybe Types.AttachedPermissionsBoundary)
rdPermissionsBoundary = Lens.field @"permissionsBoundary"
{-# INLINEABLE rdPermissionsBoundary #-}
{-# DEPRECATED permissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead"  #-}

-- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoleId :: Lens.Lens' RoleDetail (Core.Maybe Types.RoleId)
rdRoleId = Lens.field @"roleId"
{-# INLINEABLE rdRoleId #-}
{-# DEPRECATED roleId "Use generic-lens or generic-optics with 'roleId' instead"  #-}

-- | Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleLastUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoleLastUsed :: Lens.Lens' RoleDetail (Core.Maybe Types.RoleLastUsed)
rdRoleLastUsed = Lens.field @"roleLastUsed"
{-# INLINEABLE rdRoleLastUsed #-}
{-# DEPRECATED roleLastUsed "Use generic-lens or generic-optics with 'roleLastUsed' instead"  #-}

-- | The friendly name that identifies the role.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoleName :: Lens.Lens' RoleDetail (Core.Maybe Types.RoleName)
rdRoleName = Lens.field @"roleName"
{-# INLINEABLE rdRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | A list of inline policies embedded in the role. These policies are the role's access (permissions) policies.
--
-- /Note:/ Consider using 'rolePolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRolePolicyList :: Lens.Lens' RoleDetail (Core.Maybe [Types.PolicyDetail])
rdRolePolicyList = Lens.field @"rolePolicyList"
{-# INLINEABLE rdRolePolicyList #-}
{-# DEPRECATED rolePolicyList "Use generic-lens or generic-optics with 'rolePolicyList' instead"  #-}

-- | A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdTags :: Lens.Lens' RoleDetail (Core.Maybe [Types.Tag])
rdTags = Lens.field @"tags"
{-# INLINEABLE rdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML RoleDetail where
        parseXML x
          = RoleDetail' Core.<$>
              (x Core..@? "Arn") Core.<*> x Core..@? "AssumeRolePolicyDocument"
                Core.<*>
                x Core..@? "AttachedManagedPolicies" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "CreateDate"
                Core.<*>
                x Core..@? "InstanceProfileList" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "Path"
                Core.<*> x Core..@? "PermissionsBoundary"
                Core.<*> x Core..@? "RoleId"
                Core.<*> x Core..@? "RoleLastUsed"
                Core.<*> x Core..@? "RoleName"
                Core.<*>
                x Core..@? "RolePolicyList" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "member"
