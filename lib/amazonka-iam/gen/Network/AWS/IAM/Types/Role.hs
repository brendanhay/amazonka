{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Role
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Role
  ( Role (..),

    -- * Smart constructor
    mkRole,

    -- * Lenses
    rPath,
    rRoleName,
    rRoleId,
    rArn,
    rCreateDate,
    rAssumeRolePolicyDocument,
    rDescription,
    rMaxSessionDuration,
    rPermissionsBoundary,
    rRoleLastUsed,
    rTags,
  )
where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.IAM.Types.AssumeRolePolicyDocument as Types
import qualified Network.AWS.IAM.Types.AttachedPermissionsBoundary as Types
import qualified Network.AWS.IAM.Types.IdType as Types
import qualified Network.AWS.IAM.Types.Path as Types
import qualified Network.AWS.IAM.Types.RoleDescriptionType as Types
import qualified Network.AWS.IAM.Types.RoleLastUsed as Types
import qualified Network.AWS.IAM.Types.RoleName as Types
import qualified Network.AWS.IAM.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an IAM role. This structure is returned as a response element in several API operations that interact with roles.
--
-- /See:/ 'mkRole' smart constructor.
data Role = Role'
  { -- | The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Types.Path,
    -- | The friendly name that identifies the role.
    roleName :: Types.RoleName,
    -- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    roleId :: Types.IdType,
    -- | The Amazon Resource Name (ARN) specifying the role. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ guide.
    arn :: Types.Arn,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
    createDate :: Core.UTCTime,
    -- | The policy that grants an entity permission to assume the role.
    assumeRolePolicyDocument :: Core.Maybe Types.AssumeRolePolicyDocument,
    -- | A description of the role that you provide.
    description :: Core.Maybe Types.RoleDescriptionType,
    -- | The maximum session duration (in seconds) for the specified role. Anyone who uses the AWS CLI, or API to assume the role can specify the duration using the optional @DurationSeconds@ API parameter or @duration-seconds@ CLI parameter.
    maxSessionDuration :: Core.Maybe Core.Natural,
    -- | The ARN of the policy used to set the permissions boundary for the role.
    --
    -- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
    permissionsBoundary :: Core.Maybe Types.AttachedPermissionsBoundary,
    -- | Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
    roleLastUsed :: Core.Maybe Types.RoleLastUsed,
    -- | A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Role' value with any optional fields omitted.
mkRole ::
  -- | 'path'
  Types.Path ->
  -- | 'roleName'
  Types.RoleName ->
  -- | 'roleId'
  Types.IdType ->
  -- | 'arn'
  Types.Arn ->
  -- | 'createDate'
  Core.UTCTime ->
  Role
mkRole path roleName roleId arn createDate =
  Role'
    { path,
      roleName,
      roleId,
      arn,
      createDate,
      assumeRolePolicyDocument = Core.Nothing,
      description = Core.Nothing,
      maxSessionDuration = Core.Nothing,
      permissionsBoundary = Core.Nothing,
      roleLastUsed = Core.Nothing,
      tags = Core.Nothing
    }

-- | The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPath :: Lens.Lens' Role Types.Path
rPath = Lens.field @"path"
{-# DEPRECATED rPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The friendly name that identifies the role.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoleName :: Lens.Lens' Role Types.RoleName
rRoleName = Lens.field @"roleName"
{-# DEPRECATED rRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoleId :: Lens.Lens' Role Types.IdType
rRoleId = Lens.field @"roleId"
{-# DEPRECATED rRoleId "Use generic-lens or generic-optics with 'roleId' instead." #-}

-- | The Amazon Resource Name (ARN) specifying the role. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ guide.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Role Types.Arn
rArn = Lens.field @"arn"
{-# DEPRECATED rArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreateDate :: Lens.Lens' Role Core.UTCTime
rCreateDate = Lens.field @"createDate"
{-# DEPRECATED rCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The policy that grants an entity permission to assume the role.
--
-- /Note:/ Consider using 'assumeRolePolicyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAssumeRolePolicyDocument :: Lens.Lens' Role (Core.Maybe Types.AssumeRolePolicyDocument)
rAssumeRolePolicyDocument = Lens.field @"assumeRolePolicyDocument"
{-# DEPRECATED rAssumeRolePolicyDocument "Use generic-lens or generic-optics with 'assumeRolePolicyDocument' instead." #-}

-- | A description of the role that you provide.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' Role (Core.Maybe Types.RoleDescriptionType)
rDescription = Lens.field @"description"
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The maximum session duration (in seconds) for the specified role. Anyone who uses the AWS CLI, or API to assume the role can specify the duration using the optional @DurationSeconds@ API parameter or @duration-seconds@ CLI parameter.
--
-- /Note:/ Consider using 'maxSessionDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMaxSessionDuration :: Lens.Lens' Role (Core.Maybe Core.Natural)
rMaxSessionDuration = Lens.field @"maxSessionDuration"
{-# DEPRECATED rMaxSessionDuration "Use generic-lens or generic-optics with 'maxSessionDuration' instead." #-}

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPermissionsBoundary :: Lens.Lens' Role (Core.Maybe Types.AttachedPermissionsBoundary)
rPermissionsBoundary = Lens.field @"permissionsBoundary"
{-# DEPRECATED rPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleLastUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoleLastUsed :: Lens.Lens' Role (Core.Maybe Types.RoleLastUsed)
rRoleLastUsed = Lens.field @"roleLastUsed"
{-# DEPRECATED rRoleLastUsed "Use generic-lens or generic-optics with 'roleLastUsed' instead." #-}

-- | A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTags :: Lens.Lens' Role (Core.Maybe [Types.Tag])
rTags = Lens.field @"tags"
{-# DEPRECATED rTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML Role where
  parseXML x =
    Role'
      Core.<$> (x Core..@ "Path")
      Core.<*> (x Core..@ "RoleName")
      Core.<*> (x Core..@ "RoleId")
      Core.<*> (x Core..@ "Arn")
      Core.<*> (x Core..@ "CreateDate")
      Core.<*> (x Core..@? "AssumeRolePolicyDocument")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "MaxSessionDuration")
      Core.<*> (x Core..@? "PermissionsBoundary")
      Core.<*> (x Core..@? "RoleLastUsed")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "member")
