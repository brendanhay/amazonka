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
    rMaxSessionDuration,
    rAssumeRolePolicyDocument,
    rRoleLastUsed,
    rPermissionsBoundary,
    rDescription,
    rTags,
    rPath,
    rRoleName,
    rRoleId,
    rARN,
    rCreateDate,
  )
where

import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an IAM role. This structure is returned as a response element in several API operations that interact with roles.
--
-- /See:/ 'mkRole' smart constructor.
data Role = Role'
  { maxSessionDuration :: Lude.Maybe Lude.Natural,
    assumeRolePolicyDocument :: Lude.Maybe Lude.Text,
    roleLastUsed :: Lude.Maybe RoleLastUsed,
    permissionsBoundary :: Lude.Maybe AttachedPermissionsBoundary,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    path :: Lude.Text,
    roleName :: Lude.Text,
    roleId :: Lude.Text,
    arn :: Lude.Text,
    createDate :: Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Role' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) specifying the role. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ guide.
-- * 'assumeRolePolicyDocument' - The policy that grants an entity permission to assume the role.
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
-- * 'description' - A description of the role that you provide.
-- * 'maxSessionDuration' - The maximum session duration (in seconds) for the specified role. Anyone who uses the AWS CLI, or API to assume the role can specify the duration using the optional @DurationSeconds@ API parameter or @duration-seconds@ CLI parameter.
-- * 'path' - The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
-- * 'roleId' - The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'roleLastUsed' - Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
-- * 'roleName' - The friendly name that identifies the role.
-- * 'tags' - A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
mkRole ::
  -- | 'path'
  Lude.Text ->
  -- | 'roleName'
  Lude.Text ->
  -- | 'roleId'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'createDate'
  Lude.DateTime ->
  Role
mkRole pPath_ pRoleName_ pRoleId_ pARN_ pCreateDate_ =
  Role'
    { maxSessionDuration = Lude.Nothing,
      assumeRolePolicyDocument = Lude.Nothing,
      roleLastUsed = Lude.Nothing,
      permissionsBoundary = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      path = pPath_,
      roleName = pRoleName_,
      roleId = pRoleId_,
      arn = pARN_,
      createDate = pCreateDate_
    }

-- | The maximum session duration (in seconds) for the specified role. Anyone who uses the AWS CLI, or API to assume the role can specify the duration using the optional @DurationSeconds@ API parameter or @duration-seconds@ CLI parameter.
--
-- /Note:/ Consider using 'maxSessionDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rMaxSessionDuration :: Lens.Lens' Role (Lude.Maybe Lude.Natural)
rMaxSessionDuration = Lens.lens (maxSessionDuration :: Role -> Lude.Maybe Lude.Natural) (\s a -> s {maxSessionDuration = a} :: Role)
{-# DEPRECATED rMaxSessionDuration "Use generic-lens or generic-optics with 'maxSessionDuration' instead." #-}

-- | The policy that grants an entity permission to assume the role.
--
-- /Note:/ Consider using 'assumeRolePolicyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAssumeRolePolicyDocument :: Lens.Lens' Role (Lude.Maybe Lude.Text)
rAssumeRolePolicyDocument = Lens.lens (assumeRolePolicyDocument :: Role -> Lude.Maybe Lude.Text) (\s a -> s {assumeRolePolicyDocument = a} :: Role)
{-# DEPRECATED rAssumeRolePolicyDocument "Use generic-lens or generic-optics with 'assumeRolePolicyDocument' instead." #-}

-- | Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleLastUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoleLastUsed :: Lens.Lens' Role (Lude.Maybe RoleLastUsed)
rRoleLastUsed = Lens.lens (roleLastUsed :: Role -> Lude.Maybe RoleLastUsed) (\s a -> s {roleLastUsed = a} :: Role)
{-# DEPRECATED rRoleLastUsed "Use generic-lens or generic-optics with 'roleLastUsed' instead." #-}

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPermissionsBoundary :: Lens.Lens' Role (Lude.Maybe AttachedPermissionsBoundary)
rPermissionsBoundary = Lens.lens (permissionsBoundary :: Role -> Lude.Maybe AttachedPermissionsBoundary) (\s a -> s {permissionsBoundary = a} :: Role)
{-# DEPRECATED rPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | A description of the role that you provide.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' Role (Lude.Maybe Lude.Text)
rDescription = Lens.lens (description :: Role -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Role)
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTags :: Lens.Lens' Role (Lude.Maybe [Tag])
rTags = Lens.lens (tags :: Role -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Role)
{-# DEPRECATED rTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPath :: Lens.Lens' Role Lude.Text
rPath = Lens.lens (path :: Role -> Lude.Text) (\s a -> s {path = a} :: Role)
{-# DEPRECATED rPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The friendly name that identifies the role.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoleName :: Lens.Lens' Role Lude.Text
rRoleName = Lens.lens (roleName :: Role -> Lude.Text) (\s a -> s {roleName = a} :: Role)
{-# DEPRECATED rRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoleId :: Lens.Lens' Role Lude.Text
rRoleId = Lens.lens (roleId :: Role -> Lude.Text) (\s a -> s {roleId = a} :: Role)
{-# DEPRECATED rRoleId "Use generic-lens or generic-optics with 'roleId' instead." #-}

-- | The Amazon Resource Name (ARN) specifying the role. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ guide.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rARN :: Lens.Lens' Role Lude.Text
rARN = Lens.lens (arn :: Role -> Lude.Text) (\s a -> s {arn = a} :: Role)
{-# DEPRECATED rARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreateDate :: Lens.Lens' Role Lude.DateTime
rCreateDate = Lens.lens (createDate :: Role -> Lude.DateTime) (\s a -> s {createDate = a} :: Role)
{-# DEPRECATED rCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

instance Lude.FromXML Role where
  parseXML x =
    Role'
      Lude.<$> (x Lude..@? "MaxSessionDuration")
      Lude.<*> (x Lude..@? "AssumeRolePolicyDocument")
      Lude.<*> (x Lude..@? "RoleLastUsed")
      Lude.<*> (x Lude..@? "PermissionsBoundary")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@ "Path")
      Lude.<*> (x Lude..@ "RoleName")
      Lude.<*> (x Lude..@ "RoleId")
      Lude.<*> (x Lude..@ "Arn")
      Lude.<*> (x Lude..@ "CreateDate")
