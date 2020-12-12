{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.RoleDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.RoleDetail
  ( RoleDetail (..),

    -- * Smart constructor
    mkRoleDetail,

    -- * Lenses
    rdAssumeRolePolicyDocument,
    rdARN,
    rdPath,
    rdInstanceProfileList,
    rdCreateDate,
    rdRoleName,
    rdRoleId,
    rdRoleLastUsed,
    rdPermissionsBoundary,
    rdRolePolicyList,
    rdTags,
    rdAttachedManagedPolicies,
  )
where

import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.InstanceProfile
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an IAM role, including all of the role's policies.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- /See:/ 'mkRoleDetail' smart constructor.
data RoleDetail = RoleDetail'
  { assumeRolePolicyDocument ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    path :: Lude.Maybe Lude.Text,
    instanceProfileList :: Lude.Maybe [InstanceProfile],
    createDate :: Lude.Maybe Lude.DateTime,
    roleName :: Lude.Maybe Lude.Text,
    roleId :: Lude.Maybe Lude.Text,
    roleLastUsed :: Lude.Maybe RoleLastUsed,
    permissionsBoundary :: Lude.Maybe AttachedPermissionsBoundary,
    rolePolicyList :: Lude.Maybe [PolicyDetail],
    tags :: Lude.Maybe [Tag],
    attachedManagedPolicies :: Lude.Maybe [AttachedPolicy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoleDetail' with the minimum fields required to make a request.
--
-- * 'arn' - Undocumented field.
-- * 'assumeRolePolicyDocument' - The trust policy that grants permission to assume the role.
-- * 'attachedManagedPolicies' - A list of managed policies attached to the role. These policies are the role's access (permissions) policies.
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
-- * 'instanceProfileList' - A list of instance profiles that contain this role.
-- * 'path' - The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
-- * 'roleId' - The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'roleLastUsed' - Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
-- * 'roleName' - The friendly name that identifies the role.
-- * 'rolePolicyList' - A list of inline policies embedded in the role. These policies are the role's access (permissions) policies.
-- * 'tags' - A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
mkRoleDetail ::
  RoleDetail
mkRoleDetail =
  RoleDetail'
    { assumeRolePolicyDocument = Lude.Nothing,
      arn = Lude.Nothing,
      path = Lude.Nothing,
      instanceProfileList = Lude.Nothing,
      createDate = Lude.Nothing,
      roleName = Lude.Nothing,
      roleId = Lude.Nothing,
      roleLastUsed = Lude.Nothing,
      permissionsBoundary = Lude.Nothing,
      rolePolicyList = Lude.Nothing,
      tags = Lude.Nothing,
      attachedManagedPolicies = Lude.Nothing
    }

-- | The trust policy that grants permission to assume the role.
--
-- /Note:/ Consider using 'assumeRolePolicyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAssumeRolePolicyDocument :: Lens.Lens' RoleDetail (Lude.Maybe Lude.Text)
rdAssumeRolePolicyDocument = Lens.lens (assumeRolePolicyDocument :: RoleDetail -> Lude.Maybe Lude.Text) (\s a -> s {assumeRolePolicyDocument = a} :: RoleDetail)
{-# DEPRECATED rdAssumeRolePolicyDocument "Use generic-lens or generic-optics with 'assumeRolePolicyDocument' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdARN :: Lens.Lens' RoleDetail (Lude.Maybe Lude.Text)
rdARN = Lens.lens (arn :: RoleDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RoleDetail)
{-# DEPRECATED rdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPath :: Lens.Lens' RoleDetail (Lude.Maybe Lude.Text)
rdPath = Lens.lens (path :: RoleDetail -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: RoleDetail)
{-# DEPRECATED rdPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | A list of instance profiles that contain this role.
--
-- /Note:/ Consider using 'instanceProfileList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdInstanceProfileList :: Lens.Lens' RoleDetail (Lude.Maybe [InstanceProfile])
rdInstanceProfileList = Lens.lens (instanceProfileList :: RoleDetail -> Lude.Maybe [InstanceProfile]) (\s a -> s {instanceProfileList = a} :: RoleDetail)
{-# DEPRECATED rdInstanceProfileList "Use generic-lens or generic-optics with 'instanceProfileList' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCreateDate :: Lens.Lens' RoleDetail (Lude.Maybe Lude.DateTime)
rdCreateDate = Lens.lens (createDate :: RoleDetail -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: RoleDetail)
{-# DEPRECATED rdCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The friendly name that identifies the role.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoleName :: Lens.Lens' RoleDetail (Lude.Maybe Lude.Text)
rdRoleName = Lens.lens (roleName :: RoleDetail -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: RoleDetail)
{-# DEPRECATED rdRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoleId :: Lens.Lens' RoleDetail (Lude.Maybe Lude.Text)
rdRoleId = Lens.lens (roleId :: RoleDetail -> Lude.Maybe Lude.Text) (\s a -> s {roleId = a} :: RoleDetail)
{-# DEPRECATED rdRoleId "Use generic-lens or generic-optics with 'roleId' instead." #-}

-- | Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'roleLastUsed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoleLastUsed :: Lens.Lens' RoleDetail (Lude.Maybe RoleLastUsed)
rdRoleLastUsed = Lens.lens (roleLastUsed :: RoleDetail -> Lude.Maybe RoleLastUsed) (\s a -> s {roleLastUsed = a} :: RoleDetail)
{-# DEPRECATED rdRoleLastUsed "Use generic-lens or generic-optics with 'roleLastUsed' instead." #-}

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPermissionsBoundary :: Lens.Lens' RoleDetail (Lude.Maybe AttachedPermissionsBoundary)
rdPermissionsBoundary = Lens.lens (permissionsBoundary :: RoleDetail -> Lude.Maybe AttachedPermissionsBoundary) (\s a -> s {permissionsBoundary = a} :: RoleDetail)
{-# DEPRECATED rdPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | A list of inline policies embedded in the role. These policies are the role's access (permissions) policies.
--
-- /Note:/ Consider using 'rolePolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRolePolicyList :: Lens.Lens' RoleDetail (Lude.Maybe [PolicyDetail])
rdRolePolicyList = Lens.lens (rolePolicyList :: RoleDetail -> Lude.Maybe [PolicyDetail]) (\s a -> s {rolePolicyList = a} :: RoleDetail)
{-# DEPRECATED rdRolePolicyList "Use generic-lens or generic-optics with 'rolePolicyList' instead." #-}

-- | A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdTags :: Lens.Lens' RoleDetail (Lude.Maybe [Tag])
rdTags = Lens.lens (tags :: RoleDetail -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RoleDetail)
{-# DEPRECATED rdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of managed policies attached to the role. These policies are the role's access (permissions) policies.
--
-- /Note:/ Consider using 'attachedManagedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAttachedManagedPolicies :: Lens.Lens' RoleDetail (Lude.Maybe [AttachedPolicy])
rdAttachedManagedPolicies = Lens.lens (attachedManagedPolicies :: RoleDetail -> Lude.Maybe [AttachedPolicy]) (\s a -> s {attachedManagedPolicies = a} :: RoleDetail)
{-# DEPRECATED rdAttachedManagedPolicies "Use generic-lens or generic-optics with 'attachedManagedPolicies' instead." #-}

instance Lude.FromXML RoleDetail where
  parseXML x =
    RoleDetail'
      Lude.<$> (x Lude..@? "AssumeRolePolicyDocument")
      Lude.<*> (x Lude..@? "Arn")
      Lude.<*> (x Lude..@? "Path")
      Lude.<*> ( x Lude..@? "InstanceProfileList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "CreateDate")
      Lude.<*> (x Lude..@? "RoleName")
      Lude.<*> (x Lude..@? "RoleId")
      Lude.<*> (x Lude..@? "RoleLastUsed")
      Lude.<*> (x Lude..@? "PermissionsBoundary")
      Lude.<*> ( x Lude..@? "RolePolicyList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "AttachedManagedPolicies" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
