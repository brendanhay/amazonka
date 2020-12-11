{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new role for your AWS account. For more information about roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html IAM Roles> . The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateRole
  ( -- * Creating a request
    CreateRole (..),
    mkCreateRole,

    -- ** Request lenses
    crMaxSessionDuration,
    crPath,
    crPermissionsBoundary,
    crDescription,
    crTags,
    crRoleName,
    crAssumeRolePolicyDocument,

    -- * Destructuring the response
    CreateRoleResponse (..),
    mkCreateRoleResponse,

    -- ** Response lenses
    crrsResponseStatus,
    crrsRole,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRole' smart constructor.
data CreateRole = CreateRole'
  { maxSessionDuration ::
      Lude.Maybe Lude.Natural,
    path :: Lude.Maybe Lude.Text,
    permissionsBoundary :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    roleName :: Lude.Text,
    assumeRolePolicyDocument :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRole' with the minimum fields required to make a request.
--
-- * 'assumeRolePolicyDocument' - The trust relationship policy document that grants an entity permission to assume the role.
--
-- In IAM, you must provide a JSON policy that has been converted to a string. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- Upon success, the response includes the same trust policy in JSON format.
-- * 'description' - A description of the role.
-- * 'maxSessionDuration' - The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
-- * 'path' - The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'permissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for the role.
-- * 'roleName' - The name of the role to create.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
-- * 'tags' - A list of tags that you want to attach to the newly created role. Each tag consists of a key name and an associated value. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
mkCreateRole ::
  -- | 'roleName'
  Lude.Text ->
  -- | 'assumeRolePolicyDocument'
  Lude.Text ->
  CreateRole
mkCreateRole pRoleName_ pAssumeRolePolicyDocument_ =
  CreateRole'
    { maxSessionDuration = Lude.Nothing,
      path = Lude.Nothing,
      permissionsBoundary = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      roleName = pRoleName_,
      assumeRolePolicyDocument = pAssumeRolePolicyDocument_
    }

-- | The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'maxSessionDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMaxSessionDuration :: Lens.Lens' CreateRole (Lude.Maybe Lude.Natural)
crMaxSessionDuration = Lens.lens (maxSessionDuration :: CreateRole -> Lude.Maybe Lude.Natural) (\s a -> s {maxSessionDuration = a} :: CreateRole)
{-# DEPRECATED crMaxSessionDuration "Use generic-lens or generic-optics with 'maxSessionDuration' instead." #-}

-- | The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPath :: Lens.Lens' CreateRole (Lude.Maybe Lude.Text)
crPath = Lens.lens (path :: CreateRole -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: CreateRole)
{-# DEPRECATED crPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The ARN of the policy that is used to set the permissions boundary for the role.
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPermissionsBoundary :: Lens.Lens' CreateRole (Lude.Maybe Lude.Text)
crPermissionsBoundary = Lens.lens (permissionsBoundary :: CreateRole -> Lude.Maybe Lude.Text) (\s a -> s {permissionsBoundary = a} :: CreateRole)
{-# DEPRECATED crPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | A description of the role.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' CreateRole (Lude.Maybe Lude.Text)
crDescription = Lens.lens (description :: CreateRole -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateRole)
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags that you want to attach to the newly created role. Each tag consists of a key name and an associated value. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRole (Lude.Maybe [Tag])
crTags = Lens.lens (tags :: CreateRole -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateRole)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the role to create.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRoleName :: Lens.Lens' CreateRole Lude.Text
crRoleName = Lens.lens (roleName :: CreateRole -> Lude.Text) (\s a -> s {roleName = a} :: CreateRole)
{-# DEPRECATED crRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The trust relationship policy document that grants an entity permission to assume the role.
--
-- In IAM, you must provide a JSON policy that has been converted to a string. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- Upon success, the response includes the same trust policy in JSON format.
--
-- /Note:/ Consider using 'assumeRolePolicyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAssumeRolePolicyDocument :: Lens.Lens' CreateRole Lude.Text
crAssumeRolePolicyDocument = Lens.lens (assumeRolePolicyDocument :: CreateRole -> Lude.Text) (\s a -> s {assumeRolePolicyDocument = a} :: CreateRole)
{-# DEPRECATED crAssumeRolePolicyDocument "Use generic-lens or generic-optics with 'assumeRolePolicyDocument' instead." #-}

instance Lude.AWSRequest CreateRole where
  type Rs CreateRole = CreateRoleResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateRoleResult"
      ( \s h x ->
          CreateRoleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "Role")
      )

instance Lude.ToHeaders CreateRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateRole where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRole where
  toQuery CreateRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "MaxSessionDuration" Lude.=: maxSessionDuration,
        "Path" Lude.=: path,
        "PermissionsBoundary" Lude.=: permissionsBoundary,
        "Description" Lude.=: description,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "RoleName" Lude.=: roleName,
        "AssumeRolePolicyDocument" Lude.=: assumeRolePolicyDocument
      ]

-- | Contains the response to a successful 'CreateRole' request.
--
-- /See:/ 'mkCreateRoleResponse' smart constructor.
data CreateRoleResponse = CreateRoleResponse'
  { responseStatus ::
      Lude.Int,
    role' :: Role
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRoleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'role'' - A structure containing details about the new role.
mkCreateRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'role''
  Role ->
  CreateRoleResponse
mkCreateRoleResponse pResponseStatus_ pRole_ =
  CreateRoleResponse'
    { responseStatus = pResponseStatus_,
      role' = pRole_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateRoleResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRoleResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure containing details about the new role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRole :: Lens.Lens' CreateRoleResponse Role
crrsRole = Lens.lens (role' :: CreateRoleResponse -> Role) (\s a -> s {role' = a} :: CreateRoleResponse)
{-# DEPRECATED crrsRole "Use generic-lens or generic-optics with 'role'' instead." #-}
