{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateRole (..)
    , mkCreateRole
    -- ** Request lenses
    , crRoleName
    , crAssumeRolePolicyDocument
    , crDescription
    , crMaxSessionDuration
    , crPath
    , crPermissionsBoundary
    , crTags

    -- * Destructuring the response
    , CreateRoleResponse (..)
    , mkCreateRoleResponse
    -- ** Response lenses
    , crrrsRole
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRole' smart constructor.
data CreateRole = CreateRole'
  { roleName :: Types.RoleNameType
    -- ^ The name of the role to create.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
  , assumeRolePolicyDocument :: Types.PolicyDocumentType
    -- ^ The trust relationship policy document that grants an entity permission to assume the role.
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
  , description :: Core.Maybe Types.RoleDescriptionType
    -- ^ A description of the role.
  , maxSessionDuration :: Core.Maybe Core.Natural
    -- ^ The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
  , path :: Core.Maybe Types.PathType
    -- ^ The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
  , permissionsBoundary :: Core.Maybe Types.ArnType
    -- ^ The ARN of the policy that is used to set the permissions boundary for the role.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags that you want to attach to the newly created role. Each tag consists of a key name and an associated value. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRole' value with any optional fields omitted.
mkCreateRole
    :: Types.RoleNameType -- ^ 'roleName'
    -> Types.PolicyDocumentType -- ^ 'assumeRolePolicyDocument'
    -> CreateRole
mkCreateRole roleName assumeRolePolicyDocument
  = CreateRole'{roleName, assumeRolePolicyDocument,
                description = Core.Nothing, maxSessionDuration = Core.Nothing,
                path = Core.Nothing, permissionsBoundary = Core.Nothing,
                tags = Core.Nothing}

-- | The name of the role to create.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRoleName :: Lens.Lens' CreateRole Types.RoleNameType
crRoleName = Lens.field @"roleName"
{-# INLINEABLE crRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

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
crAssumeRolePolicyDocument :: Lens.Lens' CreateRole Types.PolicyDocumentType
crAssumeRolePolicyDocument = Lens.field @"assumeRolePolicyDocument"
{-# INLINEABLE crAssumeRolePolicyDocument #-}
{-# DEPRECATED assumeRolePolicyDocument "Use generic-lens or generic-optics with 'assumeRolePolicyDocument' instead"  #-}

-- | A description of the role.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' CreateRole (Core.Maybe Types.RoleDescriptionType)
crDescription = Lens.field @"description"
{-# INLINEABLE crDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'maxSessionDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMaxSessionDuration :: Lens.Lens' CreateRole (Core.Maybe Core.Natural)
crMaxSessionDuration = Lens.field @"maxSessionDuration"
{-# INLINEABLE crMaxSessionDuration #-}
{-# DEPRECATED maxSessionDuration "Use generic-lens or generic-optics with 'maxSessionDuration' instead"  #-}

-- | The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPath :: Lens.Lens' CreateRole (Core.Maybe Types.PathType)
crPath = Lens.field @"path"
{-# INLINEABLE crPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The ARN of the policy that is used to set the permissions boundary for the role.
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPermissionsBoundary :: Lens.Lens' CreateRole (Core.Maybe Types.ArnType)
crPermissionsBoundary = Lens.field @"permissionsBoundary"
{-# INLINEABLE crPermissionsBoundary #-}
{-# DEPRECATED permissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead"  #-}

-- | A list of tags that you want to attach to the newly created role. Each tag consists of a key name and an associated value. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRole (Core.Maybe [Types.Tag])
crTags = Lens.field @"tags"
{-# INLINEABLE crTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRole where
        toQuery CreateRole{..}
          = Core.toQueryPair "Action" ("CreateRole" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<>
              Core.toQueryPair "AssumeRolePolicyDocument"
                assumeRolePolicyDocument
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxSessionDuration")
                maxSessionDuration
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Path") path
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PermissionsBoundary")
                permissionsBoundary
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)

instance Core.ToHeaders CreateRole where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateRole where
        type Rs CreateRole = CreateRoleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateRoleResult"
              (\ s h x ->
                 CreateRoleResponse' Core.<$>
                   (x Core..@ "Role") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'CreateRole' request. 
--
-- /See:/ 'mkCreateRoleResponse' smart constructor.
data CreateRoleResponse = CreateRoleResponse'
  { role' :: Types.Role
    -- ^ A structure containing details about the new role.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateRoleResponse' value with any optional fields omitted.
mkCreateRoleResponse
    :: Types.Role -- ^ 'role\''
    -> Core.Int -- ^ 'responseStatus'
    -> CreateRoleResponse
mkCreateRoleResponse role' responseStatus
  = CreateRoleResponse'{role', responseStatus}

-- | A structure containing details about the new role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRole :: Lens.Lens' CreateRoleResponse Types.Role
crrrsRole = Lens.field @"role'"
{-# INLINEABLE crrrsRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRoleResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
