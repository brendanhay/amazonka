{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new managed policy for your AWS account.
--
-- This operation creates a policy version with a version identifier of @v1@ and sets v1 as the policy's default version. For more information about policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
-- For more information about managed policies in general, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.CreatePolicy
  ( -- * Creating a request
    CreatePolicy (..),
    mkCreatePolicy,

    -- ** Request lenses
    cpPolicyName,
    cpPolicyDocument,
    cpDescription,
    cpPath,

    -- * Destructuring the response
    CreatePolicyResponse (..),
    mkCreatePolicyResponse,

    -- ** Response lenses
    cprrsPolicy,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { -- | The friendly name of the policy.
    --
    -- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
    policyName :: Types.PolicyNameType,
    -- | The JSON policy document that you want to use as the content for the new policy.
    --
    -- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
    --
    --     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
    --
    --
    --     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
    --
    --
    --     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
    policyDocument :: Types.PolicyDocumentType,
    -- | A friendly description of the policy.
    --
    -- Typically used to store information about the permissions defined in the policy. For example, "Grants access to production DynamoDB tables."
    -- The policy description is immutable. After a value is assigned, it cannot be changed.
    description :: Core.Maybe Types.Description,
    -- | The path for the policy.
    --
    -- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    -- This parameter is optional. If it is not included, it defaults to a slash (/).
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    path :: Core.Maybe Types.PolicyPathType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicy' value with any optional fields omitted.
mkCreatePolicy ::
  -- | 'policyName'
  Types.PolicyNameType ->
  -- | 'policyDocument'
  Types.PolicyDocumentType ->
  CreatePolicy
mkCreatePolicy policyName policyDocument =
  CreatePolicy'
    { policyName,
      policyDocument,
      description = Core.Nothing,
      path = Core.Nothing
    }

-- | The friendly name of the policy.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyName :: Lens.Lens' CreatePolicy Types.PolicyNameType
cpPolicyName = Lens.field @"policyName"
{-# DEPRECATED cpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON policy document that you want to use as the content for the new policy.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
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
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyDocument :: Lens.Lens' CreatePolicy Types.PolicyDocumentType
cpPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED cpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the policy. For example, "Grants access to production DynamoDB tables."
-- The policy description is immutable. After a value is assigned, it cannot be changed.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePolicy (Core.Maybe Types.Description)
cpDescription = Lens.field @"description"
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The path for the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPath :: Lens.Lens' CreatePolicy (Core.Maybe Types.PolicyPathType)
cpPath = Lens.field @"path"
{-# DEPRECATED cpPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.AWSRequest CreatePolicy where
  type Rs CreatePolicy = CreatePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreatePolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "PolicyName" policyName)
                Core.<> (Core.toQueryValue "PolicyDocument" policyDocument)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "Path" Core.<$> path)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreatePolicyResult"
      ( \s h x ->
          CreatePolicyResponse'
            Core.<$> (x Core..@? "Policy") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'CreatePolicy' request.
--
-- /See:/ 'mkCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { -- | A structure containing details about the new policy.
    policy :: Core.Maybe Types.Policy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreatePolicyResponse' value with any optional fields omitted.
mkCreatePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePolicyResponse
mkCreatePolicyResponse responseStatus =
  CreatePolicyResponse' {policy = Core.Nothing, responseStatus}

-- | A structure containing details about the new policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPolicy :: Lens.Lens' CreatePolicyResponse (Core.Maybe Types.Policy)
cprrsPolicy = Lens.field @"policy"
{-# DEPRECATED cprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePolicyResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
