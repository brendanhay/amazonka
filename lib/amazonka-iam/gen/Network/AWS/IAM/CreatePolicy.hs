{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreatePolicy (..)
    , mkCreatePolicy
    -- ** Request lenses
    , cpPolicyName
    , cpPolicyDocument
    , cpDescription
    , cpPath

    -- * Destructuring the response
    , CreatePolicyResponse (..)
    , mkCreatePolicyResponse
    -- ** Response lenses
    , cprrsPolicy
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { policyName :: Types.PolicyNameType
    -- ^ The friendly name of the policy.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
  , policyDocument :: Types.PolicyDocumentType
    -- ^ The JSON policy document that you want to use as the content for the new policy.
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
  , description :: Core.Maybe Types.Description
    -- ^ A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the policy. For example, "Grants access to production DynamoDB tables."
-- The policy description is immutable. After a value is assigned, it cannot be changed.
  , path :: Core.Maybe Types.PolicyPathType
    -- ^ The path for the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicy' value with any optional fields omitted.
mkCreatePolicy
    :: Types.PolicyNameType -- ^ 'policyName'
    -> Types.PolicyDocumentType -- ^ 'policyDocument'
    -> CreatePolicy
mkCreatePolicy policyName policyDocument
  = CreatePolicy'{policyName, policyDocument,
                  description = Core.Nothing, path = Core.Nothing}

-- | The friendly name of the policy.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyName :: Lens.Lens' CreatePolicy Types.PolicyNameType
cpPolicyName = Lens.field @"policyName"
{-# INLINEABLE cpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

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
{-# INLINEABLE cpPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the policy. For example, "Grants access to production DynamoDB tables."
-- The policy description is immutable. After a value is assigned, it cannot be changed.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePolicy (Core.Maybe Types.Description)
cpDescription = Lens.field @"description"
{-# INLINEABLE cpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The path for the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPath :: Lens.Lens' CreatePolicy (Core.Maybe Types.PolicyPathType)
cpPath = Lens.field @"path"
{-# INLINEABLE cpPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.ToQuery CreatePolicy where
        toQuery CreatePolicy{..}
          = Core.toQueryPair "Action" ("CreatePolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<> Core.toQueryPair "PolicyDocument" policyDocument
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Path") path

instance Core.ToHeaders CreatePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreatePolicy where
        type Rs CreatePolicy = CreatePolicyResponse
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
          = Response.receiveXMLWrapper "CreatePolicyResult"
              (\ s h x ->
                 CreatePolicyResponse' Core.<$>
                   (x Core..@? "Policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'CreatePolicy' request. 
--
-- /See:/ 'mkCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { policy :: Core.Maybe Types.Policy
    -- ^ A structure containing details about the new policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePolicyResponse' value with any optional fields omitted.
mkCreatePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePolicyResponse
mkCreatePolicyResponse responseStatus
  = CreatePolicyResponse'{policy = Core.Nothing, responseStatus}

-- | A structure containing details about the new policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPolicy :: Lens.Lens' CreatePolicyResponse (Core.Maybe Types.Policy)
cprrsPolicy = Lens.field @"policy"
{-# INLINEABLE cprrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePolicyResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
