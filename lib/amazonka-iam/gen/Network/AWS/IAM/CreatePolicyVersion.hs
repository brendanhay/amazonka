{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreatePolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified managed policy. To update a managed policy, you create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must delete an existing version using 'DeletePolicyVersion' before you create a new version.
--
-- Optionally, you can set the new version as the policy's default version. The default version is the version that is in effect for the IAM users, groups, and roles to which the policy is attached.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.CreatePolicyVersion
    (
    -- * Creating a request
      CreatePolicyVersion (..)
    , mkCreatePolicyVersion
    -- ** Request lenses
    , cpvPolicyArn
    , cpvPolicyDocument
    , cpvSetAsDefault

    -- * Destructuring the response
    , CreatePolicyVersionResponse (..)
    , mkCreatePolicyVersionResponse
    -- ** Response lenses
    , cpvrrsPolicyVersion
    , cpvrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { policyArn :: Types.PolicyArn
    -- ^ The Amazon Resource Name (ARN) of the IAM policy to which you want to add a new version.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , policyDocument :: Types.PolicyDocument
    -- ^ The JSON policy document that you want to use as the content for this new version of the policy.
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
  , setAsDefault :: Core.Maybe Core.Bool
    -- ^ Specifies whether to set this version as the policy's default version.
--
-- When this parameter is @true@ , the new policy version becomes the operative version. That is, it becomes the version that is in effect for the IAM users, groups, and roles that the policy is attached to.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicyVersion' value with any optional fields omitted.
mkCreatePolicyVersion
    :: Types.PolicyArn -- ^ 'policyArn'
    -> Types.PolicyDocument -- ^ 'policyDocument'
    -> CreatePolicyVersion
mkCreatePolicyVersion policyArn policyDocument
  = CreatePolicyVersion'{policyArn, policyDocument,
                         setAsDefault = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM policy to which you want to add a new version.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPolicyArn :: Lens.Lens' CreatePolicyVersion Types.PolicyArn
cpvPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE cpvPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | The JSON policy document that you want to use as the content for this new version of the policy.
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
cpvPolicyDocument :: Lens.Lens' CreatePolicyVersion Types.PolicyDocument
cpvPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE cpvPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | Specifies whether to set this version as the policy's default version.
--
-- When this parameter is @true@ , the new policy version becomes the operative version. That is, it becomes the version that is in effect for the IAM users, groups, and roles that the policy is attached to.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'setAsDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvSetAsDefault :: Lens.Lens' CreatePolicyVersion (Core.Maybe Core.Bool)
cpvSetAsDefault = Lens.field @"setAsDefault"
{-# INLINEABLE cpvSetAsDefault #-}
{-# DEPRECATED setAsDefault "Use generic-lens or generic-optics with 'setAsDefault' instead"  #-}

instance Core.ToQuery CreatePolicyVersion where
        toQuery CreatePolicyVersion{..}
          = Core.toQueryPair "Action" ("CreatePolicyVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "PolicyArn" policyArn
              Core.<> Core.toQueryPair "PolicyDocument" policyDocument
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SetAsDefault")
                setAsDefault

instance Core.ToHeaders CreatePolicyVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreatePolicyVersion where
        type Rs CreatePolicyVersion = CreatePolicyVersionResponse
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
          = Response.receiveXMLWrapper "CreatePolicyVersionResult"
              (\ s h x ->
                 CreatePolicyVersionResponse' Core.<$>
                   (x Core..@? "PolicyVersion") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'CreatePolicyVersion' request. 
--
-- /See:/ 'mkCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { policyVersion :: Core.Maybe Types.PolicyVersion
    -- ^ A structure containing details about the new policy version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePolicyVersionResponse' value with any optional fields omitted.
mkCreatePolicyVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePolicyVersionResponse
mkCreatePolicyVersionResponse responseStatus
  = CreatePolicyVersionResponse'{policyVersion = Core.Nothing,
                                 responseStatus}

-- | A structure containing details about the new policy version.
--
-- /Note:/ Consider using 'policyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsPolicyVersion :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe Types.PolicyVersion)
cpvrrsPolicyVersion = Lens.field @"policyVersion"
{-# INLINEABLE cpvrrsPolicyVersion #-}
{-# DEPRECATED policyVersion "Use generic-lens or generic-optics with 'policyVersion' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrrsResponseStatus :: Lens.Lens' CreatePolicyVersionResponse Core.Int
cpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
