{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded in the specified IAM group.
--
-- An IAM group can also have managed policies attached to it. To retrieve a managed policy document that is attached to a group, use 'GetPolicy' to determine the policy's default version, then use 'GetPolicyVersion' to retrieve the policy document.
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.GetGroupPolicy
    (
    -- * Creating a request
      GetGroupPolicy (..)
    , mkGetGroupPolicy
    -- ** Request lenses
    , ggpGroupName
    , ggpPolicyName

    -- * Destructuring the response
    , GetGroupPolicyResponse (..)
    , mkGetGroupPolicyResponse
    -- ** Response lenses
    , ggprrsGroupName
    , ggprrsPolicyName
    , ggprrsPolicyDocument
    , ggprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroupPolicy' smart constructor.
data GetGroupPolicy = GetGroupPolicy'
  { groupName :: Types.GroupName
    -- ^ The name of the group the policy is associated with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyName :: Types.PolicyName
    -- ^ The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupPolicy' value with any optional fields omitted.
mkGetGroupPolicy
    :: Types.GroupName -- ^ 'groupName'
    -> Types.PolicyName -- ^ 'policyName'
    -> GetGroupPolicy
mkGetGroupPolicy groupName policyName
  = GetGroupPolicy'{groupName, policyName}

-- | The name of the group the policy is associated with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggpGroupName :: Lens.Lens' GetGroupPolicy Types.GroupName
ggpGroupName = Lens.field @"groupName"
{-# INLINEABLE ggpGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggpPolicyName :: Lens.Lens' GetGroupPolicy Types.PolicyName
ggpPolicyName = Lens.field @"policyName"
{-# INLINEABLE ggpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery GetGroupPolicy where
        toQuery GetGroupPolicy{..}
          = Core.toQueryPair "Action" ("GetGroupPolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "GroupName" groupName
              Core.<> Core.toQueryPair "PolicyName" policyName

instance Core.ToHeaders GetGroupPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetGroupPolicy where
        type Rs GetGroupPolicy = GetGroupPolicyResponse
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
          = Response.receiveXMLWrapper "GetGroupPolicyResult"
              (\ s h x ->
                 GetGroupPolicyResponse' Core.<$>
                   (x Core..@ "GroupName") Core.<*> x Core..@ "PolicyName" Core.<*>
                     x Core..@ "PolicyDocument"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetGroupPolicy' request. 
--
-- /See:/ 'mkGetGroupPolicyResponse' smart constructor.
data GetGroupPolicyResponse = GetGroupPolicyResponse'
  { groupName :: Types.GroupName
    -- ^ The group the policy is associated with.
  , policyName :: Types.PolicyName
    -- ^ The name of the policy.
  , policyDocument :: Types.PolicyDocument
    -- ^ The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupPolicyResponse' value with any optional fields omitted.
mkGetGroupPolicyResponse
    :: Types.GroupName -- ^ 'groupName'
    -> Types.PolicyName -- ^ 'policyName'
    -> Types.PolicyDocument -- ^ 'policyDocument'
    -> Core.Int -- ^ 'responseStatus'
    -> GetGroupPolicyResponse
mkGetGroupPolicyResponse groupName policyName policyDocument
  responseStatus
  = GetGroupPolicyResponse'{groupName, policyName, policyDocument,
                            responseStatus}

-- | The group the policy is associated with.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggprrsGroupName :: Lens.Lens' GetGroupPolicyResponse Types.GroupName
ggprrsGroupName = Lens.field @"groupName"
{-# INLINEABLE ggprrsGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggprrsPolicyName :: Lens.Lens' GetGroupPolicyResponse Types.PolicyName
ggprrsPolicyName = Lens.field @"policyName"
{-# INLINEABLE ggprrsPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggprrsPolicyDocument :: Lens.Lens' GetGroupPolicyResponse Types.PolicyDocument
ggprrsPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE ggprrsPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggprrsResponseStatus :: Lens.Lens' GetGroupPolicyResponse Core.Int
ggprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
