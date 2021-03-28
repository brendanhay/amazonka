{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy of a specified type that you can attach to a root, an organizational unit (OU), or an individual AWS account.
--
-- For more information about policies and their use, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html Managing Organization Policies> .
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission.
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.CreatePolicy
    (
    -- * Creating a request
      CreatePolicy (..)
    , mkCreatePolicy
    -- ** Request lenses
    , cpContent
    , cpDescription
    , cpName
    , cpType
    , cpTags

    -- * Destructuring the response
    , CreatePolicyResponse (..)
    , mkCreatePolicyResponse
    -- ** Response lenses
    , cprrsPolicy
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { content :: Types.Content
    -- ^ The policy text content to add to the new policy. The text that you supply must adhere to the rules of the policy type you specify in the @Type@ parameter.
  , description :: Types.PolicyDescription
    -- ^ An optional description to assign to the policy.
  , name :: Types.PolicyName
    -- ^ The friendly name to assign to the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
  , type' :: Types.PolicyType
    -- ^ The type of policy to create. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY> 
--
--
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags that you want to attach to the newly created policy. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicy' value with any optional fields omitted.
mkCreatePolicy
    :: Types.Content -- ^ 'content'
    -> Types.PolicyDescription -- ^ 'description'
    -> Types.PolicyName -- ^ 'name'
    -> Types.PolicyType -- ^ 'type\''
    -> CreatePolicy
mkCreatePolicy content description name type'
  = CreatePolicy'{content, description, name, type',
                  tags = Core.Nothing}

-- | The policy text content to add to the new policy. The text that you supply must adhere to the rules of the policy type you specify in the @Type@ parameter.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpContent :: Lens.Lens' CreatePolicy Types.Content
cpContent = Lens.field @"content"
{-# INLINEABLE cpContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | An optional description to assign to the policy.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePolicy Types.PolicyDescription
cpDescription = Lens.field @"description"
{-# INLINEABLE cpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The friendly name to assign to the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePolicy Types.PolicyName
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of policy to create. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY> 
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpType :: Lens.Lens' CreatePolicy Types.PolicyType
cpType = Lens.field @"type'"
{-# INLINEABLE cpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A list of tags that you want to attach to the newly created policy. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePolicy (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# INLINEABLE cpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreatePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePolicy where
        toHeaders CreatePolicy{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.CreatePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePolicy where
        toJSON CreatePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Content" Core..= content),
                  Core.Just ("Description" Core..= description),
                  Core.Just ("Name" Core..= name), Core.Just ("Type" Core..= type'),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreatePolicy where
        type Rs CreatePolicy = CreatePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePolicyResponse' Core.<$>
                   (x Core..:? "Policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { policy :: Core.Maybe Types.Policy
    -- ^ A structure that contains details about the newly created policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePolicyResponse' value with any optional fields omitted.
mkCreatePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePolicyResponse
mkCreatePolicyResponse responseStatus
  = CreatePolicyResponse'{policy = Core.Nothing, responseStatus}

-- | A structure that contains details about the newly created policy.
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
