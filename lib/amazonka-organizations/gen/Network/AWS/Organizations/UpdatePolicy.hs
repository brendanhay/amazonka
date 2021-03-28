{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.UpdatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing policy with a new name, description, or content. If you don't supply any parameter, that value remains unchanged. You can't change a policy's type.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.UpdatePolicy
    (
    -- * Creating a request
      UpdatePolicy (..)
    , mkUpdatePolicy
    -- ** Request lenses
    , upPolicyId
    , upContent
    , upDescription
    , upName

    -- * Destructuring the response
    , UpdatePolicyResponse (..)
    , mkUpdatePolicyResponse
    -- ** Response lenses
    , uprrsPolicy
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePolicy' smart constructor.
data UpdatePolicy = UpdatePolicy'
  { policyId :: Types.PolicyId
    -- ^ The unique identifier (ID) of the policy that you want to update.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
  , content :: Core.Maybe Types.Content
    -- ^ If provided, the new content for the policy. The text must be correctly formatted JSON that complies with the syntax for the policy's type. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide./ 
  , description :: Core.Maybe Types.Description
    -- ^ If provided, the new description for the policy.
  , name :: Core.Maybe Types.PolicyName
    -- ^ If provided, the new name for the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePolicy' value with any optional fields omitted.
mkUpdatePolicy
    :: Types.PolicyId -- ^ 'policyId'
    -> UpdatePolicy
mkUpdatePolicy policyId
  = UpdatePolicy'{policyId, content = Core.Nothing,
                  description = Core.Nothing, name = Core.Nothing}

-- | The unique identifier (ID) of the policy that you want to update.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPolicyId :: Lens.Lens' UpdatePolicy Types.PolicyId
upPolicyId = Lens.field @"policyId"
{-# INLINEABLE upPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | If provided, the new content for the policy. The text must be correctly formatted JSON that complies with the syntax for the policy's type. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide./ 
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContent :: Lens.Lens' UpdatePolicy (Core.Maybe Types.Content)
upContent = Lens.field @"content"
{-# INLINEABLE upContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | If provided, the new description for the policy.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdatePolicy (Core.Maybe Types.Description)
upDescription = Lens.field @"description"
{-# INLINEABLE upDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | If provided, the new name for the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdatePolicy (Core.Maybe Types.PolicyName)
upName = Lens.field @"name"
{-# INLINEABLE upName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdatePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePolicy where
        toHeaders UpdatePolicy{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.UpdatePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdatePolicy where
        toJSON UpdatePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyId" Core..= policyId),
                  ("Content" Core..=) Core.<$> content,
                  ("Description" Core..=) Core.<$> description,
                  ("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdatePolicy where
        type Rs UpdatePolicy = UpdatePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdatePolicyResponse' Core.<$>
                   (x Core..:? "Policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdatePolicyResponse' smart constructor.
data UpdatePolicyResponse = UpdatePolicyResponse'
  { policy :: Core.Maybe Types.Policy
    -- ^ A structure that contains details about the updated policy, showing the requested changes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePolicyResponse' value with any optional fields omitted.
mkUpdatePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdatePolicyResponse
mkUpdatePolicyResponse responseStatus
  = UpdatePolicyResponse'{policy = Core.Nothing, responseStatus}

-- | A structure that contains details about the updated policy, showing the requested changes.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsPolicy :: Lens.Lens' UpdatePolicyResponse (Core.Maybe Types.Policy)
uprrsPolicy = Lens.field @"policy"
{-# INLINEABLE uprrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdatePolicyResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
