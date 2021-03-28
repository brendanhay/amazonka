{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListIdentityPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of sending authorization policies that are attached to the given identity (an email address or a domain). This API returns only a list. If you want the actual policy content, you can use @GetIdentityPolicies@ .
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.ListIdentityPolicies
    (
    -- * Creating a request
      ListIdentityPolicies (..)
    , mkListIdentityPolicies
    -- ** Request lenses
    , lipIdentity

    -- * Destructuring the response
    , ListIdentityPoliciesResponse (..)
    , mkListIdentityPoliciesResponse
    -- ** Response lenses
    , liprrsPolicyNames
    , liprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to return a list of sending authorization policies that are attached to an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListIdentityPolicies' smart constructor.
newtype ListIdentityPolicies = ListIdentityPolicies'
  { identity :: Types.Identity
    -- ^ The identity that is associated with the policy for which the policies will be listed. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentityPolicies' value with any optional fields omitted.
mkListIdentityPolicies
    :: Types.Identity -- ^ 'identity'
    -> ListIdentityPolicies
mkListIdentityPolicies identity = ListIdentityPolicies'{identity}

-- | The identity that is associated with the policy for which the policies will be listed. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipIdentity :: Lens.Lens' ListIdentityPolicies Types.Identity
lipIdentity = Lens.field @"identity"
{-# INLINEABLE lipIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

instance Core.ToQuery ListIdentityPolicies where
        toQuery ListIdentityPolicies{..}
          = Core.toQueryPair "Action" ("ListIdentityPolicies" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Identity" identity

instance Core.ToHeaders ListIdentityPolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListIdentityPolicies where
        type Rs ListIdentityPolicies = ListIdentityPoliciesResponse
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
          = Response.receiveXMLWrapper "ListIdentityPoliciesResult"
              (\ s h x ->
                 ListIdentityPoliciesResponse' Core.<$>
                   (x Core..@ "PolicyNames" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A list of names of sending authorization policies that apply to an identity.
--
-- /See:/ 'mkListIdentityPoliciesResponse' smart constructor.
data ListIdentityPoliciesResponse = ListIdentityPoliciesResponse'
  { policyNames :: [Types.PolicyName]
    -- ^ A list of names of policies that apply to the specified identity.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIdentityPoliciesResponse' value with any optional fields omitted.
mkListIdentityPoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListIdentityPoliciesResponse
mkListIdentityPoliciesResponse responseStatus
  = ListIdentityPoliciesResponse'{policyNames = Core.mempty,
                                  responseStatus}

-- | A list of names of policies that apply to the specified identity.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsPolicyNames :: Lens.Lens' ListIdentityPoliciesResponse [Types.PolicyName]
liprrsPolicyNames = Lens.field @"policyNames"
{-# INLINEABLE liprrsPolicyNames #-}
{-# DEPRECATED policyNames "Use generic-lens or generic-optics with 'policyNames' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprrsResponseStatus :: Lens.Lens' ListIdentityPoliciesResponse Core.Int
liprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE liprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
