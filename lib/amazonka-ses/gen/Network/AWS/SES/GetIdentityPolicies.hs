{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the requested sending authorization policies for the given identity (an email address or a domain). The policies are returned as a map of policy names to policy contents. You can retrieve a maximum of 20 policies at a time.
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetIdentityPolicies
    (
    -- * Creating a request
      GetIdentityPolicies (..)
    , mkGetIdentityPolicies
    -- ** Request lenses
    , gipIdentity
    , gipPolicyNames

    -- * Destructuring the response
    , GetIdentityPoliciesResponse (..)
    , mkGetIdentityPoliciesResponse
    -- ** Response lenses
    , giprrsPolicies
    , giprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to return the requested sending authorization policies for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkGetIdentityPolicies' smart constructor.
data GetIdentityPolicies = GetIdentityPolicies'
  { identity :: Types.Identity
    -- ^ The identity for which the policies will be retrieved. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
  , policyNames :: [Types.PolicyName]
    -- ^ A list of the names of policies to be retrieved. You can retrieve a maximum of 20 policies at a time. If you do not know the names of the policies that are attached to the identity, you can use @ListIdentityPolicies@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityPolicies' value with any optional fields omitted.
mkGetIdentityPolicies
    :: Types.Identity -- ^ 'identity'
    -> GetIdentityPolicies
mkGetIdentityPolicies identity
  = GetIdentityPolicies'{identity, policyNames = Core.mempty}

-- | The identity for which the policies will be retrieved. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipIdentity :: Lens.Lens' GetIdentityPolicies Types.Identity
gipIdentity = Lens.field @"identity"
{-# INLINEABLE gipIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

-- | A list of the names of policies to be retrieved. You can retrieve a maximum of 20 policies at a time. If you do not know the names of the policies that are attached to the identity, you can use @ListIdentityPolicies@ .
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipPolicyNames :: Lens.Lens' GetIdentityPolicies [Types.PolicyName]
gipPolicyNames = Lens.field @"policyNames"
{-# INLINEABLE gipPolicyNames #-}
{-# DEPRECATED policyNames "Use generic-lens or generic-optics with 'policyNames' instead"  #-}

instance Core.ToQuery GetIdentityPolicies where
        toQuery GetIdentityPolicies{..}
          = Core.toQueryPair "Action" ("GetIdentityPolicies" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Identity" identity
              Core.<>
              Core.toQueryPair "PolicyNames"
                (Core.toQueryList "member" policyNames)

instance Core.ToHeaders GetIdentityPolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetIdentityPolicies where
        type Rs GetIdentityPolicies = GetIdentityPoliciesResponse
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
          = Response.receiveXMLWrapper "GetIdentityPoliciesResult"
              (\ s h x ->
                 GetIdentityPoliciesResponse' Core.<$>
                   (x Core..@ "Policies" Core..@! Core.mempty Core..<@>
                      Core.parseXMLMap "entry" "key" "value")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the requested sending authorization policies.
--
-- /See:/ 'mkGetIdentityPoliciesResponse' smart constructor.
data GetIdentityPoliciesResponse = GetIdentityPoliciesResponse'
  { policies :: Core.HashMap Types.PolicyName Types.Policy
    -- ^ A map of policy names to policies.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityPoliciesResponse' value with any optional fields omitted.
mkGetIdentityPoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetIdentityPoliciesResponse
mkGetIdentityPoliciesResponse responseStatus
  = GetIdentityPoliciesResponse'{policies = Core.mempty,
                                 responseStatus}

-- | A map of policy names to policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsPolicies :: Lens.Lens' GetIdentityPoliciesResponse (Core.HashMap Types.PolicyName Types.Policy)
giprrsPolicies = Lens.field @"policies"
{-# INLINEABLE giprrsPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsResponseStatus :: Lens.Lens' GetIdentityPoliciesResponse Core.Int
giprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE giprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
