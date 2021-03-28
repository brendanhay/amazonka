{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.PutIdentityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a sending authorization policy for the specified identity (an email address or a domain).
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.PutIdentityPolicy
    (
    -- * Creating a request
      PutIdentityPolicy (..)
    , mkPutIdentityPolicy
    -- ** Request lenses
    , pipIdentity
    , pipPolicyName
    , pipPolicy

    -- * Destructuring the response
    , PutIdentityPolicyResponse (..)
    , mkPutIdentityPolicyResponse
    -- ** Response lenses
    , piprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to add or update a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkPutIdentityPolicy' smart constructor.
data PutIdentityPolicy = PutIdentityPolicy'
  { identity :: Types.Identity
    -- ^ The identity that the policy will apply to. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
  , policyName :: Types.PolicyName
    -- ^ The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include alphanumeric characters, dashes, and underscores.
  , policy :: Types.Policy
    -- ^ The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutIdentityPolicy' value with any optional fields omitted.
mkPutIdentityPolicy
    :: Types.Identity -- ^ 'identity'
    -> Types.PolicyName -- ^ 'policyName'
    -> Types.Policy -- ^ 'policy'
    -> PutIdentityPolicy
mkPutIdentityPolicy identity policyName policy
  = PutIdentityPolicy'{identity, policyName, policy}

-- | The identity that the policy will apply to. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipIdentity :: Lens.Lens' PutIdentityPolicy Types.Identity
pipIdentity = Lens.field @"identity"
{-# INLINEABLE pipIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

-- | The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include alphanumeric characters, dashes, and underscores.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyName :: Lens.Lens' PutIdentityPolicy Types.PolicyName
pipPolicyName = Lens.field @"policyName"
{-# INLINEABLE pipPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide> . 
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicy :: Lens.Lens' PutIdentityPolicy Types.Policy
pipPolicy = Lens.field @"policy"
{-# INLINEABLE pipPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

instance Core.ToQuery PutIdentityPolicy where
        toQuery PutIdentityPolicy{..}
          = Core.toQueryPair "Action" ("PutIdentityPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Identity" identity
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<> Core.toQueryPair "Policy" policy

instance Core.ToHeaders PutIdentityPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutIdentityPolicy where
        type Rs PutIdentityPolicy = PutIdentityPolicyResponse
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
          = Response.receiveXMLWrapper "PutIdentityPolicyResult"
              (\ s h x ->
                 PutIdentityPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkPutIdentityPolicyResponse' smart constructor.
newtype PutIdentityPolicyResponse = PutIdentityPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutIdentityPolicyResponse' value with any optional fields omitted.
mkPutIdentityPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutIdentityPolicyResponse
mkPutIdentityPolicyResponse responseStatus
  = PutIdentityPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprrsResponseStatus :: Lens.Lens' PutIdentityPolicyResponse Core.Int
piprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE piprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
