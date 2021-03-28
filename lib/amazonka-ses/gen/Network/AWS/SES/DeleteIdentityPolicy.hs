{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteIdentityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified sending authorization policy for the given identity (an email address or a domain). This API returns successfully even if a policy with the specified name does not exist.
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteIdentityPolicy
    (
    -- * Creating a request
      DeleteIdentityPolicy (..)
    , mkDeleteIdentityPolicy
    -- ** Request lenses
    , dipIdentity
    , dipPolicyName

    -- * Destructuring the response
    , DeleteIdentityPolicyResponse (..)
    , mkDeleteIdentityPolicyResponse
    -- ** Response lenses
    , diprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteIdentityPolicy' smart constructor.
data DeleteIdentityPolicy = DeleteIdentityPolicy'
  { identity :: Types.Identity
    -- ^ The identity that is associated with the policy that you want to delete. You can specify the identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
  , policyName :: Types.PolicyName
    -- ^ The name of the policy to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityPolicy' value with any optional fields omitted.
mkDeleteIdentityPolicy
    :: Types.Identity -- ^ 'identity'
    -> Types.PolicyName -- ^ 'policyName'
    -> DeleteIdentityPolicy
mkDeleteIdentityPolicy identity policyName
  = DeleteIdentityPolicy'{identity, policyName}

-- | The identity that is associated with the policy that you want to delete. You can specify the identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipIdentity :: Lens.Lens' DeleteIdentityPolicy Types.Identity
dipIdentity = Lens.field @"identity"
{-# INLINEABLE dipIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

-- | The name of the policy to be deleted.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipPolicyName :: Lens.Lens' DeleteIdentityPolicy Types.PolicyName
dipPolicyName = Lens.field @"policyName"
{-# INLINEABLE dipPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery DeleteIdentityPolicy where
        toQuery DeleteIdentityPolicy{..}
          = Core.toQueryPair "Action" ("DeleteIdentityPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Identity" identity
              Core.<> Core.toQueryPair "PolicyName" policyName

instance Core.ToHeaders DeleteIdentityPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteIdentityPolicy where
        type Rs DeleteIdentityPolicy = DeleteIdentityPolicyResponse
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
          = Response.receiveXMLWrapper "DeleteIdentityPolicyResult"
              (\ s h x ->
                 DeleteIdentityPolicyResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteIdentityPolicyResponse' smart constructor.
newtype DeleteIdentityPolicyResponse = DeleteIdentityPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityPolicyResponse' value with any optional fields omitted.
mkDeleteIdentityPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteIdentityPolicyResponse
mkDeleteIdentityPolicyResponse responseStatus
  = DeleteIdentityPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsResponseStatus :: Lens.Lens' DeleteIdentityPolicyResponse Core.Int
diprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
