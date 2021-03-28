{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.AcceptHandshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a response to the originator of a handshake agreeing to the action proposed by the handshake request.
--
-- This operation can be called only by the following principals when they also have the relevant IAM permissions:
--
--     * __Invitation to join__ or __Approve all features request__ handshakes: only a principal from the member account.
-- The user who calls the API for an invitation to join must have the @organizations:AcceptHandshake@ permission. If you enabled all features in the organization, the user must also have the @iam:CreateServiceLinkedRole@ permission so that AWS Organizations can create the required service-linked role named @AWSServiceRoleForOrganizations@ . For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integration_services.html#orgs_integration_service-linked-roles AWS Organizations and Service-Linked Roles> in the /AWS Organizations User Guide/ .
--
--
--     * __Enable all features final confirmation__ handshake: only a principal from the management account.
-- For more information about invitations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_invites.html Inviting an AWS Account to Join Your Organization> in the /AWS Organizations User Guide./ For more information about requests to enable all features in the organization, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide./ 
--
--
-- After you accept a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that, it's deleted.
module Network.AWS.Organizations.AcceptHandshake
    (
    -- * Creating a request
      AcceptHandshake (..)
    , mkAcceptHandshake
    -- ** Request lenses
    , ahHandshakeId

    -- * Destructuring the response
    , AcceptHandshakeResponse (..)
    , mkAcceptHandshakeResponse
    -- ** Response lenses
    , ahrrsHandshake
    , ahrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptHandshake' smart constructor.
newtype AcceptHandshake = AcceptHandshake'
  { handshakeId :: Types.HandshakeId
    -- ^ The unique identifier (ID) of the handshake that you want to accept.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptHandshake' value with any optional fields omitted.
mkAcceptHandshake
    :: Types.HandshakeId -- ^ 'handshakeId'
    -> AcceptHandshake
mkAcceptHandshake handshakeId = AcceptHandshake'{handshakeId}

-- | The unique identifier (ID) of the handshake that you want to accept.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'handshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahHandshakeId :: Lens.Lens' AcceptHandshake Types.HandshakeId
ahHandshakeId = Lens.field @"handshakeId"
{-# INLINEABLE ahHandshakeId #-}
{-# DEPRECATED handshakeId "Use generic-lens or generic-optics with 'handshakeId' instead"  #-}

instance Core.ToQuery AcceptHandshake where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AcceptHandshake where
        toHeaders AcceptHandshake{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.AcceptHandshake")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AcceptHandshake where
        toJSON AcceptHandshake{..}
          = Core.object
              (Core.catMaybes [Core.Just ("HandshakeId" Core..= handshakeId)])

instance Core.AWSRequest AcceptHandshake where
        type Rs AcceptHandshake = AcceptHandshakeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AcceptHandshakeResponse' Core.<$>
                   (x Core..:? "Handshake") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptHandshakeResponse' smart constructor.
data AcceptHandshakeResponse = AcceptHandshakeResponse'
  { handshake :: Core.Maybe Types.Handshake
    -- ^ A structure that contains details about the accepted handshake.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AcceptHandshakeResponse' value with any optional fields omitted.
mkAcceptHandshakeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptHandshakeResponse
mkAcceptHandshakeResponse responseStatus
  = AcceptHandshakeResponse'{handshake = Core.Nothing,
                             responseStatus}

-- | A structure that contains details about the accepted handshake.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrrsHandshake :: Lens.Lens' AcceptHandshakeResponse (Core.Maybe Types.Handshake)
ahrrsHandshake = Lens.field @"handshake"
{-# INLINEABLE ahrrsHandshake #-}
{-# DEPRECATED handshake "Use generic-lens or generic-optics with 'handshake' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrrsResponseStatus :: Lens.Lens' AcceptHandshakeResponse Core.Int
ahrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ahrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
