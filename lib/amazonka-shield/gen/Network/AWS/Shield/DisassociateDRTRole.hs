{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DisassociateDRTRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the DDoS Response Team's (DRT) access to your AWS account.
--
-- To make a @DisassociateDRTRole@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> . However, if you are not subscribed to one of these support plans, but had been previously and had granted the DRT access to your account, you can submit a @DisassociateDRTRole@ request to remove this access.
module Network.AWS.Shield.DisassociateDRTRole
    (
    -- * Creating a request
      DisassociateDRTRole (..)
    , mkDisassociateDRTRole

    -- * Destructuring the response
    , DisassociateDRTRoleResponse (..)
    , mkDisassociateDRTRoleResponse
    -- ** Response lenses
    , ddrtrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDisassociateDRTRole' smart constructor.
data DisassociateDRTRole = DisassociateDRTRole'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDRTRole' value with any optional fields omitted.
mkDisassociateDRTRole
    :: DisassociateDRTRole
mkDisassociateDRTRole = DisassociateDRTRole'

instance Core.ToQuery DisassociateDRTRole where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateDRTRole where
        toHeaders DisassociateDRTRole{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.DisassociateDRTRole")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateDRTRole where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DisassociateDRTRole where
        type Rs DisassociateDRTRole = DisassociateDRTRoleResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateDRTRoleResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateDRTRoleResponse' smart constructor.
newtype DisassociateDRTRoleResponse = DisassociateDRTRoleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDRTRoleResponse' value with any optional fields omitted.
mkDisassociateDRTRoleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateDRTRoleResponse
mkDisassociateDRTRoleResponse responseStatus
  = DisassociateDRTRoleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtrrrsResponseStatus :: Lens.Lens' DisassociateDRTRoleResponse Core.Int
ddrtrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrtrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
