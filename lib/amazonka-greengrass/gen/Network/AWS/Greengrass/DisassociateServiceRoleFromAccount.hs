{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the service role from your account. Without a service role, deployments will not work.
module Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
    (
    -- * Creating a request
      DisassociateServiceRoleFromAccount (..)
    , mkDisassociateServiceRoleFromAccount

    -- * Destructuring the response
    , DisassociateServiceRoleFromAccountResponse (..)
    , mkDisassociateServiceRoleFromAccountResponse
    -- ** Response lenses
    , dsrfarrsDisassociatedAt
    , dsrfarrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateServiceRoleFromAccount' smart constructor.
data DisassociateServiceRoleFromAccount = DisassociateServiceRoleFromAccount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateServiceRoleFromAccount' value with any optional fields omitted.
mkDisassociateServiceRoleFromAccount
    :: DisassociateServiceRoleFromAccount
mkDisassociateServiceRoleFromAccount
  = DisassociateServiceRoleFromAccount'

instance Core.ToQuery DisassociateServiceRoleFromAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateServiceRoleFromAccount where
        toHeaders DisassociateServiceRoleFromAccount{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DisassociateServiceRoleFromAccount where
        type Rs DisassociateServiceRoleFromAccount =
             DisassociateServiceRoleFromAccountResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/greengrass/servicerole",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DisassociateServiceRoleFromAccountResponse' Core.<$>
                   (x Core..:? "DisassociatedAt") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateServiceRoleFromAccountResponse' smart constructor.
data DisassociateServiceRoleFromAccountResponse = DisassociateServiceRoleFromAccountResponse'
  { disassociatedAt :: Core.Maybe Core.Text
    -- ^ The time when the service role was disassociated from the account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateServiceRoleFromAccountResponse' value with any optional fields omitted.
mkDisassociateServiceRoleFromAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateServiceRoleFromAccountResponse
mkDisassociateServiceRoleFromAccountResponse responseStatus
  = DisassociateServiceRoleFromAccountResponse'{disassociatedAt =
                                                  Core.Nothing,
                                                responseStatus}

-- | The time when the service role was disassociated from the account.
--
-- /Note:/ Consider using 'disassociatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfarrsDisassociatedAt :: Lens.Lens' DisassociateServiceRoleFromAccountResponse (Core.Maybe Core.Text)
dsrfarrsDisassociatedAt = Lens.field @"disassociatedAt"
{-# INLINEABLE dsrfarrsDisassociatedAt #-}
{-# DEPRECATED disassociatedAt "Use generic-lens or generic-optics with 'disassociatedAt' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfarrsResponseStatus :: Lens.Lens' DisassociateServiceRoleFromAccountResponse Core.Int
dsrfarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
