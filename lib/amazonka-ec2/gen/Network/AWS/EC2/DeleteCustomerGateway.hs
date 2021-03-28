{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteCustomerGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified customer gateway. You must delete the VPN connection before you can delete the customer gateway.
module Network.AWS.EC2.DeleteCustomerGateway
    (
    -- * Creating a request
      DeleteCustomerGateway (..)
    , mkDeleteCustomerGateway
    -- ** Request lenses
    , dcggCustomerGatewayId
    , dcggDryRun

    -- * Destructuring the response
    , DeleteCustomerGatewayResponse (..)
    , mkDeleteCustomerGatewayResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteCustomerGateway.
--
-- /See:/ 'mkDeleteCustomerGateway' smart constructor.
data DeleteCustomerGateway = DeleteCustomerGateway'
  { customerGatewayId :: Types.CustomerGatewayId
    -- ^ The ID of the customer gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomerGateway' value with any optional fields omitted.
mkDeleteCustomerGateway
    :: Types.CustomerGatewayId -- ^ 'customerGatewayId'
    -> DeleteCustomerGateway
mkDeleteCustomerGateway customerGatewayId
  = DeleteCustomerGateway'{customerGatewayId, dryRun = Core.Nothing}

-- | The ID of the customer gateway.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcggCustomerGatewayId :: Lens.Lens' DeleteCustomerGateway Types.CustomerGatewayId
dcggCustomerGatewayId = Lens.field @"customerGatewayId"
{-# INLINEABLE dcggCustomerGatewayId #-}
{-# DEPRECATED customerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcggDryRun :: Lens.Lens' DeleteCustomerGateway (Core.Maybe Core.Bool)
dcggDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcggDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteCustomerGateway where
        toQuery DeleteCustomerGateway{..}
          = Core.toQueryPair "Action" ("DeleteCustomerGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "CustomerGatewayId" customerGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteCustomerGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCustomerGateway where
        type Rs DeleteCustomerGateway = DeleteCustomerGatewayResponse
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
        parseResponse = Response.receiveNull DeleteCustomerGatewayResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCustomerGatewayResponse' smart constructor.
data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomerGatewayResponse' value with any optional fields omitted.
mkDeleteCustomerGatewayResponse
    :: DeleteCustomerGatewayResponse
mkDeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'
