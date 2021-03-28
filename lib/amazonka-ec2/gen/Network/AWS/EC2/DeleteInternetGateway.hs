{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified internet gateway. You must detach the internet gateway from the VPC before you can delete it.
module Network.AWS.EC2.DeleteInternetGateway
    (
    -- * Creating a request
      DeleteInternetGateway (..)
    , mkDeleteInternetGateway
    -- ** Request lenses
    , digfInternetGatewayId
    , digfDryRun

    -- * Destructuring the response
    , DeleteInternetGatewayResponse (..)
    , mkDeleteInternetGatewayResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInternetGateway' smart constructor.
data DeleteInternetGateway = DeleteInternetGateway'
  { internetGatewayId :: Types.InternetGatewayId
    -- ^ The ID of the internet gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInternetGateway' value with any optional fields omitted.
mkDeleteInternetGateway
    :: Types.InternetGatewayId -- ^ 'internetGatewayId'
    -> DeleteInternetGateway
mkDeleteInternetGateway internetGatewayId
  = DeleteInternetGateway'{internetGatewayId, dryRun = Core.Nothing}

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digfInternetGatewayId :: Lens.Lens' DeleteInternetGateway Types.InternetGatewayId
digfInternetGatewayId = Lens.field @"internetGatewayId"
{-# INLINEABLE digfInternetGatewayId #-}
{-# DEPRECATED internetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digfDryRun :: Lens.Lens' DeleteInternetGateway (Core.Maybe Core.Bool)
digfDryRun = Lens.field @"dryRun"
{-# INLINEABLE digfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteInternetGateway where
        toQuery DeleteInternetGateway{..}
          = Core.toQueryPair "Action" ("DeleteInternetGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InternetGatewayId" internetGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteInternetGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteInternetGateway where
        type Rs DeleteInternetGateway = DeleteInternetGatewayResponse
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
        parseResponse = Response.receiveNull DeleteInternetGatewayResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteInternetGatewayResponse' smart constructor.
data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInternetGatewayResponse' value with any optional fields omitted.
mkDeleteInternetGatewayResponse
    :: DeleteInternetGatewayResponse
mkDeleteInternetGatewayResponse = DeleteInternetGatewayResponse'
