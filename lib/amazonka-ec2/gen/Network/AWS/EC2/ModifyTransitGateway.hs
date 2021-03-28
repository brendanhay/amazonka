{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified transit gateway. When you modify a transit gateway, the modified options are applied to new transit gateway attachments only. Your existing transit gateway attachments are not modified.
module Network.AWS.EC2.ModifyTransitGateway
    (
    -- * Creating a request
      ModifyTransitGateway (..)
    , mkModifyTransitGateway
    -- ** Request lenses
    , mtgTransitGatewayId
    , mtgDescription
    , mtgDryRun
    , mtgOptions

    -- * Destructuring the response
    , ModifyTransitGatewayResponse (..)
    , mkModifyTransitGatewayResponse
    -- ** Response lenses
    , mtgrrsTransitGateway
    , mtgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTransitGateway' smart constructor.
data ModifyTransitGateway = ModifyTransitGateway'
  { transitGatewayId :: Types.TransitGatewayId
    -- ^ The ID of the transit gateway.
  , description :: Core.Maybe Core.Text
    -- ^ The description for the transit gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , options :: Core.Maybe Types.ModifyTransitGatewayOptions
    -- ^ The options to modify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGateway' value with any optional fields omitted.
mkModifyTransitGateway
    :: Types.TransitGatewayId -- ^ 'transitGatewayId'
    -> ModifyTransitGateway
mkModifyTransitGateway transitGatewayId
  = ModifyTransitGateway'{transitGatewayId,
                          description = Core.Nothing, dryRun = Core.Nothing,
                          options = Core.Nothing}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgTransitGatewayId :: Lens.Lens' ModifyTransitGateway Types.TransitGatewayId
mtgTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE mtgTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The description for the transit gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgDescription :: Lens.Lens' ModifyTransitGateway (Core.Maybe Core.Text)
mtgDescription = Lens.field @"description"
{-# INLINEABLE mtgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgDryRun :: Lens.Lens' ModifyTransitGateway (Core.Maybe Core.Bool)
mtgDryRun = Lens.field @"dryRun"
{-# INLINEABLE mtgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The options to modify.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgOptions :: Lens.Lens' ModifyTransitGateway (Core.Maybe Types.ModifyTransitGatewayOptions)
mtgOptions = Lens.field @"options"
{-# INLINEABLE mtgOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

instance Core.ToQuery ModifyTransitGateway where
        toQuery ModifyTransitGateway{..}
          = Core.toQueryPair "Action" ("ModifyTransitGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "TransitGatewayId" transitGatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Options") options

instance Core.ToHeaders ModifyTransitGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyTransitGateway where
        type Rs ModifyTransitGateway = ModifyTransitGatewayResponse
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
          = Response.receiveXML
              (\ s h x ->
                 ModifyTransitGatewayResponse' Core.<$>
                   (x Core..@? "transitGateway") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyTransitGatewayResponse' smart constructor.
data ModifyTransitGatewayResponse = ModifyTransitGatewayResponse'
  { transitGateway :: Core.Maybe Types.TransitGateway
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyTransitGatewayResponse' value with any optional fields omitted.
mkModifyTransitGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyTransitGatewayResponse
mkModifyTransitGatewayResponse responseStatus
  = ModifyTransitGatewayResponse'{transitGateway = Core.Nothing,
                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrrsTransitGateway :: Lens.Lens' ModifyTransitGatewayResponse (Core.Maybe Types.TransitGateway)
mtgrrsTransitGateway = Lens.field @"transitGateway"
{-# INLINEABLE mtgrrsTransitGateway #-}
{-# DEPRECATED transitGateway "Use generic-lens or generic-optics with 'transitGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrrsResponseStatus :: Lens.Lens' ModifyTransitGatewayResponse Core.Int
mtgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mtgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
