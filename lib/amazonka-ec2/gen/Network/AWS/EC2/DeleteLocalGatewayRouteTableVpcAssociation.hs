{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRouteTableVpcAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified association between a VPC and local gateway route table.
module Network.AWS.EC2.DeleteLocalGatewayRouteTableVpcAssociation
    (
    -- * Creating a request
      DeleteLocalGatewayRouteTableVpcAssociation (..)
    , mkDeleteLocalGatewayRouteTableVpcAssociation
    -- ** Request lenses
    , dlgrtvaLocalGatewayRouteTableVpcAssociationId
    , dlgrtvaDryRun

    -- * Destructuring the response
    , DeleteLocalGatewayRouteTableVpcAssociationResponse (..)
    , mkDeleteLocalGatewayRouteTableVpcAssociationResponse
    -- ** Response lenses
    , dlgrtvarrsLocalGatewayRouteTableVpcAssociation
    , dlgrtvarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLocalGatewayRouteTableVpcAssociation' smart constructor.
data DeleteLocalGatewayRouteTableVpcAssociation = DeleteLocalGatewayRouteTableVpcAssociation'
  { localGatewayRouteTableVpcAssociationId :: Types.LocalGatewayRouteTableVpcAssociationId
    -- ^ The ID of the association.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLocalGatewayRouteTableVpcAssociation' value with any optional fields omitted.
mkDeleteLocalGatewayRouteTableVpcAssociation
    :: Types.LocalGatewayRouteTableVpcAssociationId -- ^ 'localGatewayRouteTableVpcAssociationId'
    -> DeleteLocalGatewayRouteTableVpcAssociation
mkDeleteLocalGatewayRouteTableVpcAssociation
  localGatewayRouteTableVpcAssociationId
  = DeleteLocalGatewayRouteTableVpcAssociation'{localGatewayRouteTableVpcAssociationId,
                                                dryRun = Core.Nothing}

-- | The ID of the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVpcAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvaLocalGatewayRouteTableVpcAssociationId :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociation Types.LocalGatewayRouteTableVpcAssociationId
dlgrtvaLocalGatewayRouteTableVpcAssociationId = Lens.field @"localGatewayRouteTableVpcAssociationId"
{-# INLINEABLE dlgrtvaLocalGatewayRouteTableVpcAssociationId #-}
{-# DEPRECATED localGatewayRouteTableVpcAssociationId "Use generic-lens or generic-optics with 'localGatewayRouteTableVpcAssociationId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvaDryRun :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociation (Core.Maybe Core.Bool)
dlgrtvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dlgrtvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteLocalGatewayRouteTableVpcAssociation
         where
        toQuery DeleteLocalGatewayRouteTableVpcAssociation{..}
          = Core.toQueryPair "Action"
              ("DeleteLocalGatewayRouteTableVpcAssociation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "LocalGatewayRouteTableVpcAssociationId"
                localGatewayRouteTableVpcAssociationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteLocalGatewayRouteTableVpcAssociation
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteLocalGatewayRouteTableVpcAssociation
         where
        type Rs DeleteLocalGatewayRouteTableVpcAssociation =
             DeleteLocalGatewayRouteTableVpcAssociationResponse
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
                 DeleteLocalGatewayRouteTableVpcAssociationResponse' Core.<$>
                   (x Core..@? "localGatewayRouteTableVpcAssociation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLocalGatewayRouteTableVpcAssociationResponse' smart constructor.
data DeleteLocalGatewayRouteTableVpcAssociationResponse = DeleteLocalGatewayRouteTableVpcAssociationResponse'
  { localGatewayRouteTableVpcAssociation :: Core.Maybe Types.LocalGatewayRouteTableVpcAssociation
    -- ^ Information about the association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLocalGatewayRouteTableVpcAssociationResponse' value with any optional fields omitted.
mkDeleteLocalGatewayRouteTableVpcAssociationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLocalGatewayRouteTableVpcAssociationResponse
mkDeleteLocalGatewayRouteTableVpcAssociationResponse responseStatus
  = DeleteLocalGatewayRouteTableVpcAssociationResponse'{localGatewayRouteTableVpcAssociation
                                                          = Core.Nothing,
                                                        responseStatus}

-- | Information about the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVpcAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvarrsLocalGatewayRouteTableVpcAssociation :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociationResponse (Core.Maybe Types.LocalGatewayRouteTableVpcAssociation)
dlgrtvarrsLocalGatewayRouteTableVpcAssociation = Lens.field @"localGatewayRouteTableVpcAssociation"
{-# INLINEABLE dlgrtvarrsLocalGatewayRouteTableVpcAssociation #-}
{-# DEPRECATED localGatewayRouteTableVpcAssociation "Use generic-lens or generic-optics with 'localGatewayRouteTableVpcAssociation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvarrsResponseStatus :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociationResponse Core.Int
dlgrtvarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlgrtvarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
