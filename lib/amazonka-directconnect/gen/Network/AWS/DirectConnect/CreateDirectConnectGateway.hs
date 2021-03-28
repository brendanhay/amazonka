{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Direct Connect gateway, which is an intermediate object that enables you to connect a set of virtual interfaces and virtual private gateways. A Direct Connect gateway is global and visible in any AWS Region after it is created. The virtual interfaces and virtual private gateways that are connected through a Direct Connect gateway can be in different AWS Regions. This enables you to connect to a VPC in any Region, regardless of the Region in which the virtual interfaces are located, and pass traffic between them.
module Network.AWS.DirectConnect.CreateDirectConnectGateway
    (
    -- * Creating a request
      CreateDirectConnectGateway (..)
    , mkCreateDirectConnectGateway
    -- ** Request lenses
    , cdcgDirectConnectGatewayName
    , cdcgAmazonSideAsn

    -- * Destructuring the response
    , CreateDirectConnectGatewayResponse (..)
    , mkCreateDirectConnectGatewayResponse
    -- ** Response lenses
    , cdcgrrsDirectConnectGateway
    , cdcgrrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDirectConnectGateway' smart constructor.
data CreateDirectConnectGateway = CreateDirectConnectGateway'
  { directConnectGatewayName :: Types.DirectConnectGatewayName
    -- ^ The name of the Direct Connect gateway.
  , amazonSideAsn :: Core.Maybe Core.Integer
    -- ^ The autonomous system number (ASN) for Border Gateway Protocol (BGP) to be configured on the Amazon side of the connection. The ASN must be in the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294. The default is 64512.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectConnectGateway' value with any optional fields omitted.
mkCreateDirectConnectGateway
    :: Types.DirectConnectGatewayName -- ^ 'directConnectGatewayName'
    -> CreateDirectConnectGateway
mkCreateDirectConnectGateway directConnectGatewayName
  = CreateDirectConnectGateway'{directConnectGatewayName,
                                amazonSideAsn = Core.Nothing}

-- | The name of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgDirectConnectGatewayName :: Lens.Lens' CreateDirectConnectGateway Types.DirectConnectGatewayName
cdcgDirectConnectGatewayName = Lens.field @"directConnectGatewayName"
{-# INLINEABLE cdcgDirectConnectGatewayName #-}
{-# DEPRECATED directConnectGatewayName "Use generic-lens or generic-optics with 'directConnectGatewayName' instead"  #-}

-- | The autonomous system number (ASN) for Border Gateway Protocol (BGP) to be configured on the Amazon side of the connection. The ASN must be in the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294. The default is 64512.
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgAmazonSideAsn :: Lens.Lens' CreateDirectConnectGateway (Core.Maybe Core.Integer)
cdcgAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# INLINEABLE cdcgAmazonSideAsn #-}
{-# DEPRECATED amazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead"  #-}

instance Core.ToQuery CreateDirectConnectGateway where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDirectConnectGateway where
        toHeaders CreateDirectConnectGateway{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.CreateDirectConnectGateway")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDirectConnectGateway where
        toJSON CreateDirectConnectGateway{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("directConnectGatewayName" Core..= directConnectGatewayName),
                  ("amazonSideAsn" Core..=) Core.<$> amazonSideAsn])

instance Core.AWSRequest CreateDirectConnectGateway where
        type Rs CreateDirectConnectGateway =
             CreateDirectConnectGatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDirectConnectGatewayResponse' Core.<$>
                   (x Core..:? "directConnectGateway") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDirectConnectGatewayResponse' smart constructor.
data CreateDirectConnectGatewayResponse = CreateDirectConnectGatewayResponse'
  { directConnectGateway :: Core.Maybe Types.DirectConnectGateway
    -- ^ The Direct Connect gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectConnectGatewayResponse' value with any optional fields omitted.
mkCreateDirectConnectGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDirectConnectGatewayResponse
mkCreateDirectConnectGatewayResponse responseStatus
  = CreateDirectConnectGatewayResponse'{directConnectGateway =
                                          Core.Nothing,
                                        responseStatus}

-- | The Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgrrsDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayResponse (Core.Maybe Types.DirectConnectGateway)
cdcgrrsDirectConnectGateway = Lens.field @"directConnectGateway"
{-# INLINEABLE cdcgrrsDirectConnectGateway #-}
{-# DEPRECATED directConnectGateway "Use generic-lens or generic-optics with 'directConnectGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgrrsResponseStatus :: Lens.Lens' CreateDirectConnectGatewayResponse Core.Int
cdcgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdcgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
