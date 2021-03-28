{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an internet gateway for use with a VPC. After creating the internet gateway, you attach it to a VPC using 'AttachInternetGateway' .
--
-- For more information about your VPC and internet gateway, see the <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide> .
module Network.AWS.EC2.CreateInternetGateway
    (
    -- * Creating a request
      CreateInternetGateway (..)
    , mkCreateInternetGateway
    -- ** Request lenses
    , cigDryRun
    , cigTagSpecifications

    -- * Destructuring the response
    , CreateInternetGatewayResponse (..)
    , mkCreateInternetGatewayResponse
    -- ** Response lenses
    , cigrrsInternetGateway
    , cigrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInternetGateway' smart constructor.
data CreateInternetGateway = CreateInternetGateway'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the internet gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInternetGateway' value with any optional fields omitted.
mkCreateInternetGateway
    :: CreateInternetGateway
mkCreateInternetGateway
  = CreateInternetGateway'{dryRun = Core.Nothing,
                           tagSpecifications = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigDryRun :: Lens.Lens' CreateInternetGateway (Core.Maybe Core.Bool)
cigDryRun = Lens.field @"dryRun"
{-# INLINEABLE cigDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to assign to the internet gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigTagSpecifications :: Lens.Lens' CreateInternetGateway (Core.Maybe [Types.TagSpecification])
cigTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cigTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateInternetGateway where
        toQuery CreateInternetGateway{..}
          = Core.toQueryPair "Action" ("CreateInternetGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateInternetGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateInternetGateway where
        type Rs CreateInternetGateway = CreateInternetGatewayResponse
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
                 CreateInternetGatewayResponse' Core.<$>
                   (x Core..@? "internetGateway") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateInternetGatewayResponse' smart constructor.
data CreateInternetGatewayResponse = CreateInternetGatewayResponse'
  { internetGateway :: Core.Maybe Types.InternetGateway
    -- ^ Information about the internet gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInternetGatewayResponse' value with any optional fields omitted.
mkCreateInternetGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateInternetGatewayResponse
mkCreateInternetGatewayResponse responseStatus
  = CreateInternetGatewayResponse'{internetGateway = Core.Nothing,
                                   responseStatus}

-- | Information about the internet gateway.
--
-- /Note:/ Consider using 'internetGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrrsInternetGateway :: Lens.Lens' CreateInternetGatewayResponse (Core.Maybe Types.InternetGateway)
cigrrsInternetGateway = Lens.field @"internetGateway"
{-# INLINEABLE cigrrsInternetGateway #-}
{-# DEPRECATED internetGateway "Use generic-lens or generic-optics with 'internetGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrrsResponseStatus :: Lens.Lens' CreateInternetGatewayResponse Core.Int
cigrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cigrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
