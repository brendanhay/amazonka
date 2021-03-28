{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateDefaultSubnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a default subnet with a size @/20@ IPv4 CIDR block in the specified Availability Zone in your default VPC. You can have only one default subnet per Availability Zone. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html#create-default-subnet Creating a Default Subnet> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateDefaultSubnet
    (
    -- * Creating a request
      CreateDefaultSubnet (..)
    , mkCreateDefaultSubnet
    -- ** Request lenses
    , cdsAvailabilityZone
    , cdsDryRun

    -- * Destructuring the response
    , CreateDefaultSubnetResponse (..)
    , mkCreateDefaultSubnetResponse
    -- ** Response lenses
    , cdsrrsSubnet
    , cdsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDefaultSubnet' smart constructor.
data CreateDefaultSubnet = CreateDefaultSubnet'
  { availabilityZone :: Core.Text
    -- ^ The Availability Zone in which to create the default subnet.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDefaultSubnet' value with any optional fields omitted.
mkCreateDefaultSubnet
    :: Core.Text -- ^ 'availabilityZone'
    -> CreateDefaultSubnet
mkCreateDefaultSubnet availabilityZone
  = CreateDefaultSubnet'{availabilityZone, dryRun = Core.Nothing}

-- | The Availability Zone in which to create the default subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsAvailabilityZone :: Lens.Lens' CreateDefaultSubnet Core.Text
cdsAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE cdsAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDryRun :: Lens.Lens' CreateDefaultSubnet (Core.Maybe Core.Bool)
cdsDryRun = Lens.field @"dryRun"
{-# INLINEABLE cdsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery CreateDefaultSubnet where
        toQuery CreateDefaultSubnet{..}
          = Core.toQueryPair "Action" ("CreateDefaultSubnet" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AvailabilityZone" availabilityZone
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders CreateDefaultSubnet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDefaultSubnet where
        type Rs CreateDefaultSubnet = CreateDefaultSubnetResponse
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
                 CreateDefaultSubnetResponse' Core.<$>
                   (x Core..@? "subnet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDefaultSubnetResponse' smart constructor.
data CreateDefaultSubnetResponse = CreateDefaultSubnetResponse'
  { subnet :: Core.Maybe Types.Subnet
    -- ^ Information about the subnet.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDefaultSubnetResponse' value with any optional fields omitted.
mkCreateDefaultSubnetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDefaultSubnetResponse
mkCreateDefaultSubnetResponse responseStatus
  = CreateDefaultSubnetResponse'{subnet = Core.Nothing,
                                 responseStatus}

-- | Information about the subnet.
--
-- /Note:/ Consider using 'subnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsSubnet :: Lens.Lens' CreateDefaultSubnetResponse (Core.Maybe Types.Subnet)
cdsrrsSubnet = Lens.field @"subnet"
{-# INLINEABLE cdsrrsSubnet #-}
{-# DEPRECATED subnet "Use generic-lens or generic-optics with 'subnet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsResponseStatus :: Lens.Lens' CreateDefaultSubnetResponse Core.Int
cdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
