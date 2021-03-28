{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReleaseAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the specified Elastic IP address.
--
-- [EC2-Classic, default VPC] Releasing an Elastic IP address automatically disassociates it from any instance that it's associated with. To disassociate an Elastic IP address without releasing it, use 'DisassociateAddress' .
-- [Nondefault VPC] You must use 'DisassociateAddress' to disassociate the Elastic IP address before you can release it. Otherwise, Amazon EC2 returns an error (@InvalidIPAddress.InUse@ ).
-- After releasing an Elastic IP address, it is released to the IP address pool. Be sure to update your DNS records and any servers or devices that communicate with the address. If you attempt to release an Elastic IP address that you already released, you'll get an @AuthFailure@ error if the address is already allocated to another AWS account.
-- [EC2-VPC] After you release an Elastic IP address for use in a VPC, you might be able to recover it. For more information, see 'AllocateAddress' .
module Network.AWS.EC2.ReleaseAddress
    (
    -- * Creating a request
      ReleaseAddress (..)
    , mkReleaseAddress
    -- ** Request lenses
    , raAllocationId
    , raDryRun
    , raNetworkBorderGroup
    , raPublicIp

    -- * Destructuring the response
    , ReleaseAddressResponse (..)
    , mkReleaseAddressResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReleaseAddress' smart constructor.
data ReleaseAddress = ReleaseAddress'
  { allocationId :: Core.Maybe Types.AllocationId
    -- ^ [EC2-VPC] The allocation ID. Required for EC2-VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , networkBorderGroup :: Core.Maybe Core.Text
    -- ^ The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- If you provide an incorrect network border group, you will receive an @InvalidAddress.NotFound@ error. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes> .
  , publicIp :: Core.Maybe Core.Text
    -- ^ [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReleaseAddress' value with any optional fields omitted.
mkReleaseAddress
    :: ReleaseAddress
mkReleaseAddress
  = ReleaseAddress'{allocationId = Core.Nothing,
                    dryRun = Core.Nothing, networkBorderGroup = Core.Nothing,
                    publicIp = Core.Nothing}

-- | [EC2-VPC] The allocation ID. Required for EC2-VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAllocationId :: Lens.Lens' ReleaseAddress (Core.Maybe Types.AllocationId)
raAllocationId = Lens.field @"allocationId"
{-# INLINEABLE raAllocationId #-}
{-# DEPRECATED allocationId "Use generic-lens or generic-optics with 'allocationId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raDryRun :: Lens.Lens' ReleaseAddress (Core.Maybe Core.Bool)
raDryRun = Lens.field @"dryRun"
{-# INLINEABLE raDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- If you provide an incorrect network border group, you will receive an @InvalidAddress.NotFound@ error. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes> .
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raNetworkBorderGroup :: Lens.Lens' ReleaseAddress (Core.Maybe Core.Text)
raNetworkBorderGroup = Lens.field @"networkBorderGroup"
{-# INLINEABLE raNetworkBorderGroup #-}
{-# DEPRECATED networkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead"  #-}

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raPublicIp :: Lens.Lens' ReleaseAddress (Core.Maybe Core.Text)
raPublicIp = Lens.field @"publicIp"
{-# INLINEABLE raPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

instance Core.ToQuery ReleaseAddress where
        toQuery ReleaseAddress{..}
          = Core.toQueryPair "Action" ("ReleaseAddress" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AllocationId")
                allocationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NetworkBorderGroup")
                networkBorderGroup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PublicIp") publicIp

instance Core.ToHeaders ReleaseAddress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReleaseAddress where
        type Rs ReleaseAddress = ReleaseAddressResponse
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
        parseResponse = Response.receiveNull ReleaseAddressResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReleaseAddressResponse' smart constructor.
data ReleaseAddressResponse = ReleaseAddressResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReleaseAddressResponse' value with any optional fields omitted.
mkReleaseAddressResponse
    :: ReleaseAddressResponse
mkReleaseAddressResponse = ReleaseAddressResponse'
