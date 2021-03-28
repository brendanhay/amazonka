{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateDhcpOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of DHCP options (that you've previously created) with the specified VPC, or associates no DHCP options with the VPC.
--
-- After you associate the options with the VPC, any existing instances and all new instances that you launch in that VPC use the options. You don't need to restart or relaunch the instances. They automatically pick up the changes within a few hours, depending on how frequently the instance renews its DHCP lease. You can explicitly renew the lease using the operating system on the instance.
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.AssociateDhcpOptions
    (
    -- * Creating a request
      AssociateDhcpOptions (..)
    , mkAssociateDhcpOptions
    -- ** Request lenses
    , adoDhcpOptionsId
    , adoVpcId
    , adoDryRun

    -- * Destructuring the response
    , AssociateDhcpOptionsResponse (..)
    , mkAssociateDhcpOptionsResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateDhcpOptions' smart constructor.
data AssociateDhcpOptions = AssociateDhcpOptions'
  { dhcpOptionsId :: Types.DhcpOptionsId
    -- ^ The ID of the DHCP options set, or @default@ to associate no DHCP options with the VPC.
  , vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDhcpOptions' value with any optional fields omitted.
mkAssociateDhcpOptions
    :: Types.DhcpOptionsId -- ^ 'dhcpOptionsId'
    -> Types.VpcId -- ^ 'vpcId'
    -> AssociateDhcpOptions
mkAssociateDhcpOptions dhcpOptionsId vpcId
  = AssociateDhcpOptions'{dhcpOptionsId, vpcId,
                          dryRun = Core.Nothing}

-- | The ID of the DHCP options set, or @default@ to associate no DHCP options with the VPC.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adoDhcpOptionsId :: Lens.Lens' AssociateDhcpOptions Types.DhcpOptionsId
adoDhcpOptionsId = Lens.field @"dhcpOptionsId"
{-# INLINEABLE adoDhcpOptionsId #-}
{-# DEPRECATED dhcpOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adoVpcId :: Lens.Lens' AssociateDhcpOptions Types.VpcId
adoVpcId = Lens.field @"vpcId"
{-# INLINEABLE adoVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adoDryRun :: Lens.Lens' AssociateDhcpOptions (Core.Maybe Core.Bool)
adoDryRun = Lens.field @"dryRun"
{-# INLINEABLE adoDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AssociateDhcpOptions where
        toQuery AssociateDhcpOptions{..}
          = Core.toQueryPair "Action" ("AssociateDhcpOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "DhcpOptionsId" dhcpOptionsId
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AssociateDhcpOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AssociateDhcpOptions where
        type Rs AssociateDhcpOptions = AssociateDhcpOptionsResponse
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
        parseResponse = Response.receiveNull AssociateDhcpOptionsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateDhcpOptionsResponse' smart constructor.
data AssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDhcpOptionsResponse' value with any optional fields omitted.
mkAssociateDhcpOptionsResponse
    :: AssociateDhcpOptionsResponse
mkAssociateDhcpOptionsResponse = AssociateDhcpOptionsResponse'
