{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ProvisionByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions an IPv4 or IPv6 address range for use with your AWS resources through bring your own IP addresses (BYOIP) and creates a corresponding address pool. After the address range is provisioned, it is ready to be advertised using 'AdvertiseByoipCidr' .
--
-- AWS verifies that you own the address range and are authorized to advertise it. You must ensure that the address range is registered to you and that you created an RPKI ROA to authorize Amazon ASNs 16509 and 14618 to advertise the address range. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html Bring Your Own IP Addresses (BYOIP)> in the /Amazon Elastic Compute Cloud User Guide/ .
-- Provisioning an address range is an asynchronous operation, so the call returns immediately, but the address range is not ready to use until its status changes from @pending-provision@ to @provisioned@ . To monitor the status of an address range, use 'DescribeByoipCidrs' . To allocate an Elastic IP address from your IPv4 address pool, use 'AllocateAddress' with either the specific address from the address pool or the ID of the address pool.
module Network.AWS.EC2.ProvisionByoipCidr
  ( -- * Creating a request
    ProvisionByoipCidr (..),
    mkProvisionByoipCidr,

    -- ** Request lenses
    pbcCidr,
    pbcCidrAuthorizationContext,
    pbcDescription,
    pbcDryRun,
    pbcPoolTagSpecifications,
    pbcPubliclyAdvertisable,

    -- * Destructuring the response
    ProvisionByoipCidrResponse (..),
    mkProvisionByoipCidrResponse,

    -- ** Response lenses
    pbcrrsByoipCidr,
    pbcrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkProvisionByoipCidr' smart constructor.
data ProvisionByoipCidr = ProvisionByoipCidr'
  { -- | The public IPv4 or IPv6 address range, in CIDR notation. The most specific IPv4 prefix that you can specify is /24. The most specific IPv6 prefix you can specify is /56. The address range cannot overlap with another address range that you've brought to this or another Region.
    cidr :: Types.String,
    -- | A signed document that proves that you are authorized to bring the specified IP address range to Amazon using BYOIP.
    cidrAuthorizationContext :: Core.Maybe Types.CidrAuthorizationContext,
    -- | A description for the address range and the address pool.
    description :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to apply to the address pool.
    poolTagSpecifications :: Core.Maybe [Types.TagSpecification],
    -- | (IPv6 only) Indicate whether the address range will be publicly advertised to the internet.
    --
    -- Default: true
    publiclyAdvertisable :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionByoipCidr' value with any optional fields omitted.
mkProvisionByoipCidr ::
  -- | 'cidr'
  Types.String ->
  ProvisionByoipCidr
mkProvisionByoipCidr cidr =
  ProvisionByoipCidr'
    { cidr,
      cidrAuthorizationContext = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      poolTagSpecifications = Core.Nothing,
      publiclyAdvertisable = Core.Nothing
    }

-- | The public IPv4 or IPv6 address range, in CIDR notation. The most specific IPv4 prefix that you can specify is /24. The most specific IPv6 prefix you can specify is /56. The address range cannot overlap with another address range that you've brought to this or another Region.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcCidr :: Lens.Lens' ProvisionByoipCidr Types.String
pbcCidr = Lens.field @"cidr"
{-# DEPRECATED pbcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | A signed document that proves that you are authorized to bring the specified IP address range to Amazon using BYOIP.
--
-- /Note:/ Consider using 'cidrAuthorizationContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcCidrAuthorizationContext :: Lens.Lens' ProvisionByoipCidr (Core.Maybe Types.CidrAuthorizationContext)
pbcCidrAuthorizationContext = Lens.field @"cidrAuthorizationContext"
{-# DEPRECATED pbcCidrAuthorizationContext "Use generic-lens or generic-optics with 'cidrAuthorizationContext' instead." #-}

-- | A description for the address range and the address pool.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcDescription :: Lens.Lens' ProvisionByoipCidr (Core.Maybe Types.String)
pbcDescription = Lens.field @"description"
{-# DEPRECATED pbcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcDryRun :: Lens.Lens' ProvisionByoipCidr (Core.Maybe Core.Bool)
pbcDryRun = Lens.field @"dryRun"
{-# DEPRECATED pbcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to apply to the address pool.
--
-- /Note:/ Consider using 'poolTagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcPoolTagSpecifications :: Lens.Lens' ProvisionByoipCidr (Core.Maybe [Types.TagSpecification])
pbcPoolTagSpecifications = Lens.field @"poolTagSpecifications"
{-# DEPRECATED pbcPoolTagSpecifications "Use generic-lens or generic-optics with 'poolTagSpecifications' instead." #-}

-- | (IPv6 only) Indicate whether the address range will be publicly advertised to the internet.
--
-- Default: true
--
-- /Note:/ Consider using 'publiclyAdvertisable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcPubliclyAdvertisable :: Lens.Lens' ProvisionByoipCidr (Core.Maybe Core.Bool)
pbcPubliclyAdvertisable = Lens.field @"publiclyAdvertisable"
{-# DEPRECATED pbcPubliclyAdvertisable "Use generic-lens or generic-optics with 'publiclyAdvertisable' instead." #-}

instance Core.AWSRequest ProvisionByoipCidr where
  type Rs ProvisionByoipCidr = ProvisionByoipCidrResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ProvisionByoipCidr")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Cidr" cidr)
                Core.<> ( Core.toQueryValue "CidrAuthorizationContext"
                            Core.<$> cidrAuthorizationContext
                        )
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryList "PoolTagSpecification"
                            Core.<$> poolTagSpecifications
                        )
                Core.<> ( Core.toQueryValue "PubliclyAdvertisable"
                            Core.<$> publiclyAdvertisable
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ProvisionByoipCidrResponse'
            Core.<$> (x Core..@? "byoipCidr") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkProvisionByoipCidrResponse' smart constructor.
data ProvisionByoipCidrResponse = ProvisionByoipCidrResponse'
  { -- | Information about the address range.
    byoipCidr :: Core.Maybe Types.ByoipCidr,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionByoipCidrResponse' value with any optional fields omitted.
mkProvisionByoipCidrResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ProvisionByoipCidrResponse
mkProvisionByoipCidrResponse responseStatus =
  ProvisionByoipCidrResponse'
    { byoipCidr = Core.Nothing,
      responseStatus
    }

-- | Information about the address range.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcrrsByoipCidr :: Lens.Lens' ProvisionByoipCidrResponse (Core.Maybe Types.ByoipCidr)
pbcrrsByoipCidr = Lens.field @"byoipCidr"
{-# DEPRECATED pbcrrsByoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcrrsResponseStatus :: Lens.Lens' ProvisionByoipCidrResponse Core.Int
pbcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
