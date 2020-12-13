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
    pbcCidrAuthorizationContext,
    pbcPoolTagSpecifications,
    pbcPubliclyAdvertisable,
    pbcCidr,
    pbcDescription,
    pbcDryRun,

    -- * Destructuring the response
    ProvisionByoipCidrResponse (..),
    mkProvisionByoipCidrResponse,

    -- ** Response lenses
    pbcrsByoipCidr,
    pbcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkProvisionByoipCidr' smart constructor.
data ProvisionByoipCidr = ProvisionByoipCidr'
  { -- | A signed document that proves that you are authorized to bring the specified IP address range to Amazon using BYOIP.
    cidrAuthorizationContext :: Lude.Maybe CidrAuthorizationContext,
    -- | The tags to apply to the address pool.
    poolTagSpecifications :: Lude.Maybe [TagSpecification],
    -- | (IPv6 only) Indicate whether the address range will be publicly advertised to the internet.
    --
    -- Default: true
    publiclyAdvertisable :: Lude.Maybe Lude.Bool,
    -- | The public IPv4 or IPv6 address range, in CIDR notation. The most specific IPv4 prefix that you can specify is /24. The most specific IPv6 prefix you can specify is /56. The address range cannot overlap with another address range that you've brought to this or another Region.
    cidr :: Lude.Text,
    -- | A description for the address range and the address pool.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionByoipCidr' with the minimum fields required to make a request.
--
-- * 'cidrAuthorizationContext' - A signed document that proves that you are authorized to bring the specified IP address range to Amazon using BYOIP.
-- * 'poolTagSpecifications' - The tags to apply to the address pool.
-- * 'publiclyAdvertisable' - (IPv6 only) Indicate whether the address range will be publicly advertised to the internet.
--
-- Default: true
-- * 'cidr' - The public IPv4 or IPv6 address range, in CIDR notation. The most specific IPv4 prefix that you can specify is /24. The most specific IPv6 prefix you can specify is /56. The address range cannot overlap with another address range that you've brought to this or another Region.
-- * 'description' - A description for the address range and the address pool.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkProvisionByoipCidr ::
  -- | 'cidr'
  Lude.Text ->
  ProvisionByoipCidr
mkProvisionByoipCidr pCidr_ =
  ProvisionByoipCidr'
    { cidrAuthorizationContext = Lude.Nothing,
      poolTagSpecifications = Lude.Nothing,
      publiclyAdvertisable = Lude.Nothing,
      cidr = pCidr_,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | A signed document that proves that you are authorized to bring the specified IP address range to Amazon using BYOIP.
--
-- /Note:/ Consider using 'cidrAuthorizationContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcCidrAuthorizationContext :: Lens.Lens' ProvisionByoipCidr (Lude.Maybe CidrAuthorizationContext)
pbcCidrAuthorizationContext = Lens.lens (cidrAuthorizationContext :: ProvisionByoipCidr -> Lude.Maybe CidrAuthorizationContext) (\s a -> s {cidrAuthorizationContext = a} :: ProvisionByoipCidr)
{-# DEPRECATED pbcCidrAuthorizationContext "Use generic-lens or generic-optics with 'cidrAuthorizationContext' instead." #-}

-- | The tags to apply to the address pool.
--
-- /Note:/ Consider using 'poolTagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcPoolTagSpecifications :: Lens.Lens' ProvisionByoipCidr (Lude.Maybe [TagSpecification])
pbcPoolTagSpecifications = Lens.lens (poolTagSpecifications :: ProvisionByoipCidr -> Lude.Maybe [TagSpecification]) (\s a -> s {poolTagSpecifications = a} :: ProvisionByoipCidr)
{-# DEPRECATED pbcPoolTagSpecifications "Use generic-lens or generic-optics with 'poolTagSpecifications' instead." #-}

-- | (IPv6 only) Indicate whether the address range will be publicly advertised to the internet.
--
-- Default: true
--
-- /Note:/ Consider using 'publiclyAdvertisable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcPubliclyAdvertisable :: Lens.Lens' ProvisionByoipCidr (Lude.Maybe Lude.Bool)
pbcPubliclyAdvertisable = Lens.lens (publiclyAdvertisable :: ProvisionByoipCidr -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAdvertisable = a} :: ProvisionByoipCidr)
{-# DEPRECATED pbcPubliclyAdvertisable "Use generic-lens or generic-optics with 'publiclyAdvertisable' instead." #-}

-- | The public IPv4 or IPv6 address range, in CIDR notation. The most specific IPv4 prefix that you can specify is /24. The most specific IPv6 prefix you can specify is /56. The address range cannot overlap with another address range that you've brought to this or another Region.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcCidr :: Lens.Lens' ProvisionByoipCidr Lude.Text
pbcCidr = Lens.lens (cidr :: ProvisionByoipCidr -> Lude.Text) (\s a -> s {cidr = a} :: ProvisionByoipCidr)
{-# DEPRECATED pbcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | A description for the address range and the address pool.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcDescription :: Lens.Lens' ProvisionByoipCidr (Lude.Maybe Lude.Text)
pbcDescription = Lens.lens (description :: ProvisionByoipCidr -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisionByoipCidr)
{-# DEPRECATED pbcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcDryRun :: Lens.Lens' ProvisionByoipCidr (Lude.Maybe Lude.Bool)
pbcDryRun = Lens.lens (dryRun :: ProvisionByoipCidr -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ProvisionByoipCidr)
{-# DEPRECATED pbcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ProvisionByoipCidr where
  type Rs ProvisionByoipCidr = ProvisionByoipCidrResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ProvisionByoipCidrResponse'
            Lude.<$> (x Lude..@? "byoipCidr") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ProvisionByoipCidr where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ProvisionByoipCidr where
  toPath = Lude.const "/"

instance Lude.ToQuery ProvisionByoipCidr where
  toQuery ProvisionByoipCidr' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ProvisionByoipCidr" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CidrAuthorizationContext" Lude.=: cidrAuthorizationContext,
        Lude.toQuery
          ( Lude.toQueryList "PoolTagSpecification"
              Lude.<$> poolTagSpecifications
          ),
        "PubliclyAdvertisable" Lude.=: publiclyAdvertisable,
        "Cidr" Lude.=: cidr,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkProvisionByoipCidrResponse' smart constructor.
data ProvisionByoipCidrResponse = ProvisionByoipCidrResponse'
  { -- | Information about the address range.
    byoipCidr :: Lude.Maybe ByoipCidr,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionByoipCidrResponse' with the minimum fields required to make a request.
--
-- * 'byoipCidr' - Information about the address range.
-- * 'responseStatus' - The response status code.
mkProvisionByoipCidrResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ProvisionByoipCidrResponse
mkProvisionByoipCidrResponse pResponseStatus_ =
  ProvisionByoipCidrResponse'
    { byoipCidr = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the address range.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcrsByoipCidr :: Lens.Lens' ProvisionByoipCidrResponse (Lude.Maybe ByoipCidr)
pbcrsByoipCidr = Lens.lens (byoipCidr :: ProvisionByoipCidrResponse -> Lude.Maybe ByoipCidr) (\s a -> s {byoipCidr = a} :: ProvisionByoipCidrResponse)
{-# DEPRECATED pbcrsByoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbcrsResponseStatus :: Lens.Lens' ProvisionByoipCidrResponse Lude.Int
pbcrsResponseStatus = Lens.lens (responseStatus :: ProvisionByoipCidrResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ProvisionByoipCidrResponse)
{-# DEPRECATED pbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
