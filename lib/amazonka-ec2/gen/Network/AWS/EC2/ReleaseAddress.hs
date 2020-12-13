{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ReleaseAddress (..),
    mkReleaseAddress,

    -- ** Request lenses
    raAllocationId,
    raNetworkBorderGroup,
    raPublicIP,
    raDryRun,

    -- * Destructuring the response
    ReleaseAddressResponse (..),
    mkReleaseAddressResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReleaseAddress' smart constructor.
data ReleaseAddress = ReleaseAddress'
  { -- | [EC2-VPC] The allocation ID. Required for EC2-VPC.
    allocationId :: Lude.Maybe Lude.Text,
    -- | The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
    --
    -- If you provide an incorrect network border group, you will receive an @InvalidAddress.NotFound@ error. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes> .
    networkBorderGroup :: Lude.Maybe Lude.Text,
    -- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
    publicIP :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReleaseAddress' with the minimum fields required to make a request.
--
-- * 'allocationId' - [EC2-VPC] The allocation ID. Required for EC2-VPC.
-- * 'networkBorderGroup' - The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- If you provide an incorrect network border group, you will receive an @InvalidAddress.NotFound@ error. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes> .
-- * 'publicIP' - [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkReleaseAddress ::
  ReleaseAddress
mkReleaseAddress =
  ReleaseAddress'
    { allocationId = Lude.Nothing,
      networkBorderGroup = Lude.Nothing,
      publicIP = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | [EC2-VPC] The allocation ID. Required for EC2-VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAllocationId :: Lens.Lens' ReleaseAddress (Lude.Maybe Lude.Text)
raAllocationId = Lens.lens (allocationId :: ReleaseAddress -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: ReleaseAddress)
{-# DEPRECATED raAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- If you provide an incorrect network border group, you will receive an @InvalidAddress.NotFound@ error. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes> .
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raNetworkBorderGroup :: Lens.Lens' ReleaseAddress (Lude.Maybe Lude.Text)
raNetworkBorderGroup = Lens.lens (networkBorderGroup :: ReleaseAddress -> Lude.Maybe Lude.Text) (\s a -> s {networkBorderGroup = a} :: ReleaseAddress)
{-# DEPRECATED raNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raPublicIP :: Lens.Lens' ReleaseAddress (Lude.Maybe Lude.Text)
raPublicIP = Lens.lens (publicIP :: ReleaseAddress -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: ReleaseAddress)
{-# DEPRECATED raPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raDryRun :: Lens.Lens' ReleaseAddress (Lude.Maybe Lude.Bool)
raDryRun = Lens.lens (dryRun :: ReleaseAddress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ReleaseAddress)
{-# DEPRECATED raDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ReleaseAddress where
  type Rs ReleaseAddress = ReleaseAddressResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ReleaseAddressResponse'

instance Lude.ToHeaders ReleaseAddress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReleaseAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery ReleaseAddress where
  toQuery ReleaseAddress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ReleaseAddress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AllocationId" Lude.=: allocationId,
        "NetworkBorderGroup" Lude.=: networkBorderGroup,
        "PublicIp" Lude.=: publicIP,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkReleaseAddressResponse' smart constructor.
data ReleaseAddressResponse = ReleaseAddressResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReleaseAddressResponse' with the minimum fields required to make a request.
mkReleaseAddressResponse ::
  ReleaseAddressResponse
mkReleaseAddressResponse = ReleaseAddressResponse'
