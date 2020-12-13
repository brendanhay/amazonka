{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDefaultSubnet (..),
    mkCreateDefaultSubnet,

    -- ** Request lenses
    cdsAvailabilityZone,
    cdsDryRun,

    -- * Destructuring the response
    CreateDefaultSubnetResponse (..),
    mkCreateDefaultSubnetResponse,

    -- ** Response lenses
    cdsrsSubnet,
    cdsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDefaultSubnet' smart constructor.
data CreateDefaultSubnet = CreateDefaultSubnet'
  { -- | The Availability Zone in which to create the default subnet.
    availabilityZone :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDefaultSubnet' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone in which to create the default subnet.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateDefaultSubnet ::
  -- | 'availabilityZone'
  Lude.Text ->
  CreateDefaultSubnet
mkCreateDefaultSubnet pAvailabilityZone_ =
  CreateDefaultSubnet'
    { availabilityZone = pAvailabilityZone_,
      dryRun = Lude.Nothing
    }

-- | The Availability Zone in which to create the default subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsAvailabilityZone :: Lens.Lens' CreateDefaultSubnet Lude.Text
cdsAvailabilityZone = Lens.lens (availabilityZone :: CreateDefaultSubnet -> Lude.Text) (\s a -> s {availabilityZone = a} :: CreateDefaultSubnet)
{-# DEPRECATED cdsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDryRun :: Lens.Lens' CreateDefaultSubnet (Lude.Maybe Lude.Bool)
cdsDryRun = Lens.lens (dryRun :: CreateDefaultSubnet -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateDefaultSubnet)
{-# DEPRECATED cdsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateDefaultSubnet where
  type Rs CreateDefaultSubnet = CreateDefaultSubnetResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateDefaultSubnetResponse'
            Lude.<$> (x Lude..@? "subnet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDefaultSubnet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDefaultSubnet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDefaultSubnet where
  toQuery CreateDefaultSubnet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDefaultSubnet" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AvailabilityZone" Lude.=: availabilityZone,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateDefaultSubnetResponse' smart constructor.
data CreateDefaultSubnetResponse = CreateDefaultSubnetResponse'
  { -- | Information about the subnet.
    subnet :: Lude.Maybe Subnet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDefaultSubnetResponse' with the minimum fields required to make a request.
--
-- * 'subnet' - Information about the subnet.
-- * 'responseStatus' - The response status code.
mkCreateDefaultSubnetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDefaultSubnetResponse
mkCreateDefaultSubnetResponse pResponseStatus_ =
  CreateDefaultSubnetResponse'
    { subnet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the subnet.
--
-- /Note:/ Consider using 'subnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsSubnet :: Lens.Lens' CreateDefaultSubnetResponse (Lude.Maybe Subnet)
cdsrsSubnet = Lens.lens (subnet :: CreateDefaultSubnetResponse -> Lude.Maybe Subnet) (\s a -> s {subnet = a} :: CreateDefaultSubnetResponse)
{-# DEPRECATED cdsrsSubnet "Use generic-lens or generic-optics with 'subnet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsResponseStatus :: Lens.Lens' CreateDefaultSubnetResponse Lude.Int
cdsrsResponseStatus = Lens.lens (responseStatus :: CreateDefaultSubnetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDefaultSubnetResponse)
{-# DEPRECATED cdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
