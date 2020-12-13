{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.MoveAddressToVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves an Elastic IP address from the EC2-Classic platform to the EC2-VPC platform. The Elastic IP address must be allocated to your account for more than 24 hours, and it must not be associated with an instance. After the Elastic IP address is moved, it is no longer available for use in the EC2-Classic platform, unless you move it back using the 'RestoreAddressToClassic' request. You cannot move an Elastic IP address that was originally allocated for use in the EC2-VPC platform to the EC2-Classic platform.
module Network.AWS.EC2.MoveAddressToVPC
  ( -- * Creating a request
    MoveAddressToVPC (..),
    mkMoveAddressToVPC,

    -- ** Request lenses
    matvPublicIP,
    matvDryRun,

    -- * Destructuring the response
    MoveAddressToVPCResponse (..),
    mkMoveAddressToVPCResponse,

    -- ** Response lenses
    matvrsStatus,
    matvrsAllocationId,
    matvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMoveAddressToVPC' smart constructor.
data MoveAddressToVPC = MoveAddressToVPC'
  { -- | The Elastic IP address.
    publicIP :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MoveAddressToVPC' with the minimum fields required to make a request.
--
-- * 'publicIP' - The Elastic IP address.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkMoveAddressToVPC ::
  -- | 'publicIP'
  Lude.Text ->
  MoveAddressToVPC
mkMoveAddressToVPC pPublicIP_ =
  MoveAddressToVPC' {publicIP = pPublicIP_, dryRun = Lude.Nothing}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvPublicIP :: Lens.Lens' MoveAddressToVPC Lude.Text
matvPublicIP = Lens.lens (publicIP :: MoveAddressToVPC -> Lude.Text) (\s a -> s {publicIP = a} :: MoveAddressToVPC)
{-# DEPRECATED matvPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvDryRun :: Lens.Lens' MoveAddressToVPC (Lude.Maybe Lude.Bool)
matvDryRun = Lens.lens (dryRun :: MoveAddressToVPC -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: MoveAddressToVPC)
{-# DEPRECATED matvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest MoveAddressToVPC where
  type Rs MoveAddressToVPC = MoveAddressToVPCResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          MoveAddressToVPCResponse'
            Lude.<$> (x Lude..@? "status")
            Lude.<*> (x Lude..@? "allocationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MoveAddressToVPC where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath MoveAddressToVPC where
  toPath = Lude.const "/"

instance Lude.ToQuery MoveAddressToVPC where
  toQuery MoveAddressToVPC' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("MoveAddressToVpc" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "PublicIp" Lude.=: publicIP,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkMoveAddressToVPCResponse' smart constructor.
data MoveAddressToVPCResponse = MoveAddressToVPCResponse'
  { -- | The status of the move of the IP address.
    status :: Lude.Maybe AddressStatus,
    -- | The allocation ID for the Elastic IP address.
    allocationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MoveAddressToVPCResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the move of the IP address.
-- * 'allocationId' - The allocation ID for the Elastic IP address.
-- * 'responseStatus' - The response status code.
mkMoveAddressToVPCResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MoveAddressToVPCResponse
mkMoveAddressToVPCResponse pResponseStatus_ =
  MoveAddressToVPCResponse'
    { status = Lude.Nothing,
      allocationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the move of the IP address.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvrsStatus :: Lens.Lens' MoveAddressToVPCResponse (Lude.Maybe AddressStatus)
matvrsStatus = Lens.lens (status :: MoveAddressToVPCResponse -> Lude.Maybe AddressStatus) (\s a -> s {status = a} :: MoveAddressToVPCResponse)
{-# DEPRECATED matvrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The allocation ID for the Elastic IP address.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvrsAllocationId :: Lens.Lens' MoveAddressToVPCResponse (Lude.Maybe Lude.Text)
matvrsAllocationId = Lens.lens (allocationId :: MoveAddressToVPCResponse -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: MoveAddressToVPCResponse)
{-# DEPRECATED matvrsAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvrsResponseStatus :: Lens.Lens' MoveAddressToVPCResponse Lude.Int
matvrsResponseStatus = Lens.lens (responseStatus :: MoveAddressToVPCResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MoveAddressToVPCResponse)
{-# DEPRECATED matvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
