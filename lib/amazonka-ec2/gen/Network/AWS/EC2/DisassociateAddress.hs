{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from the instance or network interface it's associated with.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or in a VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
-- This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error.
module Network.AWS.EC2.DisassociateAddress
  ( -- * Creating a request
    DisassociateAddress (..),
    mkDisassociateAddress,

    -- ** Request lenses
    dasAssociationId,
    dasPublicIP,
    dasDryRun,

    -- * Destructuring the response
    DisassociateAddressResponse (..),
    mkDisassociateAddressResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateAddress' smart constructor.
data DisassociateAddress = DisassociateAddress'
  { -- | [EC2-VPC] The association ID. Required for EC2-VPC.
    associationId :: Lude.Maybe Lude.Text,
    -- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
    publicIP :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateAddress' with the minimum fields required to make a request.
--
-- * 'associationId' - [EC2-VPC] The association ID. Required for EC2-VPC.
-- * 'publicIP' - [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDisassociateAddress ::
  DisassociateAddress
mkDisassociateAddress =
  DisassociateAddress'
    { associationId = Lude.Nothing,
      publicIP = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | [EC2-VPC] The association ID. Required for EC2-VPC.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasAssociationId :: Lens.Lens' DisassociateAddress (Lude.Maybe Lude.Text)
dasAssociationId = Lens.lens (associationId :: DisassociateAddress -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: DisassociateAddress)
{-# DEPRECATED dasAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasPublicIP :: Lens.Lens' DisassociateAddress (Lude.Maybe Lude.Text)
dasPublicIP = Lens.lens (publicIP :: DisassociateAddress -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: DisassociateAddress)
{-# DEPRECATED dasPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasDryRun :: Lens.Lens' DisassociateAddress (Lude.Maybe Lude.Bool)
dasDryRun = Lens.lens (dryRun :: DisassociateAddress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisassociateAddress)
{-# DEPRECATED dasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DisassociateAddress where
  type Rs DisassociateAddress = DisassociateAddressResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DisassociateAddressResponse'

instance Lude.ToHeaders DisassociateAddress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateAddress where
  toQuery DisassociateAddress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisassociateAddress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AssociationId" Lude.=: associationId,
        "PublicIp" Lude.=: publicIP,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDisassociateAddressResponse' smart constructor.
data DisassociateAddressResponse = DisassociateAddressResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateAddressResponse' with the minimum fields required to make a request.
mkDisassociateAddressResponse ::
  DisassociateAddressResponse
mkDisassociateAddressResponse = DisassociateAddressResponse'
