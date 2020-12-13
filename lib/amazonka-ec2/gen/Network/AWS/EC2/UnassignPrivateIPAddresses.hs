{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UnassignPrivateIPAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more secondary private IP addresses from a network interface.
module Network.AWS.EC2.UnassignPrivateIPAddresses
  ( -- * Creating a request
    UnassignPrivateIPAddresses (..),
    mkUnassignPrivateIPAddresses,

    -- ** Request lenses
    upiaPrivateIPAddresses,
    upiaNetworkInterfaceId,

    -- * Destructuring the response
    UnassignPrivateIPAddressesResponse (..),
    mkUnassignPrivateIPAddressesResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for UnassignPrivateIpAddresses.
--
-- /See:/ 'mkUnassignPrivateIPAddresses' smart constructor.
data UnassignPrivateIPAddresses = UnassignPrivateIPAddresses'
  { -- | The secondary private IP addresses to unassign from the network interface. You can specify this option multiple times to unassign more than one IP address.
    privateIPAddresses :: [Lude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnassignPrivateIPAddresses' with the minimum fields required to make a request.
--
-- * 'privateIPAddresses' - The secondary private IP addresses to unassign from the network interface. You can specify this option multiple times to unassign more than one IP address.
-- * 'networkInterfaceId' - The ID of the network interface.
mkUnassignPrivateIPAddresses ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  UnassignPrivateIPAddresses
mkUnassignPrivateIPAddresses pNetworkInterfaceId_ =
  UnassignPrivateIPAddresses'
    { privateIPAddresses = Lude.mempty,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | The secondary private IP addresses to unassign from the network interface. You can specify this option multiple times to unassign more than one IP address.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upiaPrivateIPAddresses :: Lens.Lens' UnassignPrivateIPAddresses [Lude.Text]
upiaPrivateIPAddresses = Lens.lens (privateIPAddresses :: UnassignPrivateIPAddresses -> [Lude.Text]) (\s a -> s {privateIPAddresses = a} :: UnassignPrivateIPAddresses)
{-# DEPRECATED upiaPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upiaNetworkInterfaceId :: Lens.Lens' UnassignPrivateIPAddresses Lude.Text
upiaNetworkInterfaceId = Lens.lens (networkInterfaceId :: UnassignPrivateIPAddresses -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: UnassignPrivateIPAddresses)
{-# DEPRECATED upiaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

instance Lude.AWSRequest UnassignPrivateIPAddresses where
  type
    Rs UnassignPrivateIPAddresses =
      UnassignPrivateIPAddressesResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull UnassignPrivateIPAddressesResponse'

instance Lude.ToHeaders UnassignPrivateIPAddresses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UnassignPrivateIPAddresses where
  toPath = Lude.const "/"

instance Lude.ToQuery UnassignPrivateIPAddresses where
  toQuery UnassignPrivateIPAddresses' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UnassignPrivateIpAddresses" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "PrivateIpAddress" privateIPAddresses,
        "NetworkInterfaceId" Lude.=: networkInterfaceId
      ]

-- | /See:/ 'mkUnassignPrivateIPAddressesResponse' smart constructor.
data UnassignPrivateIPAddressesResponse = UnassignPrivateIPAddressesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnassignPrivateIPAddressesResponse' with the minimum fields required to make a request.
mkUnassignPrivateIPAddressesResponse ::
  UnassignPrivateIPAddressesResponse
mkUnassignPrivateIPAddressesResponse =
  UnassignPrivateIPAddressesResponse'
