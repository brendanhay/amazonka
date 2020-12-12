{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkInterface
  ( NetworkInterface (..),

    -- * Smart constructor
    mkNetworkInterface,

    -- * Lenses
    niIpv6Address,
    niPrivateIPv4Address,
    niAttachmentId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the elastic network interface for tasks that use the @awsvpc@ network mode.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { ipv6Address ::
      Lude.Maybe Lude.Text,
    privateIPv4Address :: Lude.Maybe Lude.Text,
    attachmentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- * 'attachmentId' - The attachment ID for the network interface.
-- * 'ipv6Address' - The private IPv6 address for the network interface.
-- * 'privateIPv4Address' - The private IPv4 address for the network interface.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { ipv6Address = Lude.Nothing,
      privateIPv4Address = Lude.Nothing,
      attachmentId = Lude.Nothing
    }

-- | The private IPv6 address for the network interface.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Address :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niIpv6Address = Lens.lens (ipv6Address :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: NetworkInterface)
{-# DEPRECATED niIpv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

-- | The private IPv4 address for the network interface.
--
-- /Note:/ Consider using 'privateIPv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIPv4Address :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPrivateIPv4Address = Lens.lens (privateIPv4Address :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateIPv4Address = a} :: NetworkInterface)
{-# DEPRECATED niPrivateIPv4Address "Use generic-lens or generic-optics with 'privateIPv4Address' instead." #-}

-- | The attachment ID for the network interface.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niAttachmentId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niAttachmentId = Lens.lens (attachmentId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {attachmentId = a} :: NetworkInterface)
{-# DEPRECATED niAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

instance Lude.FromJSON NetworkInterface where
  parseJSON =
    Lude.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Lude.<$> (x Lude..:? "ipv6Address")
            Lude.<*> (x Lude..:? "privateIpv4Address")
            Lude.<*> (x Lude..:? "attachmentId")
      )
