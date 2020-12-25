{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NetworkInterface
  ( NetworkInterface (..),

    -- * Smart constructor
    mkNetworkInterface,

    -- * Lenses
    niAttachmentId,
    niIpv6Address,
    niPrivateIpv4Address,
  )
where

import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the elastic network interface for a multi-node parallel job node.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The attachment ID for the network interface.
    attachmentId :: Core.Maybe Types.String,
    -- | The private IPv6 address for the network interface.
    ipv6Address :: Core.Maybe Types.String,
    -- | The private IPv4 address for the network interface.
    privateIpv4Address :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterface' value with any optional fields omitted.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { attachmentId = Core.Nothing,
      ipv6Address = Core.Nothing,
      privateIpv4Address = Core.Nothing
    }

-- | The attachment ID for the network interface.
--
-- /Note:/ Consider using 'attachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niAttachmentId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niAttachmentId = Lens.field @"attachmentId"
{-# DEPRECATED niAttachmentId "Use generic-lens or generic-optics with 'attachmentId' instead." #-}

-- | The private IPv6 address for the network interface.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Address :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niIpv6Address = Lens.field @"ipv6Address"
{-# DEPRECATED niIpv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

-- | The private IPv4 address for the network interface.
--
-- /Note:/ Consider using 'privateIpv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIpv4Address :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niPrivateIpv4Address = Lens.field @"privateIpv4Address"
{-# DEPRECATED niPrivateIpv4Address "Use generic-lens or generic-optics with 'privateIpv4Address' instead." #-}

instance Core.FromJSON NetworkInterface where
  parseJSON =
    Core.withObject "NetworkInterface" Core.$
      \x ->
        NetworkInterface'
          Core.<$> (x Core..:? "attachmentId")
          Core.<*> (x Core..:? "ipv6Address")
          Core.<*> (x Core..:? "privateIpv4Address")
