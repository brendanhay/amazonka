{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.NetworkAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.NetworkAccessConfiguration
  ( NetworkAccessConfiguration (..),

    -- * Smart constructor
    mkNetworkAccessConfiguration,

    -- * Lenses
    nacEniId,
    nacEniPrivateIpAddress,
  )
where

import qualified Network.AWS.AppStream.Types.EniId as Types
import qualified Network.AWS.AppStream.Types.EniPrivateIpAddress as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the network details of the fleet or image builder instance.
--
-- /See:/ 'mkNetworkAccessConfiguration' smart constructor.
data NetworkAccessConfiguration = NetworkAccessConfiguration'
  { -- | The resource identifier of the elastic network interface that is attached to instances in your VPC. All network interfaces have the eni-xxxxxxxx resource identifier.
    eniId :: Core.Maybe Types.EniId,
    -- | The private IP address of the elastic network interface that is attached to instances in your VPC.
    eniPrivateIpAddress :: Core.Maybe Types.EniPrivateIpAddress
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkAccessConfiguration' value with any optional fields omitted.
mkNetworkAccessConfiguration ::
  NetworkAccessConfiguration
mkNetworkAccessConfiguration =
  NetworkAccessConfiguration'
    { eniId = Core.Nothing,
      eniPrivateIpAddress = Core.Nothing
    }

-- | The resource identifier of the elastic network interface that is attached to instances in your VPC. All network interfaces have the eni-xxxxxxxx resource identifier.
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nacEniId :: Lens.Lens' NetworkAccessConfiguration (Core.Maybe Types.EniId)
nacEniId = Lens.field @"eniId"
{-# DEPRECATED nacEniId "Use generic-lens or generic-optics with 'eniId' instead." #-}

-- | The private IP address of the elastic network interface that is attached to instances in your VPC.
--
-- /Note:/ Consider using 'eniPrivateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nacEniPrivateIpAddress :: Lens.Lens' NetworkAccessConfiguration (Core.Maybe Types.EniPrivateIpAddress)
nacEniPrivateIpAddress = Lens.field @"eniPrivateIpAddress"
{-# DEPRECATED nacEniPrivateIpAddress "Use generic-lens or generic-optics with 'eniPrivateIpAddress' instead." #-}

instance Core.FromJSON NetworkAccessConfiguration where
  parseJSON =
    Core.withObject "NetworkAccessConfiguration" Core.$
      \x ->
        NetworkAccessConfiguration'
          Core.<$> (x Core..:? "EniId") Core.<*> (x Core..:? "EniPrivateIpAddress")
