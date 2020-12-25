{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PortProbeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PortProbeDetail
  ( PortProbeDetail (..),

    -- * Smart constructor
    mkPortProbeDetail,

    -- * Lenses
    ppdLocalIpDetails,
    ppdLocalPortDetails,
    ppdRemoteIpDetails,
  )
where

import qualified Network.AWS.GuardDuty.Types.LocalIpDetails as Types
import qualified Network.AWS.GuardDuty.Types.LocalPortDetails as Types
import qualified Network.AWS.GuardDuty.Types.RemoteIpDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the port probe details.
--
-- /See:/ 'mkPortProbeDetail' smart constructor.
data PortProbeDetail = PortProbeDetail'
  { -- | The local IP information of the connection.
    localIpDetails :: Core.Maybe Types.LocalIpDetails,
    -- | The local port information of the connection.
    localPortDetails :: Core.Maybe Types.LocalPortDetails,
    -- | The remote IP information of the connection.
    remoteIpDetails :: Core.Maybe Types.RemoteIpDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PortProbeDetail' value with any optional fields omitted.
mkPortProbeDetail ::
  PortProbeDetail
mkPortProbeDetail =
  PortProbeDetail'
    { localIpDetails = Core.Nothing,
      localPortDetails = Core.Nothing,
      remoteIpDetails = Core.Nothing
    }

-- | The local IP information of the connection.
--
-- /Note:/ Consider using 'localIpDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdLocalIpDetails :: Lens.Lens' PortProbeDetail (Core.Maybe Types.LocalIpDetails)
ppdLocalIpDetails = Lens.field @"localIpDetails"
{-# DEPRECATED ppdLocalIpDetails "Use generic-lens or generic-optics with 'localIpDetails' instead." #-}

-- | The local port information of the connection.
--
-- /Note:/ Consider using 'localPortDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdLocalPortDetails :: Lens.Lens' PortProbeDetail (Core.Maybe Types.LocalPortDetails)
ppdLocalPortDetails = Lens.field @"localPortDetails"
{-# DEPRECATED ppdLocalPortDetails "Use generic-lens or generic-optics with 'localPortDetails' instead." #-}

-- | The remote IP information of the connection.
--
-- /Note:/ Consider using 'remoteIpDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppdRemoteIpDetails :: Lens.Lens' PortProbeDetail (Core.Maybe Types.RemoteIpDetails)
ppdRemoteIpDetails = Lens.field @"remoteIpDetails"
{-# DEPRECATED ppdRemoteIpDetails "Use generic-lens or generic-optics with 'remoteIpDetails' instead." #-}

instance Core.FromJSON PortProbeDetail where
  parseJSON =
    Core.withObject "PortProbeDetail" Core.$
      \x ->
        PortProbeDetail'
          Core.<$> (x Core..:? "localIpDetails")
          Core.<*> (x Core..:? "localPortDetails")
          Core.<*> (x Core..:? "remoteIpDetails")
