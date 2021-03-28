{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.InstanceAccess
  ( InstanceAccess (..)
  -- * Smart constructor
  , mkInstanceAccess
  -- * Lenses
  , iaCredentials
  , iaFleetId
  , iaInstanceId
  , iaIpAddress
  , iaOperatingSystem
  ) where

import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.GameLift.Types.InstanceCredentials as Types
import qualified Network.AWS.GameLift.Types.InstanceId as Types
import qualified Network.AWS.GameLift.Types.IpAddress as Types
import qualified Network.AWS.GameLift.Types.OperatingSystem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information required to remotely connect to a fleet instance. Access is requested by calling 'GetInstanceAccess' . 
--
-- /See:/ 'mkInstanceAccess' smart constructor.
data InstanceAccess = InstanceAccess'
  { credentials :: Core.Maybe Types.InstanceCredentials
    -- ^ Credentials required to access the instance.
  , fleetId :: Core.Maybe Types.FleetId
    -- ^ A unique identifier for a fleet containing the instance being accessed.
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ A unique identifier for an instance being accessed.
  , ipAddress :: Core.Maybe Types.IpAddress
    -- ^ IP address that is assigned to the instance.
  , operatingSystem :: Core.Maybe Types.OperatingSystem
    -- ^ Operating system that is running on the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceAccess' value with any optional fields omitted.
mkInstanceAccess
    :: InstanceAccess
mkInstanceAccess
  = InstanceAccess'{credentials = Core.Nothing,
                    fleetId = Core.Nothing, instanceId = Core.Nothing,
                    ipAddress = Core.Nothing, operatingSystem = Core.Nothing}

-- | Credentials required to access the instance.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaCredentials :: Lens.Lens' InstanceAccess (Core.Maybe Types.InstanceCredentials)
iaCredentials = Lens.field @"credentials"
{-# INLINEABLE iaCredentials #-}
{-# DEPRECATED credentials "Use generic-lens or generic-optics with 'credentials' instead"  #-}

-- | A unique identifier for a fleet containing the instance being accessed.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaFleetId :: Lens.Lens' InstanceAccess (Core.Maybe Types.FleetId)
iaFleetId = Lens.field @"fleetId"
{-# INLINEABLE iaFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | A unique identifier for an instance being accessed.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaInstanceId :: Lens.Lens' InstanceAccess (Core.Maybe Types.InstanceId)
iaInstanceId = Lens.field @"instanceId"
{-# INLINEABLE iaInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | IP address that is assigned to the instance.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaIpAddress :: Lens.Lens' InstanceAccess (Core.Maybe Types.IpAddress)
iaIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE iaIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | Operating system that is running on the instance.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaOperatingSystem :: Lens.Lens' InstanceAccess (Core.Maybe Types.OperatingSystem)
iaOperatingSystem = Lens.field @"operatingSystem"
{-# INLINEABLE iaOperatingSystem #-}
{-# DEPRECATED operatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead"  #-}

instance Core.FromJSON InstanceAccess where
        parseJSON
          = Core.withObject "InstanceAccess" Core.$
              \ x ->
                InstanceAccess' Core.<$>
                  (x Core..:? "Credentials") Core.<*> x Core..:? "FleetId" Core.<*>
                    x Core..:? "InstanceId"
                    Core.<*> x Core..:? "IpAddress"
                    Core.<*> x Core..:? "OperatingSystem"
