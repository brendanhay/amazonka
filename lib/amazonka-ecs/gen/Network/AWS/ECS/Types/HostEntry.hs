{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HostEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HostEntry
  ( HostEntry (..),

    -- * Smart constructor
    mkHostEntry,

    -- * Lenses
    heHostname,
    heIpAddress,
  )
where

import qualified Network.AWS.ECS.Types.Hostname as Types
import qualified Network.AWS.ECS.Types.IpAddress as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Hostnames and IP address entries that are added to the @/etc/hosts@ file of a container via the @extraHosts@ parameter of its 'ContainerDefinition' .
--
-- /See:/ 'mkHostEntry' smart constructor.
data HostEntry = HostEntry'
  { -- | The hostname to use in the @/etc/hosts@ entry.
    hostname :: Types.Hostname,
    -- | The IP address to use in the @/etc/hosts@ entry.
    ipAddress :: Types.IpAddress
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HostEntry' value with any optional fields omitted.
mkHostEntry ::
  -- | 'hostname'
  Types.Hostname ->
  -- | 'ipAddress'
  Types.IpAddress ->
  HostEntry
mkHostEntry hostname ipAddress = HostEntry' {hostname, ipAddress}

-- | The hostname to use in the @/etc/hosts@ entry.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heHostname :: Lens.Lens' HostEntry Types.Hostname
heHostname = Lens.field @"hostname"
{-# DEPRECATED heHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The IP address to use in the @/etc/hosts@ entry.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heIpAddress :: Lens.Lens' HostEntry Types.IpAddress
heIpAddress = Lens.field @"ipAddress"
{-# DEPRECATED heIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

instance Core.FromJSON HostEntry where
  toJSON HostEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("hostname" Core..= hostname),
            Core.Just ("ipAddress" Core..= ipAddress)
          ]
      )

instance Core.FromJSON HostEntry where
  parseJSON =
    Core.withObject "HostEntry" Core.$
      \x ->
        HostEntry'
          Core.<$> (x Core..: "hostname") Core.<*> (x Core..: "ipAddress")
