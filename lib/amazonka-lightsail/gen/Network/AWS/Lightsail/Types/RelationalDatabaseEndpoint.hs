{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
  ( RelationalDatabaseEndpoint (..),

    -- * Smart constructor
    mkRelationalDatabaseEndpoint,

    -- * Lenses
    rdeAddress,
    rdePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an endpoint for a database.
--
-- /See:/ 'mkRelationalDatabaseEndpoint' smart constructor.
data RelationalDatabaseEndpoint = RelationalDatabaseEndpoint'
  { -- | Specifies the DNS address of the database.
    address :: Core.Maybe Types.NonEmptyString,
    -- | Specifies the port that the database is listening on.
    port :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RelationalDatabaseEndpoint' value with any optional fields omitted.
mkRelationalDatabaseEndpoint ::
  RelationalDatabaseEndpoint
mkRelationalDatabaseEndpoint =
  RelationalDatabaseEndpoint'
    { address = Core.Nothing,
      port = Core.Nothing
    }

-- | Specifies the DNS address of the database.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeAddress :: Lens.Lens' RelationalDatabaseEndpoint (Core.Maybe Types.NonEmptyString)
rdeAddress = Lens.field @"address"
{-# DEPRECATED rdeAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | Specifies the port that the database is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdePort :: Lens.Lens' RelationalDatabaseEndpoint (Core.Maybe Core.Int)
rdePort = Lens.field @"port"
{-# DEPRECATED rdePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Core.FromJSON RelationalDatabaseEndpoint where
  parseJSON =
    Core.withObject "RelationalDatabaseEndpoint" Core.$
      \x ->
        RelationalDatabaseEndpoint'
          Core.<$> (x Core..:? "address") Core.<*> (x Core..:? "port")
