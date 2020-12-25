{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.LocalPortDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.LocalPortDetails
  ( LocalPortDetails (..),

    -- * Smart constructor
    mkLocalPortDetails,

    -- * Lenses
    lpdPort,
    lpdPortName,
  )
where

import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the port for the local connection.
--
-- /See:/ 'mkLocalPortDetails' smart constructor.
data LocalPortDetails = LocalPortDetails'
  { -- | The port number of the local connection.
    port :: Core.Maybe Core.Int,
    -- | The port name of the local connection.
    portName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalPortDetails' value with any optional fields omitted.
mkLocalPortDetails ::
  LocalPortDetails
mkLocalPortDetails =
  LocalPortDetails' {port = Core.Nothing, portName = Core.Nothing}

-- | The port number of the local connection.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdPort :: Lens.Lens' LocalPortDetails (Core.Maybe Core.Int)
lpdPort = Lens.field @"port"
{-# DEPRECATED lpdPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The port name of the local connection.
--
-- /Note:/ Consider using 'portName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdPortName :: Lens.Lens' LocalPortDetails (Core.Maybe Types.String)
lpdPortName = Lens.field @"portName"
{-# DEPRECATED lpdPortName "Use generic-lens or generic-optics with 'portName' instead." #-}

instance Core.FromJSON LocalPortDetails where
  parseJSON =
    Core.withObject "LocalPortDetails" Core.$
      \x ->
        LocalPortDetails'
          Core.<$> (x Core..:? "port") Core.<*> (x Core..:? "portName")
