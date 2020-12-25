{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ConnectivityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ConnectivityInfo
  ( ConnectivityInfo (..),

    -- * Smart constructor
    mkConnectivityInfo,

    -- * Lenses
    ciHostAddress,
    ciId,
    ciMetadata,
    ciPortNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a Greengrass core's connectivity.
--
-- /See:/ 'mkConnectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { -- | The endpoint for the Greengrass core. Can be an IP address or DNS.
    hostAddress :: Core.Maybe Core.Text,
    -- | The ID of the connectivity information.
    id :: Core.Maybe Core.Text,
    -- | Metadata for this endpoint.
    metadata :: Core.Maybe Core.Text,
    -- | The port of the Greengrass core. Usually 8883.
    portNumber :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectivityInfo' value with any optional fields omitted.
mkConnectivityInfo ::
  ConnectivityInfo
mkConnectivityInfo =
  ConnectivityInfo'
    { hostAddress = Core.Nothing,
      id = Core.Nothing,
      metadata = Core.Nothing,
      portNumber = Core.Nothing
    }

-- | The endpoint for the Greengrass core. Can be an IP address or DNS.
--
-- /Note:/ Consider using 'hostAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciHostAddress :: Lens.Lens' ConnectivityInfo (Core.Maybe Core.Text)
ciHostAddress = Lens.field @"hostAddress"
{-# DEPRECATED ciHostAddress "Use generic-lens or generic-optics with 'hostAddress' instead." #-}

-- | The ID of the connectivity information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciId :: Lens.Lens' ConnectivityInfo (Core.Maybe Core.Text)
ciId = Lens.field @"id"
{-# DEPRECATED ciId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Metadata for this endpoint.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciMetadata :: Lens.Lens' ConnectivityInfo (Core.Maybe Core.Text)
ciMetadata = Lens.field @"metadata"
{-# DEPRECATED ciMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The port of the Greengrass core. Usually 8883.
--
-- /Note:/ Consider using 'portNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPortNumber :: Lens.Lens' ConnectivityInfo (Core.Maybe Core.Int)
ciPortNumber = Lens.field @"portNumber"
{-# DEPRECATED ciPortNumber "Use generic-lens or generic-optics with 'portNumber' instead." #-}

instance Core.FromJSON ConnectivityInfo where
  toJSON ConnectivityInfo {..} =
    Core.object
      ( Core.catMaybes
          [ ("HostAddress" Core..=) Core.<$> hostAddress,
            ("Id" Core..=) Core.<$> id,
            ("Metadata" Core..=) Core.<$> metadata,
            ("PortNumber" Core..=) Core.<$> portNumber
          ]
      )

instance Core.FromJSON ConnectivityInfo where
  parseJSON =
    Core.withObject "ConnectivityInfo" Core.$
      \x ->
        ConnectivityInfo'
          Core.<$> (x Core..:? "HostAddress")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Metadata")
          Core.<*> (x Core..:? "PortNumber")
