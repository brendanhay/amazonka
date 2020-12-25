{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbNitSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbNitSettings
  ( DvbNitSettings (..),

    -- * Smart constructor
    mkDvbNitSettings,

    -- * Lenses
    dnsNetworkName,
    dnsNetworkId,
    dnsRepInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | DVB Network Information Table (NIT)
--
-- /See:/ 'mkDvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { -- | The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
    networkName :: Core.Text,
    -- | The numeric value placed in the Network Information Table (NIT).
    networkId :: Core.Natural,
    -- | The number of milliseconds between instances of this table in the output transport stream.
    repInterval :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DvbNitSettings' value with any optional fields omitted.
mkDvbNitSettings ::
  -- | 'networkName'
  Core.Text ->
  -- | 'networkId'
  Core.Natural ->
  DvbNitSettings
mkDvbNitSettings networkName networkId =
  DvbNitSettings'
    { networkName,
      networkId,
      repInterval = Core.Nothing
    }

-- | The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'networkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkName :: Lens.Lens' DvbNitSettings Core.Text
dnsNetworkName = Lens.field @"networkName"
{-# DEPRECATED dnsNetworkName "Use generic-lens or generic-optics with 'networkName' instead." #-}

-- | The numeric value placed in the Network Information Table (NIT).
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkId :: Lens.Lens' DvbNitSettings Core.Natural
dnsNetworkId = Lens.field @"networkId"
{-# DEPRECATED dnsNetworkId "Use generic-lens or generic-optics with 'networkId' instead." #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'repInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsRepInterval :: Lens.Lens' DvbNitSettings (Core.Maybe Core.Natural)
dnsRepInterval = Lens.field @"repInterval"
{-# DEPRECATED dnsRepInterval "Use generic-lens or generic-optics with 'repInterval' instead." #-}

instance Core.FromJSON DvbNitSettings where
  toJSON DvbNitSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("networkName" Core..= networkName),
            Core.Just ("networkId" Core..= networkId),
            ("repInterval" Core..=) Core.<$> repInterval
          ]
      )

instance Core.FromJSON DvbNitSettings where
  parseJSON =
    Core.withObject "DvbNitSettings" Core.$
      \x ->
        DvbNitSettings'
          Core.<$> (x Core..: "networkName")
          Core.<*> (x Core..: "networkId")
          Core.<*> (x Core..:? "repInterval")
