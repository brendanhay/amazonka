{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbNitSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.DvbNitSettings
  ( DvbNitSettings (..)
  -- * Smart constructor
  , mkDvbNitSettings
  -- * Lenses
  , dnsNetworkName
  , dnsNetworkId
  , dnsRepInterval
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | DVB Network Information Table (NIT)
--
-- /See:/ 'mkDvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { networkName :: Core.Text
    -- ^ The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
  , networkId :: Core.Natural
    -- ^ The numeric value placed in the Network Information Table (NIT).
  , repInterval :: Core.Maybe Core.Natural
    -- ^ The number of milliseconds between instances of this table in the output transport stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DvbNitSettings' value with any optional fields omitted.
mkDvbNitSettings
    :: Core.Text -- ^ 'networkName'
    -> Core.Natural -- ^ 'networkId'
    -> DvbNitSettings
mkDvbNitSettings networkName networkId
  = DvbNitSettings'{networkName, networkId,
                    repInterval = Core.Nothing}

-- | The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'networkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkName :: Lens.Lens' DvbNitSettings Core.Text
dnsNetworkName = Lens.field @"networkName"
{-# INLINEABLE dnsNetworkName #-}
{-# DEPRECATED networkName "Use generic-lens or generic-optics with 'networkName' instead"  #-}

-- | The numeric value placed in the Network Information Table (NIT).
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkId :: Lens.Lens' DvbNitSettings Core.Natural
dnsNetworkId = Lens.field @"networkId"
{-# INLINEABLE dnsNetworkId #-}
{-# DEPRECATED networkId "Use generic-lens or generic-optics with 'networkId' instead"  #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'repInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsRepInterval :: Lens.Lens' DvbNitSettings (Core.Maybe Core.Natural)
dnsRepInterval = Lens.field @"repInterval"
{-# INLINEABLE dnsRepInterval #-}
{-# DEPRECATED repInterval "Use generic-lens or generic-optics with 'repInterval' instead"  #-}

instance Core.FromJSON DvbNitSettings where
        toJSON DvbNitSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("networkName" Core..= networkName),
                  Core.Just ("networkId" Core..= networkId),
                  ("repInterval" Core..=) Core.<$> repInterval])

instance Core.FromJSON DvbNitSettings where
        parseJSON
          = Core.withObject "DvbNitSettings" Core.$
              \ x ->
                DvbNitSettings' Core.<$>
                  (x Core..: "networkName") Core.<*> x Core..: "networkId" Core.<*>
                    x Core..:? "repInterval"
