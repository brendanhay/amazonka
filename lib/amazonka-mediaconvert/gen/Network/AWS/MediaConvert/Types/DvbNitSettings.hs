{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbNitSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbNitSettings
  ( DvbNitSettings (..)
  -- * Smart constructor
  , mkDvbNitSettings
  -- * Lenses
  , dnsNetworkId
  , dnsNetworkName
  , dnsNitInterval
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- /See:/ 'mkDvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { networkId :: Core.Maybe Core.Natural
    -- ^ The numeric value placed in the Network Information Table (NIT).
  , networkName :: Core.Maybe Core.Text
    -- ^ The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
  , nitInterval :: Core.Maybe Core.Natural
    -- ^ The number of milliseconds between instances of this table in the output transport stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DvbNitSettings' value with any optional fields omitted.
mkDvbNitSettings
    :: DvbNitSettings
mkDvbNitSettings
  = DvbNitSettings'{networkId = Core.Nothing,
                    networkName = Core.Nothing, nitInterval = Core.Nothing}

-- | The numeric value placed in the Network Information Table (NIT).
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkId :: Lens.Lens' DvbNitSettings (Core.Maybe Core.Natural)
dnsNetworkId = Lens.field @"networkId"
{-# INLINEABLE dnsNetworkId #-}
{-# DEPRECATED networkId "Use generic-lens or generic-optics with 'networkId' instead"  #-}

-- | The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- /Note:/ Consider using 'networkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNetworkName :: Lens.Lens' DvbNitSettings (Core.Maybe Core.Text)
dnsNetworkName = Lens.field @"networkName"
{-# INLINEABLE dnsNetworkName #-}
{-# DEPRECATED networkName "Use generic-lens or generic-optics with 'networkName' instead"  #-}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'nitInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsNitInterval :: Lens.Lens' DvbNitSettings (Core.Maybe Core.Natural)
dnsNitInterval = Lens.field @"nitInterval"
{-# INLINEABLE dnsNitInterval #-}
{-# DEPRECATED nitInterval "Use generic-lens or generic-optics with 'nitInterval' instead"  #-}

instance Core.FromJSON DvbNitSettings where
        toJSON DvbNitSettings{..}
          = Core.object
              (Core.catMaybes
                 [("networkId" Core..=) Core.<$> networkId,
                  ("networkName" Core..=) Core.<$> networkName,
                  ("nitInterval" Core..=) Core.<$> nitInterval])

instance Core.FromJSON DvbNitSettings where
        parseJSON
          = Core.withObject "DvbNitSettings" Core.$
              \ x ->
                DvbNitSettings' Core.<$>
                  (x Core..:? "networkId") Core.<*> x Core..:? "networkName" Core.<*>
                    x Core..:? "nitInterval"
