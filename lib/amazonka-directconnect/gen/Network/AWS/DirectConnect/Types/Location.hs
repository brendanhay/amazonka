{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Location
  ( Location (..)
  -- * Smart constructor
  , mkLocation
  -- * Lenses
  , lAvailablePortSpeeds
  , lAvailableProviders
  , lLocationCode
  , lLocationName
  , lRegion
  ) where

import qualified Network.AWS.DirectConnect.Types.LocationCode as Types
import qualified Network.AWS.DirectConnect.Types.LocationName as Types
import qualified Network.AWS.DirectConnect.Types.PortSpeed as Types
import qualified Network.AWS.DirectConnect.Types.ProviderName as Types
import qualified Network.AWS.DirectConnect.Types.Region as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an AWS Direct Connect location.
--
-- /See:/ 'mkLocation' smart constructor.
data Location = Location'
  { availablePortSpeeds :: Core.Maybe [Types.PortSpeed]
    -- ^ The available port speeds for the location.
  , availableProviders :: Core.Maybe [Types.ProviderName]
    -- ^ The name of the service provider for the location.
  , locationCode :: Core.Maybe Types.LocationCode
    -- ^ The code for the location.
  , locationName :: Core.Maybe Types.LocationName
    -- ^ The name of the location. This includes the name of the colocation partner and the physical site of the building.
  , region :: Core.Maybe Types.Region
    -- ^ The AWS Region for the location.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Location' value with any optional fields omitted.
mkLocation
    :: Location
mkLocation
  = Location'{availablePortSpeeds = Core.Nothing,
              availableProviders = Core.Nothing, locationCode = Core.Nothing,
              locationName = Core.Nothing, region = Core.Nothing}

-- | The available port speeds for the location.
--
-- /Note:/ Consider using 'availablePortSpeeds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAvailablePortSpeeds :: Lens.Lens' Location (Core.Maybe [Types.PortSpeed])
lAvailablePortSpeeds = Lens.field @"availablePortSpeeds"
{-# INLINEABLE lAvailablePortSpeeds #-}
{-# DEPRECATED availablePortSpeeds "Use generic-lens or generic-optics with 'availablePortSpeeds' instead"  #-}

-- | The name of the service provider for the location.
--
-- /Note:/ Consider using 'availableProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAvailableProviders :: Lens.Lens' Location (Core.Maybe [Types.ProviderName])
lAvailableProviders = Lens.field @"availableProviders"
{-# INLINEABLE lAvailableProviders #-}
{-# DEPRECATED availableProviders "Use generic-lens or generic-optics with 'availableProviders' instead"  #-}

-- | The code for the location.
--
-- /Note:/ Consider using 'locationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLocationCode :: Lens.Lens' Location (Core.Maybe Types.LocationCode)
lLocationCode = Lens.field @"locationCode"
{-# INLINEABLE lLocationCode #-}
{-# DEPRECATED locationCode "Use generic-lens or generic-optics with 'locationCode' instead"  #-}

-- | The name of the location. This includes the name of the colocation partner and the physical site of the building.
--
-- /Note:/ Consider using 'locationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLocationName :: Lens.Lens' Location (Core.Maybe Types.LocationName)
lLocationName = Lens.field @"locationName"
{-# INLINEABLE lLocationName #-}
{-# DEPRECATED locationName "Use generic-lens or generic-optics with 'locationName' instead"  #-}

-- | The AWS Region for the location.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lRegion :: Lens.Lens' Location (Core.Maybe Types.Region)
lRegion = Lens.field @"region"
{-# INLINEABLE lRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

instance Core.FromJSON Location where
        parseJSON
          = Core.withObject "Location" Core.$
              \ x ->
                Location' Core.<$>
                  (x Core..:? "availablePortSpeeds") Core.<*>
                    x Core..:? "availableProviders"
                    Core.<*> x Core..:? "locationCode"
                    Core.<*> x Core..:? "locationName"
                    Core.<*> x Core..:? "region"
