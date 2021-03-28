{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.GeoLocationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.GeoLocationDetails
  ( GeoLocationDetails (..)
  -- * Smart constructor
  , mkGeoLocationDetails
  -- * Lenses
  , gldContinentCode
  , gldContinentName
  , gldCountryCode
  , gldCountryName
  , gldSubdivisionCode
  , gldSubdivisionName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.ContinentCode as Types
import qualified Network.AWS.Route53.Types.ContinentName as Types
import qualified Network.AWS.Route53.Types.CountryCode as Types
import qualified Network.AWS.Route53.Types.GeoLocationCountryName as Types
import qualified Network.AWS.Route53.Types.SubdivisionCode as Types
import qualified Network.AWS.Route53.Types.SubdivisionName as Types

-- | A complex type that contains the codes and full continent, country, and subdivision names for the specified @geolocation@ code.
--
-- /See:/ 'mkGeoLocationDetails' smart constructor.
data GeoLocationDetails = GeoLocationDetails'
  { continentCode :: Core.Maybe Types.ContinentCode
    -- ^ The two-letter code for the continent.
  , continentName :: Core.Maybe Types.ContinentName
    -- ^ The full name of the continent.
  , countryCode :: Core.Maybe Types.CountryCode
    -- ^ The two-letter code for the country.
  , countryName :: Core.Maybe Types.GeoLocationCountryName
    -- ^ The name of the country.
  , subdivisionCode :: Core.Maybe Types.SubdivisionCode
    -- ^ The code for the subdivision. Route 53 currently supports only states in the United States.
  , subdivisionName :: Core.Maybe Types.SubdivisionName
    -- ^ The full name of the subdivision. Route 53 currently supports only states in the United States.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GeoLocationDetails' value with any optional fields omitted.
mkGeoLocationDetails
    :: GeoLocationDetails
mkGeoLocationDetails
  = GeoLocationDetails'{continentCode = Core.Nothing,
                        continentName = Core.Nothing, countryCode = Core.Nothing,
                        countryName = Core.Nothing, subdivisionCode = Core.Nothing,
                        subdivisionName = Core.Nothing}

-- | The two-letter code for the continent.
--
-- /Note:/ Consider using 'continentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldContinentCode :: Lens.Lens' GeoLocationDetails (Core.Maybe Types.ContinentCode)
gldContinentCode = Lens.field @"continentCode"
{-# INLINEABLE gldContinentCode #-}
{-# DEPRECATED continentCode "Use generic-lens or generic-optics with 'continentCode' instead"  #-}

-- | The full name of the continent.
--
-- /Note:/ Consider using 'continentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldContinentName :: Lens.Lens' GeoLocationDetails (Core.Maybe Types.ContinentName)
gldContinentName = Lens.field @"continentName"
{-# INLINEABLE gldContinentName #-}
{-# DEPRECATED continentName "Use generic-lens or generic-optics with 'continentName' instead"  #-}

-- | The two-letter code for the country.
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldCountryCode :: Lens.Lens' GeoLocationDetails (Core.Maybe Types.CountryCode)
gldCountryCode = Lens.field @"countryCode"
{-# INLINEABLE gldCountryCode #-}
{-# DEPRECATED countryCode "Use generic-lens or generic-optics with 'countryCode' instead"  #-}

-- | The name of the country.
--
-- /Note:/ Consider using 'countryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldCountryName :: Lens.Lens' GeoLocationDetails (Core.Maybe Types.GeoLocationCountryName)
gldCountryName = Lens.field @"countryName"
{-# INLINEABLE gldCountryName #-}
{-# DEPRECATED countryName "Use generic-lens or generic-optics with 'countryName' instead"  #-}

-- | The code for the subdivision. Route 53 currently supports only states in the United States.
--
-- /Note:/ Consider using 'subdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldSubdivisionCode :: Lens.Lens' GeoLocationDetails (Core.Maybe Types.SubdivisionCode)
gldSubdivisionCode = Lens.field @"subdivisionCode"
{-# INLINEABLE gldSubdivisionCode #-}
{-# DEPRECATED subdivisionCode "Use generic-lens or generic-optics with 'subdivisionCode' instead"  #-}

-- | The full name of the subdivision. Route 53 currently supports only states in the United States.
--
-- /Note:/ Consider using 'subdivisionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldSubdivisionName :: Lens.Lens' GeoLocationDetails (Core.Maybe Types.SubdivisionName)
gldSubdivisionName = Lens.field @"subdivisionName"
{-# INLINEABLE gldSubdivisionName #-}
{-# DEPRECATED subdivisionName "Use generic-lens or generic-optics with 'subdivisionName' instead"  #-}

instance Core.FromXML GeoLocationDetails where
        parseXML x
          = GeoLocationDetails' Core.<$>
              (x Core..@? "ContinentCode") Core.<*> x Core..@? "ContinentName"
                Core.<*> x Core..@? "CountryCode"
                Core.<*> x Core..@? "CountryName"
                Core.<*> x Core..@? "SubdivisionCode"
                Core.<*> x Core..@? "SubdivisionName"
