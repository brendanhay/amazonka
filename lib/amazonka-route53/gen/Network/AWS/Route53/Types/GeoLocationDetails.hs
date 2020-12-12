{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.GeoLocationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.GeoLocationDetails
  ( GeoLocationDetails (..),

    -- * Smart constructor
    mkGeoLocationDetails,

    -- * Lenses
    gldSubdivisionName,
    gldSubdivisionCode,
    gldCountryName,
    gldCountryCode,
    gldContinentCode,
    gldContinentName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | A complex type that contains the codes and full continent, country, and subdivision names for the specified @geolocation@ code.
--
-- /See:/ 'mkGeoLocationDetails' smart constructor.
data GeoLocationDetails = GeoLocationDetails'
  { subdivisionName ::
      Lude.Maybe Lude.Text,
    subdivisionCode :: Lude.Maybe Lude.Text,
    countryName :: Lude.Maybe Lude.Text,
    countryCode :: Lude.Maybe Lude.Text,
    continentCode :: Lude.Maybe Lude.Text,
    continentName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GeoLocationDetails' with the minimum fields required to make a request.
--
-- * 'continentCode' - The two-letter code for the continent.
-- * 'continentName' - The full name of the continent.
-- * 'countryCode' - The two-letter code for the country.
-- * 'countryName' - The name of the country.
-- * 'subdivisionCode' - The code for the subdivision. Route 53 currently supports only states in the United States.
-- * 'subdivisionName' - The full name of the subdivision. Route 53 currently supports only states in the United States.
mkGeoLocationDetails ::
  GeoLocationDetails
mkGeoLocationDetails =
  GeoLocationDetails'
    { subdivisionName = Lude.Nothing,
      subdivisionCode = Lude.Nothing,
      countryName = Lude.Nothing,
      countryCode = Lude.Nothing,
      continentCode = Lude.Nothing,
      continentName = Lude.Nothing
    }

-- | The full name of the subdivision. Route 53 currently supports only states in the United States.
--
-- /Note:/ Consider using 'subdivisionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldSubdivisionName :: Lens.Lens' GeoLocationDetails (Lude.Maybe Lude.Text)
gldSubdivisionName = Lens.lens (subdivisionName :: GeoLocationDetails -> Lude.Maybe Lude.Text) (\s a -> s {subdivisionName = a} :: GeoLocationDetails)
{-# DEPRECATED gldSubdivisionName "Use generic-lens or generic-optics with 'subdivisionName' instead." #-}

-- | The code for the subdivision. Route 53 currently supports only states in the United States.
--
-- /Note:/ Consider using 'subdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldSubdivisionCode :: Lens.Lens' GeoLocationDetails (Lude.Maybe Lude.Text)
gldSubdivisionCode = Lens.lens (subdivisionCode :: GeoLocationDetails -> Lude.Maybe Lude.Text) (\s a -> s {subdivisionCode = a} :: GeoLocationDetails)
{-# DEPRECATED gldSubdivisionCode "Use generic-lens or generic-optics with 'subdivisionCode' instead." #-}

-- | The name of the country.
--
-- /Note:/ Consider using 'countryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldCountryName :: Lens.Lens' GeoLocationDetails (Lude.Maybe Lude.Text)
gldCountryName = Lens.lens (countryName :: GeoLocationDetails -> Lude.Maybe Lude.Text) (\s a -> s {countryName = a} :: GeoLocationDetails)
{-# DEPRECATED gldCountryName "Use generic-lens or generic-optics with 'countryName' instead." #-}

-- | The two-letter code for the country.
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldCountryCode :: Lens.Lens' GeoLocationDetails (Lude.Maybe Lude.Text)
gldCountryCode = Lens.lens (countryCode :: GeoLocationDetails -> Lude.Maybe Lude.Text) (\s a -> s {countryCode = a} :: GeoLocationDetails)
{-# DEPRECATED gldCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

-- | The two-letter code for the continent.
--
-- /Note:/ Consider using 'continentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldContinentCode :: Lens.Lens' GeoLocationDetails (Lude.Maybe Lude.Text)
gldContinentCode = Lens.lens (continentCode :: GeoLocationDetails -> Lude.Maybe Lude.Text) (\s a -> s {continentCode = a} :: GeoLocationDetails)
{-# DEPRECATED gldContinentCode "Use generic-lens or generic-optics with 'continentCode' instead." #-}

-- | The full name of the continent.
--
-- /Note:/ Consider using 'continentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldContinentName :: Lens.Lens' GeoLocationDetails (Lude.Maybe Lude.Text)
gldContinentName = Lens.lens (continentName :: GeoLocationDetails -> Lude.Maybe Lude.Text) (\s a -> s {continentName = a} :: GeoLocationDetails)
{-# DEPRECATED gldContinentName "Use generic-lens or generic-optics with 'continentName' instead." #-}

instance Lude.FromXML GeoLocationDetails where
  parseXML x =
    GeoLocationDetails'
      Lude.<$> (x Lude..@? "SubdivisionName")
      Lude.<*> (x Lude..@? "SubdivisionCode")
      Lude.<*> (x Lude..@? "CountryName")
      Lude.<*> (x Lude..@? "CountryCode")
      Lude.<*> (x Lude..@? "ContinentCode")
      Lude.<*> (x Lude..@? "ContinentName")
