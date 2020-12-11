-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointLocation
  ( EndpointLocation (..),

    -- * Smart constructor
    mkEndpointLocation,

    -- * Lenses
    elPostalCode,
    elLatitude,
    elCountry,
    elCity,
    elRegion,
    elLongitude,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies geographic information about an endpoint.
--
-- /See:/ 'mkEndpointLocation' smart constructor.
data EndpointLocation = EndpointLocation'
  { postalCode ::
      Lude.Maybe Lude.Text,
    latitude :: Lude.Maybe Lude.Double,
    country :: Lude.Maybe Lude.Text,
    city :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    longitude :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointLocation' with the minimum fields required to make a request.
--
-- * 'city' - The name of the city where the endpoint is located.
-- * 'country' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the endpoint is located. For example, US for the United States.
-- * 'latitude' - The latitude coordinate of the endpoint location, rounded to one decimal place.
-- * 'longitude' - The longitude coordinate of the endpoint location, rounded to one decimal place.
-- * 'postalCode' - The postal or ZIP code for the area where the endpoint is located.
-- * 'region' - The name of the region where the endpoint is located. For locations in the United States, this value is the name of a state.
mkEndpointLocation ::
  EndpointLocation
mkEndpointLocation =
  EndpointLocation'
    { postalCode = Lude.Nothing,
      latitude = Lude.Nothing,
      country = Lude.Nothing,
      city = Lude.Nothing,
      region = Lude.Nothing,
      longitude = Lude.Nothing
    }

-- | The postal or ZIP code for the area where the endpoint is located.
--
-- /Note:/ Consider using 'postalCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elPostalCode :: Lens.Lens' EndpointLocation (Lude.Maybe Lude.Text)
elPostalCode = Lens.lens (postalCode :: EndpointLocation -> Lude.Maybe Lude.Text) (\s a -> s {postalCode = a} :: EndpointLocation)
{-# DEPRECATED elPostalCode "Use generic-lens or generic-optics with 'postalCode' instead." #-}

-- | The latitude coordinate of the endpoint location, rounded to one decimal place.
--
-- /Note:/ Consider using 'latitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLatitude :: Lens.Lens' EndpointLocation (Lude.Maybe Lude.Double)
elLatitude = Lens.lens (latitude :: EndpointLocation -> Lude.Maybe Lude.Double) (\s a -> s {latitude = a} :: EndpointLocation)
{-# DEPRECATED elLatitude "Use generic-lens or generic-optics with 'latitude' instead." #-}

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the endpoint is located. For example, US for the United States.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elCountry :: Lens.Lens' EndpointLocation (Lude.Maybe Lude.Text)
elCountry = Lens.lens (country :: EndpointLocation -> Lude.Maybe Lude.Text) (\s a -> s {country = a} :: EndpointLocation)
{-# DEPRECATED elCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The name of the city where the endpoint is located.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elCity :: Lens.Lens' EndpointLocation (Lude.Maybe Lude.Text)
elCity = Lens.lens (city :: EndpointLocation -> Lude.Maybe Lude.Text) (\s a -> s {city = a} :: EndpointLocation)
{-# DEPRECATED elCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | The name of the region where the endpoint is located. For locations in the United States, this value is the name of a state.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elRegion :: Lens.Lens' EndpointLocation (Lude.Maybe Lude.Text)
elRegion = Lens.lens (region :: EndpointLocation -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: EndpointLocation)
{-# DEPRECATED elRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The longitude coordinate of the endpoint location, rounded to one decimal place.
--
-- /Note:/ Consider using 'longitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLongitude :: Lens.Lens' EndpointLocation (Lude.Maybe Lude.Double)
elLongitude = Lens.lens (longitude :: EndpointLocation -> Lude.Maybe Lude.Double) (\s a -> s {longitude = a} :: EndpointLocation)
{-# DEPRECATED elLongitude "Use generic-lens or generic-optics with 'longitude' instead." #-}

instance Lude.FromJSON EndpointLocation where
  parseJSON =
    Lude.withObject
      "EndpointLocation"
      ( \x ->
          EndpointLocation'
            Lude.<$> (x Lude..:? "PostalCode")
            Lude.<*> (x Lude..:? "Latitude")
            Lude.<*> (x Lude..:? "Country")
            Lude.<*> (x Lude..:? "City")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "Longitude")
      )

instance Lude.ToJSON EndpointLocation where
  toJSON EndpointLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PostalCode" Lude..=) Lude.<$> postalCode,
            ("Latitude" Lude..=) Lude.<$> latitude,
            ("Country" Lude..=) Lude.<$> country,
            ("City" Lude..=) Lude.<$> city,
            ("Region" Lude..=) Lude.<$> region,
            ("Longitude" Lude..=) Lude.<$> longitude
          ]
      )
