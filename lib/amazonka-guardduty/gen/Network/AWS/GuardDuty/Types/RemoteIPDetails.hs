{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.RemoteIPDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.RemoteIPDetails
  ( RemoteIPDetails (..),

    -- * Smart constructor
    mkRemoteIPDetails,

    -- * Lenses
    ridCountry,
    ridCity,
    ridIPAddressV4,
    ridGeoLocation,
    ridOrganization,
  )
where

import Network.AWS.GuardDuty.Types.City
import Network.AWS.GuardDuty.Types.Country
import Network.AWS.GuardDuty.Types.GeoLocation
import Network.AWS.GuardDuty.Types.Organization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the remote IP address of the connection.
--
-- /See:/ 'mkRemoteIPDetails' smart constructor.
data RemoteIPDetails = RemoteIPDetails'
  { country ::
      Lude.Maybe Country,
    city :: Lude.Maybe City,
    ipAddressV4 :: Lude.Maybe Lude.Text,
    geoLocation :: Lude.Maybe GeoLocation,
    organization :: Lude.Maybe Organization
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoteIPDetails' with the minimum fields required to make a request.
--
-- * 'city' - The city information of the remote IP address.
-- * 'country' - The country code of the remote IP address.
-- * 'geoLocation' - The location information of the remote IP address.
-- * 'ipAddressV4' - The IPv4 remote address of the connection.
-- * 'organization' - The ISP organization information of the remote IP address.
mkRemoteIPDetails ::
  RemoteIPDetails
mkRemoteIPDetails =
  RemoteIPDetails'
    { country = Lude.Nothing,
      city = Lude.Nothing,
      ipAddressV4 = Lude.Nothing,
      geoLocation = Lude.Nothing,
      organization = Lude.Nothing
    }

-- | The country code of the remote IP address.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridCountry :: Lens.Lens' RemoteIPDetails (Lude.Maybe Country)
ridCountry = Lens.lens (country :: RemoteIPDetails -> Lude.Maybe Country) (\s a -> s {country = a} :: RemoteIPDetails)
{-# DEPRECATED ridCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The city information of the remote IP address.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridCity :: Lens.Lens' RemoteIPDetails (Lude.Maybe City)
ridCity = Lens.lens (city :: RemoteIPDetails -> Lude.Maybe City) (\s a -> s {city = a} :: RemoteIPDetails)
{-# DEPRECATED ridCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | The IPv4 remote address of the connection.
--
-- /Note:/ Consider using 'ipAddressV4' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridIPAddressV4 :: Lens.Lens' RemoteIPDetails (Lude.Maybe Lude.Text)
ridIPAddressV4 = Lens.lens (ipAddressV4 :: RemoteIPDetails -> Lude.Maybe Lude.Text) (\s a -> s {ipAddressV4 = a} :: RemoteIPDetails)
{-# DEPRECATED ridIPAddressV4 "Use generic-lens or generic-optics with 'ipAddressV4' instead." #-}

-- | The location information of the remote IP address.
--
-- /Note:/ Consider using 'geoLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridGeoLocation :: Lens.Lens' RemoteIPDetails (Lude.Maybe GeoLocation)
ridGeoLocation = Lens.lens (geoLocation :: RemoteIPDetails -> Lude.Maybe GeoLocation) (\s a -> s {geoLocation = a} :: RemoteIPDetails)
{-# DEPRECATED ridGeoLocation "Use generic-lens or generic-optics with 'geoLocation' instead." #-}

-- | The ISP organization information of the remote IP address.
--
-- /Note:/ Consider using 'organization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridOrganization :: Lens.Lens' RemoteIPDetails (Lude.Maybe Organization)
ridOrganization = Lens.lens (organization :: RemoteIPDetails -> Lude.Maybe Organization) (\s a -> s {organization = a} :: RemoteIPDetails)
{-# DEPRECATED ridOrganization "Use generic-lens or generic-optics with 'organization' instead." #-}

instance Lude.FromJSON RemoteIPDetails where
  parseJSON =
    Lude.withObject
      "RemoteIPDetails"
      ( \x ->
          RemoteIPDetails'
            Lude.<$> (x Lude..:? "country")
            Lude.<*> (x Lude..:? "city")
            Lude.<*> (x Lude..:? "ipAddressV4")
            Lude.<*> (x Lude..:? "geoLocation")
            Lude.<*> (x Lude..:? "organization")
      )
