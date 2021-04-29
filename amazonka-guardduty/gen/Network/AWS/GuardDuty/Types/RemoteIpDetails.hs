{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.RemoteIpDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.RemoteIpDetails where

import Network.AWS.GuardDuty.Types.City
import Network.AWS.GuardDuty.Types.Country
import Network.AWS.GuardDuty.Types.GeoLocation
import Network.AWS.GuardDuty.Types.Organization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the remote IP address of the connection.
--
-- /See:/ 'newRemoteIpDetails' smart constructor.
data RemoteIpDetails = RemoteIpDetails'
  { -- | The location information of the remote IP address.
    geoLocation :: Prelude.Maybe GeoLocation,
    -- | The city information of the remote IP address.
    city :: Prelude.Maybe City,
    -- | The ISP organization information of the remote IP address.
    organization :: Prelude.Maybe Organization,
    -- | The country code of the remote IP address.
    country :: Prelude.Maybe Country,
    -- | The IPv4 remote address of the connection.
    ipAddressV4 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoteIpDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoLocation', 'remoteIpDetails_geoLocation' - The location information of the remote IP address.
--
-- 'city', 'remoteIpDetails_city' - The city information of the remote IP address.
--
-- 'organization', 'remoteIpDetails_organization' - The ISP organization information of the remote IP address.
--
-- 'country', 'remoteIpDetails_country' - The country code of the remote IP address.
--
-- 'ipAddressV4', 'remoteIpDetails_ipAddressV4' - The IPv4 remote address of the connection.
newRemoteIpDetails ::
  RemoteIpDetails
newRemoteIpDetails =
  RemoteIpDetails'
    { geoLocation = Prelude.Nothing,
      city = Prelude.Nothing,
      organization = Prelude.Nothing,
      country = Prelude.Nothing,
      ipAddressV4 = Prelude.Nothing
    }

-- | The location information of the remote IP address.
remoteIpDetails_geoLocation :: Lens.Lens' RemoteIpDetails (Prelude.Maybe GeoLocation)
remoteIpDetails_geoLocation = Lens.lens (\RemoteIpDetails' {geoLocation} -> geoLocation) (\s@RemoteIpDetails' {} a -> s {geoLocation = a} :: RemoteIpDetails)

-- | The city information of the remote IP address.
remoteIpDetails_city :: Lens.Lens' RemoteIpDetails (Prelude.Maybe City)
remoteIpDetails_city = Lens.lens (\RemoteIpDetails' {city} -> city) (\s@RemoteIpDetails' {} a -> s {city = a} :: RemoteIpDetails)

-- | The ISP organization information of the remote IP address.
remoteIpDetails_organization :: Lens.Lens' RemoteIpDetails (Prelude.Maybe Organization)
remoteIpDetails_organization = Lens.lens (\RemoteIpDetails' {organization} -> organization) (\s@RemoteIpDetails' {} a -> s {organization = a} :: RemoteIpDetails)

-- | The country code of the remote IP address.
remoteIpDetails_country :: Lens.Lens' RemoteIpDetails (Prelude.Maybe Country)
remoteIpDetails_country = Lens.lens (\RemoteIpDetails' {country} -> country) (\s@RemoteIpDetails' {} a -> s {country = a} :: RemoteIpDetails)

-- | The IPv4 remote address of the connection.
remoteIpDetails_ipAddressV4 :: Lens.Lens' RemoteIpDetails (Prelude.Maybe Prelude.Text)
remoteIpDetails_ipAddressV4 = Lens.lens (\RemoteIpDetails' {ipAddressV4} -> ipAddressV4) (\s@RemoteIpDetails' {} a -> s {ipAddressV4 = a} :: RemoteIpDetails)

instance Prelude.FromJSON RemoteIpDetails where
  parseJSON =
    Prelude.withObject
      "RemoteIpDetails"
      ( \x ->
          RemoteIpDetails'
            Prelude.<$> (x Prelude..:? "geoLocation")
            Prelude.<*> (x Prelude..:? "city")
            Prelude.<*> (x Prelude..:? "organization")
            Prelude.<*> (x Prelude..:? "country")
            Prelude.<*> (x Prelude..:? "ipAddressV4")
      )

instance Prelude.Hashable RemoteIpDetails

instance Prelude.NFData RemoteIpDetails
