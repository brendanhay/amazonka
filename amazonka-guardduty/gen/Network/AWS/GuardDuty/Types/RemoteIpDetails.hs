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

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.City
import Network.AWS.GuardDuty.Types.Country
import Network.AWS.GuardDuty.Types.GeoLocation
import Network.AWS.GuardDuty.Types.Organization
import qualified Network.AWS.Lens as Lens

-- | Contains information about the remote IP address of the connection.
--
-- /See:/ 'newRemoteIpDetails' smart constructor.
data RemoteIpDetails = RemoteIpDetails'
  { -- | The location information of the remote IP address.
    geoLocation :: Core.Maybe GeoLocation,
    -- | The city information of the remote IP address.
    city :: Core.Maybe City,
    -- | The ISP organization information of the remote IP address.
    organization :: Core.Maybe Organization,
    -- | The country code of the remote IP address.
    country :: Core.Maybe Country,
    -- | The IPv4 remote address of the connection.
    ipAddressV4 :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { geoLocation = Core.Nothing,
      city = Core.Nothing,
      organization = Core.Nothing,
      country = Core.Nothing,
      ipAddressV4 = Core.Nothing
    }

-- | The location information of the remote IP address.
remoteIpDetails_geoLocation :: Lens.Lens' RemoteIpDetails (Core.Maybe GeoLocation)
remoteIpDetails_geoLocation = Lens.lens (\RemoteIpDetails' {geoLocation} -> geoLocation) (\s@RemoteIpDetails' {} a -> s {geoLocation = a} :: RemoteIpDetails)

-- | The city information of the remote IP address.
remoteIpDetails_city :: Lens.Lens' RemoteIpDetails (Core.Maybe City)
remoteIpDetails_city = Lens.lens (\RemoteIpDetails' {city} -> city) (\s@RemoteIpDetails' {} a -> s {city = a} :: RemoteIpDetails)

-- | The ISP organization information of the remote IP address.
remoteIpDetails_organization :: Lens.Lens' RemoteIpDetails (Core.Maybe Organization)
remoteIpDetails_organization = Lens.lens (\RemoteIpDetails' {organization} -> organization) (\s@RemoteIpDetails' {} a -> s {organization = a} :: RemoteIpDetails)

-- | The country code of the remote IP address.
remoteIpDetails_country :: Lens.Lens' RemoteIpDetails (Core.Maybe Country)
remoteIpDetails_country = Lens.lens (\RemoteIpDetails' {country} -> country) (\s@RemoteIpDetails' {} a -> s {country = a} :: RemoteIpDetails)

-- | The IPv4 remote address of the connection.
remoteIpDetails_ipAddressV4 :: Lens.Lens' RemoteIpDetails (Core.Maybe Core.Text)
remoteIpDetails_ipAddressV4 = Lens.lens (\RemoteIpDetails' {ipAddressV4} -> ipAddressV4) (\s@RemoteIpDetails' {} a -> s {ipAddressV4 = a} :: RemoteIpDetails)

instance Core.FromJSON RemoteIpDetails where
  parseJSON =
    Core.withObject
      "RemoteIpDetails"
      ( \x ->
          RemoteIpDetails'
            Core.<$> (x Core..:? "geoLocation")
            Core.<*> (x Core..:? "city")
            Core.<*> (x Core..:? "organization")
            Core.<*> (x Core..:? "country")
            Core.<*> (x Core..:? "ipAddressV4")
      )

instance Core.Hashable RemoteIpDetails

instance Core.NFData RemoteIpDetails
