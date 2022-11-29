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
-- Module      : Amazonka.SecurityHub.Types.ActionRemoteIpDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ActionRemoteIpDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.City
import Amazonka.SecurityHub.Types.Country
import Amazonka.SecurityHub.Types.GeoLocation
import Amazonka.SecurityHub.Types.IpOrganizationDetails

-- | For @AwsApiAction@, @NetworkConnectionAction@, and @PortProbeAction@,
-- @RemoteIpDetails@ provides information about the remote IP address that
-- was involved in the action.
--
-- /See:/ 'newActionRemoteIpDetails' smart constructor.
data ActionRemoteIpDetails = ActionRemoteIpDetails'
  { -- | The country where the remote IP address is located.
    country :: Prelude.Maybe Country,
    -- | The IP address.
    ipAddressV4 :: Prelude.Maybe Prelude.Text,
    -- | The city where the remote IP address is located.
    city :: Prelude.Maybe City,
    -- | The internet service provider (ISP) organization associated with the
    -- remote IP address.
    organization :: Prelude.Maybe IpOrganizationDetails,
    -- | The coordinates of the location of the remote IP address.
    geoLocation :: Prelude.Maybe GeoLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionRemoteIpDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'country', 'actionRemoteIpDetails_country' - The country where the remote IP address is located.
--
-- 'ipAddressV4', 'actionRemoteIpDetails_ipAddressV4' - The IP address.
--
-- 'city', 'actionRemoteIpDetails_city' - The city where the remote IP address is located.
--
-- 'organization', 'actionRemoteIpDetails_organization' - The internet service provider (ISP) organization associated with the
-- remote IP address.
--
-- 'geoLocation', 'actionRemoteIpDetails_geoLocation' - The coordinates of the location of the remote IP address.
newActionRemoteIpDetails ::
  ActionRemoteIpDetails
newActionRemoteIpDetails =
  ActionRemoteIpDetails'
    { country = Prelude.Nothing,
      ipAddressV4 = Prelude.Nothing,
      city = Prelude.Nothing,
      organization = Prelude.Nothing,
      geoLocation = Prelude.Nothing
    }

-- | The country where the remote IP address is located.
actionRemoteIpDetails_country :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe Country)
actionRemoteIpDetails_country = Lens.lens (\ActionRemoteIpDetails' {country} -> country) (\s@ActionRemoteIpDetails' {} a -> s {country = a} :: ActionRemoteIpDetails)

-- | The IP address.
actionRemoteIpDetails_ipAddressV4 :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe Prelude.Text)
actionRemoteIpDetails_ipAddressV4 = Lens.lens (\ActionRemoteIpDetails' {ipAddressV4} -> ipAddressV4) (\s@ActionRemoteIpDetails' {} a -> s {ipAddressV4 = a} :: ActionRemoteIpDetails)

-- | The city where the remote IP address is located.
actionRemoteIpDetails_city :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe City)
actionRemoteIpDetails_city = Lens.lens (\ActionRemoteIpDetails' {city} -> city) (\s@ActionRemoteIpDetails' {} a -> s {city = a} :: ActionRemoteIpDetails)

-- | The internet service provider (ISP) organization associated with the
-- remote IP address.
actionRemoteIpDetails_organization :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe IpOrganizationDetails)
actionRemoteIpDetails_organization = Lens.lens (\ActionRemoteIpDetails' {organization} -> organization) (\s@ActionRemoteIpDetails' {} a -> s {organization = a} :: ActionRemoteIpDetails)

-- | The coordinates of the location of the remote IP address.
actionRemoteIpDetails_geoLocation :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe GeoLocation)
actionRemoteIpDetails_geoLocation = Lens.lens (\ActionRemoteIpDetails' {geoLocation} -> geoLocation) (\s@ActionRemoteIpDetails' {} a -> s {geoLocation = a} :: ActionRemoteIpDetails)

instance Core.FromJSON ActionRemoteIpDetails where
  parseJSON =
    Core.withObject
      "ActionRemoteIpDetails"
      ( \x ->
          ActionRemoteIpDetails'
            Prelude.<$> (x Core..:? "Country")
            Prelude.<*> (x Core..:? "IpAddressV4")
            Prelude.<*> (x Core..:? "City")
            Prelude.<*> (x Core..:? "Organization")
            Prelude.<*> (x Core..:? "GeoLocation")
      )

instance Prelude.Hashable ActionRemoteIpDetails where
  hashWithSalt _salt ActionRemoteIpDetails' {..} =
    _salt `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` ipAddressV4
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` organization
      `Prelude.hashWithSalt` geoLocation

instance Prelude.NFData ActionRemoteIpDetails where
  rnf ActionRemoteIpDetails' {..} =
    Prelude.rnf country
      `Prelude.seq` Prelude.rnf ipAddressV4
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf organization
      `Prelude.seq` Prelude.rnf geoLocation

instance Core.ToJSON ActionRemoteIpDetails where
  toJSON ActionRemoteIpDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Country" Core..=) Prelude.<$> country,
            ("IpAddressV4" Core..=) Prelude.<$> ipAddressV4,
            ("City" Core..=) Prelude.<$> city,
            ("Organization" Core..=) Prelude.<$> organization,
            ("GeoLocation" Core..=) Prelude.<$> geoLocation
          ]
      )
