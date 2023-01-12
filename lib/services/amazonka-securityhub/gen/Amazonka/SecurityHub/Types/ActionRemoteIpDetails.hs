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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ActionRemoteIpDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The city where the remote IP address is located.
    city :: Prelude.Maybe City,
    -- | The country where the remote IP address is located.
    country :: Prelude.Maybe Country,
    -- | The coordinates of the location of the remote IP address.
    geoLocation :: Prelude.Maybe GeoLocation,
    -- | The IP address.
    ipAddressV4 :: Prelude.Maybe Prelude.Text,
    -- | The internet service provider (ISP) organization associated with the
    -- remote IP address.
    organization :: Prelude.Maybe IpOrganizationDetails
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
-- 'city', 'actionRemoteIpDetails_city' - The city where the remote IP address is located.
--
-- 'country', 'actionRemoteIpDetails_country' - The country where the remote IP address is located.
--
-- 'geoLocation', 'actionRemoteIpDetails_geoLocation' - The coordinates of the location of the remote IP address.
--
-- 'ipAddressV4', 'actionRemoteIpDetails_ipAddressV4' - The IP address.
--
-- 'organization', 'actionRemoteIpDetails_organization' - The internet service provider (ISP) organization associated with the
-- remote IP address.
newActionRemoteIpDetails ::
  ActionRemoteIpDetails
newActionRemoteIpDetails =
  ActionRemoteIpDetails'
    { city = Prelude.Nothing,
      country = Prelude.Nothing,
      geoLocation = Prelude.Nothing,
      ipAddressV4 = Prelude.Nothing,
      organization = Prelude.Nothing
    }

-- | The city where the remote IP address is located.
actionRemoteIpDetails_city :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe City)
actionRemoteIpDetails_city = Lens.lens (\ActionRemoteIpDetails' {city} -> city) (\s@ActionRemoteIpDetails' {} a -> s {city = a} :: ActionRemoteIpDetails)

-- | The country where the remote IP address is located.
actionRemoteIpDetails_country :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe Country)
actionRemoteIpDetails_country = Lens.lens (\ActionRemoteIpDetails' {country} -> country) (\s@ActionRemoteIpDetails' {} a -> s {country = a} :: ActionRemoteIpDetails)

-- | The coordinates of the location of the remote IP address.
actionRemoteIpDetails_geoLocation :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe GeoLocation)
actionRemoteIpDetails_geoLocation = Lens.lens (\ActionRemoteIpDetails' {geoLocation} -> geoLocation) (\s@ActionRemoteIpDetails' {} a -> s {geoLocation = a} :: ActionRemoteIpDetails)

-- | The IP address.
actionRemoteIpDetails_ipAddressV4 :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe Prelude.Text)
actionRemoteIpDetails_ipAddressV4 = Lens.lens (\ActionRemoteIpDetails' {ipAddressV4} -> ipAddressV4) (\s@ActionRemoteIpDetails' {} a -> s {ipAddressV4 = a} :: ActionRemoteIpDetails)

-- | The internet service provider (ISP) organization associated with the
-- remote IP address.
actionRemoteIpDetails_organization :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe IpOrganizationDetails)
actionRemoteIpDetails_organization = Lens.lens (\ActionRemoteIpDetails' {organization} -> organization) (\s@ActionRemoteIpDetails' {} a -> s {organization = a} :: ActionRemoteIpDetails)

instance Data.FromJSON ActionRemoteIpDetails where
  parseJSON =
    Data.withObject
      "ActionRemoteIpDetails"
      ( \x ->
          ActionRemoteIpDetails'
            Prelude.<$> (x Data..:? "City")
            Prelude.<*> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "GeoLocation")
            Prelude.<*> (x Data..:? "IpAddressV4")
            Prelude.<*> (x Data..:? "Organization")
      )

instance Prelude.Hashable ActionRemoteIpDetails where
  hashWithSalt _salt ActionRemoteIpDetails' {..} =
    _salt `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` geoLocation
      `Prelude.hashWithSalt` ipAddressV4
      `Prelude.hashWithSalt` organization

instance Prelude.NFData ActionRemoteIpDetails where
  rnf ActionRemoteIpDetails' {..} =
    Prelude.rnf city
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf geoLocation
      `Prelude.seq` Prelude.rnf ipAddressV4
      `Prelude.seq` Prelude.rnf organization

instance Data.ToJSON ActionRemoteIpDetails where
  toJSON ActionRemoteIpDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("City" Data..=) Prelude.<$> city,
            ("Country" Data..=) Prelude.<$> country,
            ("GeoLocation" Data..=) Prelude.<$> geoLocation,
            ("IpAddressV4" Data..=) Prelude.<$> ipAddressV4,
            ("Organization" Data..=) Prelude.<$> organization
          ]
      )
