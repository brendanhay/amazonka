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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ActionRemoteIpDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    -- | The city where the remote IP address is located.
    city :: Prelude.Maybe City,
    -- | The IP address.
    ipAddressV4 :: Prelude.Maybe Prelude.Text,
    -- | The coordinates of the location of the remote IP address.
    geoLocation :: Prelude.Maybe GeoLocation,
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
-- 'country', 'actionRemoteIpDetails_country' - The country where the remote IP address is located.
--
-- 'city', 'actionRemoteIpDetails_city' - The city where the remote IP address is located.
--
-- 'ipAddressV4', 'actionRemoteIpDetails_ipAddressV4' - The IP address.
--
-- 'geoLocation', 'actionRemoteIpDetails_geoLocation' - The coordinates of the location of the remote IP address.
--
-- 'organization', 'actionRemoteIpDetails_organization' - The internet service provider (ISP) organization associated with the
-- remote IP address.
newActionRemoteIpDetails ::
  ActionRemoteIpDetails
newActionRemoteIpDetails =
  ActionRemoteIpDetails'
    { country = Prelude.Nothing,
      city = Prelude.Nothing,
      ipAddressV4 = Prelude.Nothing,
      geoLocation = Prelude.Nothing,
      organization = Prelude.Nothing
    }

-- | The country where the remote IP address is located.
actionRemoteIpDetails_country :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe Country)
actionRemoteIpDetails_country = Lens.lens (\ActionRemoteIpDetails' {country} -> country) (\s@ActionRemoteIpDetails' {} a -> s {country = a} :: ActionRemoteIpDetails)

-- | The city where the remote IP address is located.
actionRemoteIpDetails_city :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe City)
actionRemoteIpDetails_city = Lens.lens (\ActionRemoteIpDetails' {city} -> city) (\s@ActionRemoteIpDetails' {} a -> s {city = a} :: ActionRemoteIpDetails)

-- | The IP address.
actionRemoteIpDetails_ipAddressV4 :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe Prelude.Text)
actionRemoteIpDetails_ipAddressV4 = Lens.lens (\ActionRemoteIpDetails' {ipAddressV4} -> ipAddressV4) (\s@ActionRemoteIpDetails' {} a -> s {ipAddressV4 = a} :: ActionRemoteIpDetails)

-- | The coordinates of the location of the remote IP address.
actionRemoteIpDetails_geoLocation :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe GeoLocation)
actionRemoteIpDetails_geoLocation = Lens.lens (\ActionRemoteIpDetails' {geoLocation} -> geoLocation) (\s@ActionRemoteIpDetails' {} a -> s {geoLocation = a} :: ActionRemoteIpDetails)

-- | The internet service provider (ISP) organization associated with the
-- remote IP address.
actionRemoteIpDetails_organization :: Lens.Lens' ActionRemoteIpDetails (Prelude.Maybe IpOrganizationDetails)
actionRemoteIpDetails_organization = Lens.lens (\ActionRemoteIpDetails' {organization} -> organization) (\s@ActionRemoteIpDetails' {} a -> s {organization = a} :: ActionRemoteIpDetails)

instance Core.FromJSON ActionRemoteIpDetails where
  parseJSON =
    Core.withObject
      "ActionRemoteIpDetails"
      ( \x ->
          ActionRemoteIpDetails'
            Prelude.<$> (x Core..:? "Country")
            Prelude.<*> (x Core..:? "City")
            Prelude.<*> (x Core..:? "IpAddressV4")
            Prelude.<*> (x Core..:? "GeoLocation")
            Prelude.<*> (x Core..:? "Organization")
      )

instance Prelude.Hashable ActionRemoteIpDetails

instance Prelude.NFData ActionRemoteIpDetails

instance Core.ToJSON ActionRemoteIpDetails where
  toJSON ActionRemoteIpDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Country" Core..=) Prelude.<$> country,
            ("City" Core..=) Prelude.<$> city,
            ("IpAddressV4" Core..=) Prelude.<$> ipAddressV4,
            ("GeoLocation" Core..=) Prelude.<$> geoLocation,
            ("Organization" Core..=) Prelude.<$> organization
          ]
      )
