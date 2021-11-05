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
-- Module      : Network.AWS.MacieV2.Types.IpAddressDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.IpAddressDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.IpCity
import Network.AWS.MacieV2.Types.IpCountry
import Network.AWS.MacieV2.Types.IpGeoLocation
import Network.AWS.MacieV2.Types.IpOwner
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the IP address of the device that an entity
-- used to perform an action on an affected resource.
--
-- /See:/ 'newIpAddressDetails' smart constructor.
data IpAddressDetails = IpAddressDetails'
  { -- | The city that the IP address originated from.
    ipCity :: Prelude.Maybe IpCity,
    -- | The geographic coordinates of the location that the IP address
    -- originated from.
    ipGeoLocation :: Prelude.Maybe IpGeoLocation,
    -- | The Internet Protocol version 4 (IPv4) address of the device.
    ipAddressV4 :: Prelude.Maybe Prelude.Text,
    -- | The registered owner of the IP address.
    ipOwner :: Prelude.Maybe IpOwner,
    -- | The country that the IP address originated from.
    ipCountry :: Prelude.Maybe IpCountry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpAddressDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipCity', 'ipAddressDetails_ipCity' - The city that the IP address originated from.
--
-- 'ipGeoLocation', 'ipAddressDetails_ipGeoLocation' - The geographic coordinates of the location that the IP address
-- originated from.
--
-- 'ipAddressV4', 'ipAddressDetails_ipAddressV4' - The Internet Protocol version 4 (IPv4) address of the device.
--
-- 'ipOwner', 'ipAddressDetails_ipOwner' - The registered owner of the IP address.
--
-- 'ipCountry', 'ipAddressDetails_ipCountry' - The country that the IP address originated from.
newIpAddressDetails ::
  IpAddressDetails
newIpAddressDetails =
  IpAddressDetails'
    { ipCity = Prelude.Nothing,
      ipGeoLocation = Prelude.Nothing,
      ipAddressV4 = Prelude.Nothing,
      ipOwner = Prelude.Nothing,
      ipCountry = Prelude.Nothing
    }

-- | The city that the IP address originated from.
ipAddressDetails_ipCity :: Lens.Lens' IpAddressDetails (Prelude.Maybe IpCity)
ipAddressDetails_ipCity = Lens.lens (\IpAddressDetails' {ipCity} -> ipCity) (\s@IpAddressDetails' {} a -> s {ipCity = a} :: IpAddressDetails)

-- | The geographic coordinates of the location that the IP address
-- originated from.
ipAddressDetails_ipGeoLocation :: Lens.Lens' IpAddressDetails (Prelude.Maybe IpGeoLocation)
ipAddressDetails_ipGeoLocation = Lens.lens (\IpAddressDetails' {ipGeoLocation} -> ipGeoLocation) (\s@IpAddressDetails' {} a -> s {ipGeoLocation = a} :: IpAddressDetails)

-- | The Internet Protocol version 4 (IPv4) address of the device.
ipAddressDetails_ipAddressV4 :: Lens.Lens' IpAddressDetails (Prelude.Maybe Prelude.Text)
ipAddressDetails_ipAddressV4 = Lens.lens (\IpAddressDetails' {ipAddressV4} -> ipAddressV4) (\s@IpAddressDetails' {} a -> s {ipAddressV4 = a} :: IpAddressDetails)

-- | The registered owner of the IP address.
ipAddressDetails_ipOwner :: Lens.Lens' IpAddressDetails (Prelude.Maybe IpOwner)
ipAddressDetails_ipOwner = Lens.lens (\IpAddressDetails' {ipOwner} -> ipOwner) (\s@IpAddressDetails' {} a -> s {ipOwner = a} :: IpAddressDetails)

-- | The country that the IP address originated from.
ipAddressDetails_ipCountry :: Lens.Lens' IpAddressDetails (Prelude.Maybe IpCountry)
ipAddressDetails_ipCountry = Lens.lens (\IpAddressDetails' {ipCountry} -> ipCountry) (\s@IpAddressDetails' {} a -> s {ipCountry = a} :: IpAddressDetails)

instance Core.FromJSON IpAddressDetails where
  parseJSON =
    Core.withObject
      "IpAddressDetails"
      ( \x ->
          IpAddressDetails'
            Prelude.<$> (x Core..:? "ipCity")
            Prelude.<*> (x Core..:? "ipGeoLocation")
            Prelude.<*> (x Core..:? "ipAddressV4")
            Prelude.<*> (x Core..:? "ipOwner")
            Prelude.<*> (x Core..:? "ipCountry")
      )

instance Prelude.Hashable IpAddressDetails

instance Prelude.NFData IpAddressDetails
