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
-- Module      : Amazonka.MacieV2.Types.IpAddressDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.IpAddressDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.IpCity
import Amazonka.MacieV2.Types.IpCountry
import Amazonka.MacieV2.Types.IpGeoLocation
import Amazonka.MacieV2.Types.IpOwner
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the IP address of the device that an entity
-- used to perform an action on an affected resource.
--
-- /See:/ 'newIpAddressDetails' smart constructor.
data IpAddressDetails = IpAddressDetails'
  { -- | The country that the IP address originated from.
    ipCountry :: Prelude.Maybe IpCountry,
    -- | The Internet Protocol version 4 (IPv4) address of the device.
    ipAddressV4 :: Prelude.Maybe Prelude.Text,
    -- | The registered owner of the IP address.
    ipOwner :: Prelude.Maybe IpOwner,
    -- | The geographic coordinates of the location that the IP address
    -- originated from.
    ipGeoLocation :: Prelude.Maybe IpGeoLocation,
    -- | The city that the IP address originated from.
    ipCity :: Prelude.Maybe IpCity
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
-- 'ipCountry', 'ipAddressDetails_ipCountry' - The country that the IP address originated from.
--
-- 'ipAddressV4', 'ipAddressDetails_ipAddressV4' - The Internet Protocol version 4 (IPv4) address of the device.
--
-- 'ipOwner', 'ipAddressDetails_ipOwner' - The registered owner of the IP address.
--
-- 'ipGeoLocation', 'ipAddressDetails_ipGeoLocation' - The geographic coordinates of the location that the IP address
-- originated from.
--
-- 'ipCity', 'ipAddressDetails_ipCity' - The city that the IP address originated from.
newIpAddressDetails ::
  IpAddressDetails
newIpAddressDetails =
  IpAddressDetails'
    { ipCountry = Prelude.Nothing,
      ipAddressV4 = Prelude.Nothing,
      ipOwner = Prelude.Nothing,
      ipGeoLocation = Prelude.Nothing,
      ipCity = Prelude.Nothing
    }

-- | The country that the IP address originated from.
ipAddressDetails_ipCountry :: Lens.Lens' IpAddressDetails (Prelude.Maybe IpCountry)
ipAddressDetails_ipCountry = Lens.lens (\IpAddressDetails' {ipCountry} -> ipCountry) (\s@IpAddressDetails' {} a -> s {ipCountry = a} :: IpAddressDetails)

-- | The Internet Protocol version 4 (IPv4) address of the device.
ipAddressDetails_ipAddressV4 :: Lens.Lens' IpAddressDetails (Prelude.Maybe Prelude.Text)
ipAddressDetails_ipAddressV4 = Lens.lens (\IpAddressDetails' {ipAddressV4} -> ipAddressV4) (\s@IpAddressDetails' {} a -> s {ipAddressV4 = a} :: IpAddressDetails)

-- | The registered owner of the IP address.
ipAddressDetails_ipOwner :: Lens.Lens' IpAddressDetails (Prelude.Maybe IpOwner)
ipAddressDetails_ipOwner = Lens.lens (\IpAddressDetails' {ipOwner} -> ipOwner) (\s@IpAddressDetails' {} a -> s {ipOwner = a} :: IpAddressDetails)

-- | The geographic coordinates of the location that the IP address
-- originated from.
ipAddressDetails_ipGeoLocation :: Lens.Lens' IpAddressDetails (Prelude.Maybe IpGeoLocation)
ipAddressDetails_ipGeoLocation = Lens.lens (\IpAddressDetails' {ipGeoLocation} -> ipGeoLocation) (\s@IpAddressDetails' {} a -> s {ipGeoLocation = a} :: IpAddressDetails)

-- | The city that the IP address originated from.
ipAddressDetails_ipCity :: Lens.Lens' IpAddressDetails (Prelude.Maybe IpCity)
ipAddressDetails_ipCity = Lens.lens (\IpAddressDetails' {ipCity} -> ipCity) (\s@IpAddressDetails' {} a -> s {ipCity = a} :: IpAddressDetails)

instance Data.FromJSON IpAddressDetails where
  parseJSON =
    Data.withObject
      "IpAddressDetails"
      ( \x ->
          IpAddressDetails'
            Prelude.<$> (x Data..:? "ipCountry")
            Prelude.<*> (x Data..:? "ipAddressV4")
            Prelude.<*> (x Data..:? "ipOwner")
            Prelude.<*> (x Data..:? "ipGeoLocation")
            Prelude.<*> (x Data..:? "ipCity")
      )

instance Prelude.Hashable IpAddressDetails where
  hashWithSalt _salt IpAddressDetails' {..} =
    _salt `Prelude.hashWithSalt` ipCountry
      `Prelude.hashWithSalt` ipAddressV4
      `Prelude.hashWithSalt` ipOwner
      `Prelude.hashWithSalt` ipGeoLocation
      `Prelude.hashWithSalt` ipCity

instance Prelude.NFData IpAddressDetails where
  rnf IpAddressDetails' {..} =
    Prelude.rnf ipCountry
      `Prelude.seq` Prelude.rnf ipAddressV4
      `Prelude.seq` Prelude.rnf ipOwner
      `Prelude.seq` Prelude.rnf ipGeoLocation
      `Prelude.seq` Prelude.rnf ipCity
