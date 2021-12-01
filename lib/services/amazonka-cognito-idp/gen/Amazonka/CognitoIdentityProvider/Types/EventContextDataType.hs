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
-- Module      : Amazonka.CognitoIdentityProvider.Types.EventContextDataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.EventContextDataType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the user context data captured at the time of an event
-- request.
--
-- /See:/ 'newEventContextDataType' smart constructor.
data EventContextDataType = EventContextDataType'
  { -- | The user\'s IP address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The user\'s country.
    country :: Prelude.Maybe Prelude.Text,
    -- | The user\'s city.
    city :: Prelude.Maybe Prelude.Text,
    -- | The user\'s device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The user\'s time zone.
    timezone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventContextDataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'eventContextDataType_ipAddress' - The user\'s IP address.
--
-- 'country', 'eventContextDataType_country' - The user\'s country.
--
-- 'city', 'eventContextDataType_city' - The user\'s city.
--
-- 'deviceName', 'eventContextDataType_deviceName' - The user\'s device name.
--
-- 'timezone', 'eventContextDataType_timezone' - The user\'s time zone.
newEventContextDataType ::
  EventContextDataType
newEventContextDataType =
  EventContextDataType'
    { ipAddress = Prelude.Nothing,
      country = Prelude.Nothing,
      city = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      timezone = Prelude.Nothing
    }

-- | The user\'s IP address.
eventContextDataType_ipAddress :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_ipAddress = Lens.lens (\EventContextDataType' {ipAddress} -> ipAddress) (\s@EventContextDataType' {} a -> s {ipAddress = a} :: EventContextDataType)

-- | The user\'s country.
eventContextDataType_country :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_country = Lens.lens (\EventContextDataType' {country} -> country) (\s@EventContextDataType' {} a -> s {country = a} :: EventContextDataType)

-- | The user\'s city.
eventContextDataType_city :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_city = Lens.lens (\EventContextDataType' {city} -> city) (\s@EventContextDataType' {} a -> s {city = a} :: EventContextDataType)

-- | The user\'s device name.
eventContextDataType_deviceName :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_deviceName = Lens.lens (\EventContextDataType' {deviceName} -> deviceName) (\s@EventContextDataType' {} a -> s {deviceName = a} :: EventContextDataType)

-- | The user\'s time zone.
eventContextDataType_timezone :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_timezone = Lens.lens (\EventContextDataType' {timezone} -> timezone) (\s@EventContextDataType' {} a -> s {timezone = a} :: EventContextDataType)

instance Core.FromJSON EventContextDataType where
  parseJSON =
    Core.withObject
      "EventContextDataType"
      ( \x ->
          EventContextDataType'
            Prelude.<$> (x Core..:? "IpAddress")
            Prelude.<*> (x Core..:? "Country")
            Prelude.<*> (x Core..:? "City")
            Prelude.<*> (x Core..:? "DeviceName")
            Prelude.<*> (x Core..:? "Timezone")
      )

instance Prelude.Hashable EventContextDataType where
  hashWithSalt salt' EventContextDataType' {..} =
    salt' `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData EventContextDataType where
  rnf EventContextDataType' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf country
