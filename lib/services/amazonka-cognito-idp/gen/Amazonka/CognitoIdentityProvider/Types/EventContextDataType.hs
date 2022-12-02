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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.EventContextDataType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the user context data captured at the time of an event
-- request.
--
-- /See:/ 'newEventContextDataType' smart constructor.
data EventContextDataType = EventContextDataType'
  { -- | The user\'s country.
    country :: Prelude.Maybe Prelude.Text,
    -- | The user\'s device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The user\'s time zone.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The user\'s city.
    city :: Prelude.Maybe Prelude.Text,
    -- | The source IP address of your user\'s device.
    ipAddress :: Prelude.Maybe Prelude.Text
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
-- 'country', 'eventContextDataType_country' - The user\'s country.
--
-- 'deviceName', 'eventContextDataType_deviceName' - The user\'s device name.
--
-- 'timezone', 'eventContextDataType_timezone' - The user\'s time zone.
--
-- 'city', 'eventContextDataType_city' - The user\'s city.
--
-- 'ipAddress', 'eventContextDataType_ipAddress' - The source IP address of your user\'s device.
newEventContextDataType ::
  EventContextDataType
newEventContextDataType =
  EventContextDataType'
    { country = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      timezone = Prelude.Nothing,
      city = Prelude.Nothing,
      ipAddress = Prelude.Nothing
    }

-- | The user\'s country.
eventContextDataType_country :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_country = Lens.lens (\EventContextDataType' {country} -> country) (\s@EventContextDataType' {} a -> s {country = a} :: EventContextDataType)

-- | The user\'s device name.
eventContextDataType_deviceName :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_deviceName = Lens.lens (\EventContextDataType' {deviceName} -> deviceName) (\s@EventContextDataType' {} a -> s {deviceName = a} :: EventContextDataType)

-- | The user\'s time zone.
eventContextDataType_timezone :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_timezone = Lens.lens (\EventContextDataType' {timezone} -> timezone) (\s@EventContextDataType' {} a -> s {timezone = a} :: EventContextDataType)

-- | The user\'s city.
eventContextDataType_city :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_city = Lens.lens (\EventContextDataType' {city} -> city) (\s@EventContextDataType' {} a -> s {city = a} :: EventContextDataType)

-- | The source IP address of your user\'s device.
eventContextDataType_ipAddress :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_ipAddress = Lens.lens (\EventContextDataType' {ipAddress} -> ipAddress) (\s@EventContextDataType' {} a -> s {ipAddress = a} :: EventContextDataType)

instance Data.FromJSON EventContextDataType where
  parseJSON =
    Data.withObject
      "EventContextDataType"
      ( \x ->
          EventContextDataType'
            Prelude.<$> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "DeviceName")
            Prelude.<*> (x Data..:? "Timezone")
            Prelude.<*> (x Data..:? "City")
            Prelude.<*> (x Data..:? "IpAddress")
      )

instance Prelude.Hashable EventContextDataType where
  hashWithSalt _salt EventContextDataType' {..} =
    _salt `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData EventContextDataType where
  rnf EventContextDataType' {..} =
    Prelude.rnf country
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf ipAddress
