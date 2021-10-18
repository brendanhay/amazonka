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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventContextDataType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the user context data captured at the time of an event
-- request.
--
-- /See:/ 'newEventContextDataType' smart constructor.
data EventContextDataType = EventContextDataType'
  { -- | The user\'s device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The user\'s city.
    city :: Prelude.Maybe Prelude.Text,
    -- | The user\'s IP address.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The user\'s time zone.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The user\'s country.
    country :: Prelude.Maybe Prelude.Text
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
-- 'deviceName', 'eventContextDataType_deviceName' - The user\'s device name.
--
-- 'city', 'eventContextDataType_city' - The user\'s city.
--
-- 'ipAddress', 'eventContextDataType_ipAddress' - The user\'s IP address.
--
-- 'timezone', 'eventContextDataType_timezone' - The user\'s time zone.
--
-- 'country', 'eventContextDataType_country' - The user\'s country.
newEventContextDataType ::
  EventContextDataType
newEventContextDataType =
  EventContextDataType'
    { deviceName = Prelude.Nothing,
      city = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      timezone = Prelude.Nothing,
      country = Prelude.Nothing
    }

-- | The user\'s device name.
eventContextDataType_deviceName :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_deviceName = Lens.lens (\EventContextDataType' {deviceName} -> deviceName) (\s@EventContextDataType' {} a -> s {deviceName = a} :: EventContextDataType)

-- | The user\'s city.
eventContextDataType_city :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_city = Lens.lens (\EventContextDataType' {city} -> city) (\s@EventContextDataType' {} a -> s {city = a} :: EventContextDataType)

-- | The user\'s IP address.
eventContextDataType_ipAddress :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_ipAddress = Lens.lens (\EventContextDataType' {ipAddress} -> ipAddress) (\s@EventContextDataType' {} a -> s {ipAddress = a} :: EventContextDataType)

-- | The user\'s time zone.
eventContextDataType_timezone :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_timezone = Lens.lens (\EventContextDataType' {timezone} -> timezone) (\s@EventContextDataType' {} a -> s {timezone = a} :: EventContextDataType)

-- | The user\'s country.
eventContextDataType_country :: Lens.Lens' EventContextDataType (Prelude.Maybe Prelude.Text)
eventContextDataType_country = Lens.lens (\EventContextDataType' {country} -> country) (\s@EventContextDataType' {} a -> s {country = a} :: EventContextDataType)

instance Core.FromJSON EventContextDataType where
  parseJSON =
    Core.withObject
      "EventContextDataType"
      ( \x ->
          EventContextDataType'
            Prelude.<$> (x Core..:? "DeviceName")
            Prelude.<*> (x Core..:? "City")
            Prelude.<*> (x Core..:? "IpAddress")
            Prelude.<*> (x Core..:? "Timezone")
            Prelude.<*> (x Core..:? "Country")
      )

instance Prelude.Hashable EventContextDataType

instance Prelude.NFData EventContextDataType
