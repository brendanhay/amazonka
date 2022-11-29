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
-- Module      : Amazonka.AlexaBusiness.Types.ProfileData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.ProfileData where

import Amazonka.AlexaBusiness.Types.DistanceUnit
import Amazonka.AlexaBusiness.Types.TemperatureUnit
import Amazonka.AlexaBusiness.Types.WakeWord
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The data of a room profile.
--
-- /See:/ 'newProfileData' smart constructor.
data ProfileData = ProfileData'
  { -- | The distance unit of a room profile.
    distanceUnit :: Prelude.Maybe DistanceUnit,
    -- | The name of a room profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The wake word of a room profile.
    wakeWord :: Prelude.Maybe WakeWord,
    -- | The locale of a room profile. (This is currently available only to a
    -- limited preview audience.)
    locale :: Prelude.Maybe Prelude.Text,
    -- | The time zone of a room profile.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a room profile.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | Retrieves if the profile data is default or not.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The address of a room profile.
    address :: Prelude.Maybe Prelude.Text,
    -- | The temperature unit of a room profile.
    temperatureUnit :: Prelude.Maybe TemperatureUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distanceUnit', 'profileData_distanceUnit' - The distance unit of a room profile.
--
-- 'profileName', 'profileData_profileName' - The name of a room profile.
--
-- 'wakeWord', 'profileData_wakeWord' - The wake word of a room profile.
--
-- 'locale', 'profileData_locale' - The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
--
-- 'timezone', 'profileData_timezone' - The time zone of a room profile.
--
-- 'profileArn', 'profileData_profileArn' - The ARN of a room profile.
--
-- 'isDefault', 'profileData_isDefault' - Retrieves if the profile data is default or not.
--
-- 'address', 'profileData_address' - The address of a room profile.
--
-- 'temperatureUnit', 'profileData_temperatureUnit' - The temperature unit of a room profile.
newProfileData ::
  ProfileData
newProfileData =
  ProfileData'
    { distanceUnit = Prelude.Nothing,
      profileName = Prelude.Nothing,
      wakeWord = Prelude.Nothing,
      locale = Prelude.Nothing,
      timezone = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      address = Prelude.Nothing,
      temperatureUnit = Prelude.Nothing
    }

-- | The distance unit of a room profile.
profileData_distanceUnit :: Lens.Lens' ProfileData (Prelude.Maybe DistanceUnit)
profileData_distanceUnit = Lens.lens (\ProfileData' {distanceUnit} -> distanceUnit) (\s@ProfileData' {} a -> s {distanceUnit = a} :: ProfileData)

-- | The name of a room profile.
profileData_profileName :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_profileName = Lens.lens (\ProfileData' {profileName} -> profileName) (\s@ProfileData' {} a -> s {profileName = a} :: ProfileData)

-- | The wake word of a room profile.
profileData_wakeWord :: Lens.Lens' ProfileData (Prelude.Maybe WakeWord)
profileData_wakeWord = Lens.lens (\ProfileData' {wakeWord} -> wakeWord) (\s@ProfileData' {} a -> s {wakeWord = a} :: ProfileData)

-- | The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
profileData_locale :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_locale = Lens.lens (\ProfileData' {locale} -> locale) (\s@ProfileData' {} a -> s {locale = a} :: ProfileData)

-- | The time zone of a room profile.
profileData_timezone :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_timezone = Lens.lens (\ProfileData' {timezone} -> timezone) (\s@ProfileData' {} a -> s {timezone = a} :: ProfileData)

-- | The ARN of a room profile.
profileData_profileArn :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_profileArn = Lens.lens (\ProfileData' {profileArn} -> profileArn) (\s@ProfileData' {} a -> s {profileArn = a} :: ProfileData)

-- | Retrieves if the profile data is default or not.
profileData_isDefault :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Bool)
profileData_isDefault = Lens.lens (\ProfileData' {isDefault} -> isDefault) (\s@ProfileData' {} a -> s {isDefault = a} :: ProfileData)

-- | The address of a room profile.
profileData_address :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_address = Lens.lens (\ProfileData' {address} -> address) (\s@ProfileData' {} a -> s {address = a} :: ProfileData)

-- | The temperature unit of a room profile.
profileData_temperatureUnit :: Lens.Lens' ProfileData (Prelude.Maybe TemperatureUnit)
profileData_temperatureUnit = Lens.lens (\ProfileData' {temperatureUnit} -> temperatureUnit) (\s@ProfileData' {} a -> s {temperatureUnit = a} :: ProfileData)

instance Core.FromJSON ProfileData where
  parseJSON =
    Core.withObject
      "ProfileData"
      ( \x ->
          ProfileData'
            Prelude.<$> (x Core..:? "DistanceUnit")
            Prelude.<*> (x Core..:? "ProfileName")
            Prelude.<*> (x Core..:? "WakeWord")
            Prelude.<*> (x Core..:? "Locale")
            Prelude.<*> (x Core..:? "Timezone")
            Prelude.<*> (x Core..:? "ProfileArn")
            Prelude.<*> (x Core..:? "IsDefault")
            Prelude.<*> (x Core..:? "Address")
            Prelude.<*> (x Core..:? "TemperatureUnit")
      )

instance Prelude.Hashable ProfileData where
  hashWithSalt _salt ProfileData' {..} =
    _salt `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` wakeWord
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` temperatureUnit

instance Prelude.NFData ProfileData where
  rnf ProfileData' {..} =
    Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf wakeWord
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf address
      `Prelude.seq` Prelude.rnf temperatureUnit
