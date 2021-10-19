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
-- Module      : Network.AWS.AlexaBusiness.Types.ProfileData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ProfileData where

import Network.AWS.AlexaBusiness.Types.DistanceUnit
import Network.AWS.AlexaBusiness.Types.TemperatureUnit
import Network.AWS.AlexaBusiness.Types.WakeWord
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The data of a room profile.
--
-- /See:/ 'newProfileData' smart constructor.
data ProfileData = ProfileData'
  { -- | The distance unit of a room profile.
    distanceUnit :: Prelude.Maybe DistanceUnit,
    -- | The locale of a room profile. (This is currently available only to a
    -- limited preview audience.)
    locale :: Prelude.Maybe Prelude.Text,
    -- | The address of a room profile.
    address :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a room profile.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The wake word of a room profile.
    wakeWord :: Prelude.Maybe WakeWord,
    -- | The name of a room profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The temperature unit of a room profile.
    temperatureUnit :: Prelude.Maybe TemperatureUnit,
    -- | The time zone of a room profile.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | Retrieves if the profile data is default or not.
    isDefault :: Prelude.Maybe Prelude.Bool
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
-- 'locale', 'profileData_locale' - The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
--
-- 'address', 'profileData_address' - The address of a room profile.
--
-- 'profileArn', 'profileData_profileArn' - The ARN of a room profile.
--
-- 'wakeWord', 'profileData_wakeWord' - The wake word of a room profile.
--
-- 'profileName', 'profileData_profileName' - The name of a room profile.
--
-- 'temperatureUnit', 'profileData_temperatureUnit' - The temperature unit of a room profile.
--
-- 'timezone', 'profileData_timezone' - The time zone of a room profile.
--
-- 'isDefault', 'profileData_isDefault' - Retrieves if the profile data is default or not.
newProfileData ::
  ProfileData
newProfileData =
  ProfileData'
    { distanceUnit = Prelude.Nothing,
      locale = Prelude.Nothing,
      address = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      wakeWord = Prelude.Nothing,
      profileName = Prelude.Nothing,
      temperatureUnit = Prelude.Nothing,
      timezone = Prelude.Nothing,
      isDefault = Prelude.Nothing
    }

-- | The distance unit of a room profile.
profileData_distanceUnit :: Lens.Lens' ProfileData (Prelude.Maybe DistanceUnit)
profileData_distanceUnit = Lens.lens (\ProfileData' {distanceUnit} -> distanceUnit) (\s@ProfileData' {} a -> s {distanceUnit = a} :: ProfileData)

-- | The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
profileData_locale :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_locale = Lens.lens (\ProfileData' {locale} -> locale) (\s@ProfileData' {} a -> s {locale = a} :: ProfileData)

-- | The address of a room profile.
profileData_address :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_address = Lens.lens (\ProfileData' {address} -> address) (\s@ProfileData' {} a -> s {address = a} :: ProfileData)

-- | The ARN of a room profile.
profileData_profileArn :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_profileArn = Lens.lens (\ProfileData' {profileArn} -> profileArn) (\s@ProfileData' {} a -> s {profileArn = a} :: ProfileData)

-- | The wake word of a room profile.
profileData_wakeWord :: Lens.Lens' ProfileData (Prelude.Maybe WakeWord)
profileData_wakeWord = Lens.lens (\ProfileData' {wakeWord} -> wakeWord) (\s@ProfileData' {} a -> s {wakeWord = a} :: ProfileData)

-- | The name of a room profile.
profileData_profileName :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_profileName = Lens.lens (\ProfileData' {profileName} -> profileName) (\s@ProfileData' {} a -> s {profileName = a} :: ProfileData)

-- | The temperature unit of a room profile.
profileData_temperatureUnit :: Lens.Lens' ProfileData (Prelude.Maybe TemperatureUnit)
profileData_temperatureUnit = Lens.lens (\ProfileData' {temperatureUnit} -> temperatureUnit) (\s@ProfileData' {} a -> s {temperatureUnit = a} :: ProfileData)

-- | The time zone of a room profile.
profileData_timezone :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Text)
profileData_timezone = Lens.lens (\ProfileData' {timezone} -> timezone) (\s@ProfileData' {} a -> s {timezone = a} :: ProfileData)

-- | Retrieves if the profile data is default or not.
profileData_isDefault :: Lens.Lens' ProfileData (Prelude.Maybe Prelude.Bool)
profileData_isDefault = Lens.lens (\ProfileData' {isDefault} -> isDefault) (\s@ProfileData' {} a -> s {isDefault = a} :: ProfileData)

instance Core.FromJSON ProfileData where
  parseJSON =
    Core.withObject
      "ProfileData"
      ( \x ->
          ProfileData'
            Prelude.<$> (x Core..:? "DistanceUnit")
            Prelude.<*> (x Core..:? "Locale")
            Prelude.<*> (x Core..:? "Address")
            Prelude.<*> (x Core..:? "ProfileArn")
            Prelude.<*> (x Core..:? "WakeWord")
            Prelude.<*> (x Core..:? "ProfileName")
            Prelude.<*> (x Core..:? "TemperatureUnit")
            Prelude.<*> (x Core..:? "Timezone")
            Prelude.<*> (x Core..:? "IsDefault")
      )

instance Prelude.Hashable ProfileData

instance Prelude.NFData ProfileData
