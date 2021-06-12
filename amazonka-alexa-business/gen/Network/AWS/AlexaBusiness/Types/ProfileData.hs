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

-- | The data of a room profile.
--
-- /See:/ 'newProfileData' smart constructor.
data ProfileData = ProfileData'
  { -- | The name of a room profile.
    profileName :: Core.Maybe Core.Text,
    -- | Retrieves if the profile data is default or not.
    isDefault :: Core.Maybe Core.Bool,
    -- | The address of a room profile.
    address :: Core.Maybe Core.Text,
    -- | The locale of a room profile. (This is currently available only to a
    -- limited preview audience.)
    locale :: Core.Maybe Core.Text,
    -- | The temperature unit of a room profile.
    temperatureUnit :: Core.Maybe TemperatureUnit,
    -- | The wake word of a room profile.
    wakeWord :: Core.Maybe WakeWord,
    -- | The ARN of a room profile.
    profileArn :: Core.Maybe Core.Text,
    -- | The time zone of a room profile.
    timezone :: Core.Maybe Core.Text,
    -- | The distance unit of a room profile.
    distanceUnit :: Core.Maybe DistanceUnit
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProfileData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileName', 'profileData_profileName' - The name of a room profile.
--
-- 'isDefault', 'profileData_isDefault' - Retrieves if the profile data is default or not.
--
-- 'address', 'profileData_address' - The address of a room profile.
--
-- 'locale', 'profileData_locale' - The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
--
-- 'temperatureUnit', 'profileData_temperatureUnit' - The temperature unit of a room profile.
--
-- 'wakeWord', 'profileData_wakeWord' - The wake word of a room profile.
--
-- 'profileArn', 'profileData_profileArn' - The ARN of a room profile.
--
-- 'timezone', 'profileData_timezone' - The time zone of a room profile.
--
-- 'distanceUnit', 'profileData_distanceUnit' - The distance unit of a room profile.
newProfileData ::
  ProfileData
newProfileData =
  ProfileData'
    { profileName = Core.Nothing,
      isDefault = Core.Nothing,
      address = Core.Nothing,
      locale = Core.Nothing,
      temperatureUnit = Core.Nothing,
      wakeWord = Core.Nothing,
      profileArn = Core.Nothing,
      timezone = Core.Nothing,
      distanceUnit = Core.Nothing
    }

-- | The name of a room profile.
profileData_profileName :: Lens.Lens' ProfileData (Core.Maybe Core.Text)
profileData_profileName = Lens.lens (\ProfileData' {profileName} -> profileName) (\s@ProfileData' {} a -> s {profileName = a} :: ProfileData)

-- | Retrieves if the profile data is default or not.
profileData_isDefault :: Lens.Lens' ProfileData (Core.Maybe Core.Bool)
profileData_isDefault = Lens.lens (\ProfileData' {isDefault} -> isDefault) (\s@ProfileData' {} a -> s {isDefault = a} :: ProfileData)

-- | The address of a room profile.
profileData_address :: Lens.Lens' ProfileData (Core.Maybe Core.Text)
profileData_address = Lens.lens (\ProfileData' {address} -> address) (\s@ProfileData' {} a -> s {address = a} :: ProfileData)

-- | The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
profileData_locale :: Lens.Lens' ProfileData (Core.Maybe Core.Text)
profileData_locale = Lens.lens (\ProfileData' {locale} -> locale) (\s@ProfileData' {} a -> s {locale = a} :: ProfileData)

-- | The temperature unit of a room profile.
profileData_temperatureUnit :: Lens.Lens' ProfileData (Core.Maybe TemperatureUnit)
profileData_temperatureUnit = Lens.lens (\ProfileData' {temperatureUnit} -> temperatureUnit) (\s@ProfileData' {} a -> s {temperatureUnit = a} :: ProfileData)

-- | The wake word of a room profile.
profileData_wakeWord :: Lens.Lens' ProfileData (Core.Maybe WakeWord)
profileData_wakeWord = Lens.lens (\ProfileData' {wakeWord} -> wakeWord) (\s@ProfileData' {} a -> s {wakeWord = a} :: ProfileData)

-- | The ARN of a room profile.
profileData_profileArn :: Lens.Lens' ProfileData (Core.Maybe Core.Text)
profileData_profileArn = Lens.lens (\ProfileData' {profileArn} -> profileArn) (\s@ProfileData' {} a -> s {profileArn = a} :: ProfileData)

-- | The time zone of a room profile.
profileData_timezone :: Lens.Lens' ProfileData (Core.Maybe Core.Text)
profileData_timezone = Lens.lens (\ProfileData' {timezone} -> timezone) (\s@ProfileData' {} a -> s {timezone = a} :: ProfileData)

-- | The distance unit of a room profile.
profileData_distanceUnit :: Lens.Lens' ProfileData (Core.Maybe DistanceUnit)
profileData_distanceUnit = Lens.lens (\ProfileData' {distanceUnit} -> distanceUnit) (\s@ProfileData' {} a -> s {distanceUnit = a} :: ProfileData)

instance Core.FromJSON ProfileData where
  parseJSON =
    Core.withObject
      "ProfileData"
      ( \x ->
          ProfileData'
            Core.<$> (x Core..:? "ProfileName")
            Core.<*> (x Core..:? "IsDefault")
            Core.<*> (x Core..:? "Address")
            Core.<*> (x Core..:? "Locale")
            Core.<*> (x Core..:? "TemperatureUnit")
            Core.<*> (x Core..:? "WakeWord")
            Core.<*> (x Core..:? "ProfileArn")
            Core.<*> (x Core..:? "Timezone")
            Core.<*> (x Core..:? "DistanceUnit")
      )

instance Core.Hashable ProfileData

instance Core.NFData ProfileData
