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
-- Module      : Network.AWS.AlexaBusiness.Types.Profile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Profile where

import Network.AWS.AlexaBusiness.Types.DistanceUnit
import Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.TemperatureUnit
import Network.AWS.AlexaBusiness.Types.WakeWord
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A room profile with attributes.
--
-- /See:/ 'newProfile' smart constructor.
data Profile = Profile'
  { -- | The name of a room profile.
    profileName :: Core.Maybe Core.Text,
    -- | Retrieves if the profile is default or not.
    isDefault :: Core.Maybe Core.Bool,
    -- | The address of a room profile.
    address :: Core.Maybe Core.Text,
    -- | The locale of a room profile. (This is currently available only to a
    -- limited preview audience.)
    locale :: Core.Maybe Core.Text,
    -- | The temperature unit of a room profile.
    temperatureUnit :: Core.Maybe TemperatureUnit,
    -- | The ARN of the address book.
    addressBookArn :: Core.Maybe Core.Text,
    -- | The setup mode of a room profile.
    setupModeDisabled :: Core.Maybe Core.Bool,
    -- | The PSTN setting of a room profile.
    pSTNEnabled :: Core.Maybe Core.Bool,
    -- | The max volume limit of a room profile.
    maxVolumeLimit :: Core.Maybe Core.Int,
    -- | Meeting room settings of a room profile.
    meetingRoomConfiguration :: Core.Maybe MeetingRoomConfiguration,
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
-- Create a value of 'Profile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileName', 'profile_profileName' - The name of a room profile.
--
-- 'isDefault', 'profile_isDefault' - Retrieves if the profile is default or not.
--
-- 'address', 'profile_address' - The address of a room profile.
--
-- 'locale', 'profile_locale' - The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
--
-- 'temperatureUnit', 'profile_temperatureUnit' - The temperature unit of a room profile.
--
-- 'addressBookArn', 'profile_addressBookArn' - The ARN of the address book.
--
-- 'setupModeDisabled', 'profile_setupModeDisabled' - The setup mode of a room profile.
--
-- 'pSTNEnabled', 'profile_pSTNEnabled' - The PSTN setting of a room profile.
--
-- 'maxVolumeLimit', 'profile_maxVolumeLimit' - The max volume limit of a room profile.
--
-- 'meetingRoomConfiguration', 'profile_meetingRoomConfiguration' - Meeting room settings of a room profile.
--
-- 'wakeWord', 'profile_wakeWord' - The wake word of a room profile.
--
-- 'profileArn', 'profile_profileArn' - The ARN of a room profile.
--
-- 'timezone', 'profile_timezone' - The time zone of a room profile.
--
-- 'distanceUnit', 'profile_distanceUnit' - The distance unit of a room profile.
newProfile ::
  Profile
newProfile =
  Profile'
    { profileName = Core.Nothing,
      isDefault = Core.Nothing,
      address = Core.Nothing,
      locale = Core.Nothing,
      temperatureUnit = Core.Nothing,
      addressBookArn = Core.Nothing,
      setupModeDisabled = Core.Nothing,
      pSTNEnabled = Core.Nothing,
      maxVolumeLimit = Core.Nothing,
      meetingRoomConfiguration = Core.Nothing,
      wakeWord = Core.Nothing,
      profileArn = Core.Nothing,
      timezone = Core.Nothing,
      distanceUnit = Core.Nothing
    }

-- | The name of a room profile.
profile_profileName :: Lens.Lens' Profile (Core.Maybe Core.Text)
profile_profileName = Lens.lens (\Profile' {profileName} -> profileName) (\s@Profile' {} a -> s {profileName = a} :: Profile)

-- | Retrieves if the profile is default or not.
profile_isDefault :: Lens.Lens' Profile (Core.Maybe Core.Bool)
profile_isDefault = Lens.lens (\Profile' {isDefault} -> isDefault) (\s@Profile' {} a -> s {isDefault = a} :: Profile)

-- | The address of a room profile.
profile_address :: Lens.Lens' Profile (Core.Maybe Core.Text)
profile_address = Lens.lens (\Profile' {address} -> address) (\s@Profile' {} a -> s {address = a} :: Profile)

-- | The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
profile_locale :: Lens.Lens' Profile (Core.Maybe Core.Text)
profile_locale = Lens.lens (\Profile' {locale} -> locale) (\s@Profile' {} a -> s {locale = a} :: Profile)

-- | The temperature unit of a room profile.
profile_temperatureUnit :: Lens.Lens' Profile (Core.Maybe TemperatureUnit)
profile_temperatureUnit = Lens.lens (\Profile' {temperatureUnit} -> temperatureUnit) (\s@Profile' {} a -> s {temperatureUnit = a} :: Profile)

-- | The ARN of the address book.
profile_addressBookArn :: Lens.Lens' Profile (Core.Maybe Core.Text)
profile_addressBookArn = Lens.lens (\Profile' {addressBookArn} -> addressBookArn) (\s@Profile' {} a -> s {addressBookArn = a} :: Profile)

-- | The setup mode of a room profile.
profile_setupModeDisabled :: Lens.Lens' Profile (Core.Maybe Core.Bool)
profile_setupModeDisabled = Lens.lens (\Profile' {setupModeDisabled} -> setupModeDisabled) (\s@Profile' {} a -> s {setupModeDisabled = a} :: Profile)

-- | The PSTN setting of a room profile.
profile_pSTNEnabled :: Lens.Lens' Profile (Core.Maybe Core.Bool)
profile_pSTNEnabled = Lens.lens (\Profile' {pSTNEnabled} -> pSTNEnabled) (\s@Profile' {} a -> s {pSTNEnabled = a} :: Profile)

-- | The max volume limit of a room profile.
profile_maxVolumeLimit :: Lens.Lens' Profile (Core.Maybe Core.Int)
profile_maxVolumeLimit = Lens.lens (\Profile' {maxVolumeLimit} -> maxVolumeLimit) (\s@Profile' {} a -> s {maxVolumeLimit = a} :: Profile)

-- | Meeting room settings of a room profile.
profile_meetingRoomConfiguration :: Lens.Lens' Profile (Core.Maybe MeetingRoomConfiguration)
profile_meetingRoomConfiguration = Lens.lens (\Profile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@Profile' {} a -> s {meetingRoomConfiguration = a} :: Profile)

-- | The wake word of a room profile.
profile_wakeWord :: Lens.Lens' Profile (Core.Maybe WakeWord)
profile_wakeWord = Lens.lens (\Profile' {wakeWord} -> wakeWord) (\s@Profile' {} a -> s {wakeWord = a} :: Profile)

-- | The ARN of a room profile.
profile_profileArn :: Lens.Lens' Profile (Core.Maybe Core.Text)
profile_profileArn = Lens.lens (\Profile' {profileArn} -> profileArn) (\s@Profile' {} a -> s {profileArn = a} :: Profile)

-- | The time zone of a room profile.
profile_timezone :: Lens.Lens' Profile (Core.Maybe Core.Text)
profile_timezone = Lens.lens (\Profile' {timezone} -> timezone) (\s@Profile' {} a -> s {timezone = a} :: Profile)

-- | The distance unit of a room profile.
profile_distanceUnit :: Lens.Lens' Profile (Core.Maybe DistanceUnit)
profile_distanceUnit = Lens.lens (\Profile' {distanceUnit} -> distanceUnit) (\s@Profile' {} a -> s {distanceUnit = a} :: Profile)

instance Core.FromJSON Profile where
  parseJSON =
    Core.withObject
      "Profile"
      ( \x ->
          Profile'
            Core.<$> (x Core..:? "ProfileName")
            Core.<*> (x Core..:? "IsDefault")
            Core.<*> (x Core..:? "Address")
            Core.<*> (x Core..:? "Locale")
            Core.<*> (x Core..:? "TemperatureUnit")
            Core.<*> (x Core..:? "AddressBookArn")
            Core.<*> (x Core..:? "SetupModeDisabled")
            Core.<*> (x Core..:? "PSTNEnabled")
            Core.<*> (x Core..:? "MaxVolumeLimit")
            Core.<*> (x Core..:? "MeetingRoomConfiguration")
            Core.<*> (x Core..:? "WakeWord")
            Core.<*> (x Core..:? "ProfileArn")
            Core.<*> (x Core..:? "Timezone")
            Core.<*> (x Core..:? "DistanceUnit")
      )

instance Core.Hashable Profile

instance Core.NFData Profile
