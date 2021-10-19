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
import qualified Network.AWS.Prelude as Prelude

-- | A room profile with attributes.
--
-- /See:/ 'newProfile' smart constructor.
data Profile = Profile'
  { -- | The setup mode of a room profile.
    setupModeDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The PSTN setting of a room profile.
    pSTNEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the address book.
    addressBookArn :: Prelude.Maybe Prelude.Text,
    -- | The distance unit of a room profile.
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
    -- | Meeting room settings of a room profile.
    meetingRoomConfiguration :: Prelude.Maybe MeetingRoomConfiguration,
    -- | The name of a room profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The temperature unit of a room profile.
    temperatureUnit :: Prelude.Maybe TemperatureUnit,
    -- | Whether data retention of the profile is enabled.
    dataRetentionOptIn :: Prelude.Maybe Prelude.Bool,
    -- | The time zone of a room profile.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The max volume limit of a room profile.
    maxVolumeLimit :: Prelude.Maybe Prelude.Int,
    -- | Retrieves if the profile is default or not.
    isDefault :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Profile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setupModeDisabled', 'profile_setupModeDisabled' - The setup mode of a room profile.
--
-- 'pSTNEnabled', 'profile_pSTNEnabled' - The PSTN setting of a room profile.
--
-- 'addressBookArn', 'profile_addressBookArn' - The ARN of the address book.
--
-- 'distanceUnit', 'profile_distanceUnit' - The distance unit of a room profile.
--
-- 'locale', 'profile_locale' - The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
--
-- 'address', 'profile_address' - The address of a room profile.
--
-- 'profileArn', 'profile_profileArn' - The ARN of a room profile.
--
-- 'wakeWord', 'profile_wakeWord' - The wake word of a room profile.
--
-- 'meetingRoomConfiguration', 'profile_meetingRoomConfiguration' - Meeting room settings of a room profile.
--
-- 'profileName', 'profile_profileName' - The name of a room profile.
--
-- 'temperatureUnit', 'profile_temperatureUnit' - The temperature unit of a room profile.
--
-- 'dataRetentionOptIn', 'profile_dataRetentionOptIn' - Whether data retention of the profile is enabled.
--
-- 'timezone', 'profile_timezone' - The time zone of a room profile.
--
-- 'maxVolumeLimit', 'profile_maxVolumeLimit' - The max volume limit of a room profile.
--
-- 'isDefault', 'profile_isDefault' - Retrieves if the profile is default or not.
newProfile ::
  Profile
newProfile =
  Profile'
    { setupModeDisabled = Prelude.Nothing,
      pSTNEnabled = Prelude.Nothing,
      addressBookArn = Prelude.Nothing,
      distanceUnit = Prelude.Nothing,
      locale = Prelude.Nothing,
      address = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      wakeWord = Prelude.Nothing,
      meetingRoomConfiguration = Prelude.Nothing,
      profileName = Prelude.Nothing,
      temperatureUnit = Prelude.Nothing,
      dataRetentionOptIn = Prelude.Nothing,
      timezone = Prelude.Nothing,
      maxVolumeLimit = Prelude.Nothing,
      isDefault = Prelude.Nothing
    }

-- | The setup mode of a room profile.
profile_setupModeDisabled :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_setupModeDisabled = Lens.lens (\Profile' {setupModeDisabled} -> setupModeDisabled) (\s@Profile' {} a -> s {setupModeDisabled = a} :: Profile)

-- | The PSTN setting of a room profile.
profile_pSTNEnabled :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_pSTNEnabled = Lens.lens (\Profile' {pSTNEnabled} -> pSTNEnabled) (\s@Profile' {} a -> s {pSTNEnabled = a} :: Profile)

-- | The ARN of the address book.
profile_addressBookArn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_addressBookArn = Lens.lens (\Profile' {addressBookArn} -> addressBookArn) (\s@Profile' {} a -> s {addressBookArn = a} :: Profile)

-- | The distance unit of a room profile.
profile_distanceUnit :: Lens.Lens' Profile (Prelude.Maybe DistanceUnit)
profile_distanceUnit = Lens.lens (\Profile' {distanceUnit} -> distanceUnit) (\s@Profile' {} a -> s {distanceUnit = a} :: Profile)

-- | The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
profile_locale :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_locale = Lens.lens (\Profile' {locale} -> locale) (\s@Profile' {} a -> s {locale = a} :: Profile)

-- | The address of a room profile.
profile_address :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_address = Lens.lens (\Profile' {address} -> address) (\s@Profile' {} a -> s {address = a} :: Profile)

-- | The ARN of a room profile.
profile_profileArn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileArn = Lens.lens (\Profile' {profileArn} -> profileArn) (\s@Profile' {} a -> s {profileArn = a} :: Profile)

-- | The wake word of a room profile.
profile_wakeWord :: Lens.Lens' Profile (Prelude.Maybe WakeWord)
profile_wakeWord = Lens.lens (\Profile' {wakeWord} -> wakeWord) (\s@Profile' {} a -> s {wakeWord = a} :: Profile)

-- | Meeting room settings of a room profile.
profile_meetingRoomConfiguration :: Lens.Lens' Profile (Prelude.Maybe MeetingRoomConfiguration)
profile_meetingRoomConfiguration = Lens.lens (\Profile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@Profile' {} a -> s {meetingRoomConfiguration = a} :: Profile)

-- | The name of a room profile.
profile_profileName :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileName = Lens.lens (\Profile' {profileName} -> profileName) (\s@Profile' {} a -> s {profileName = a} :: Profile)

-- | The temperature unit of a room profile.
profile_temperatureUnit :: Lens.Lens' Profile (Prelude.Maybe TemperatureUnit)
profile_temperatureUnit = Lens.lens (\Profile' {temperatureUnit} -> temperatureUnit) (\s@Profile' {} a -> s {temperatureUnit = a} :: Profile)

-- | Whether data retention of the profile is enabled.
profile_dataRetentionOptIn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_dataRetentionOptIn = Lens.lens (\Profile' {dataRetentionOptIn} -> dataRetentionOptIn) (\s@Profile' {} a -> s {dataRetentionOptIn = a} :: Profile)

-- | The time zone of a room profile.
profile_timezone :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_timezone = Lens.lens (\Profile' {timezone} -> timezone) (\s@Profile' {} a -> s {timezone = a} :: Profile)

-- | The max volume limit of a room profile.
profile_maxVolumeLimit :: Lens.Lens' Profile (Prelude.Maybe Prelude.Int)
profile_maxVolumeLimit = Lens.lens (\Profile' {maxVolumeLimit} -> maxVolumeLimit) (\s@Profile' {} a -> s {maxVolumeLimit = a} :: Profile)

-- | Retrieves if the profile is default or not.
profile_isDefault :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_isDefault = Lens.lens (\Profile' {isDefault} -> isDefault) (\s@Profile' {} a -> s {isDefault = a} :: Profile)

instance Core.FromJSON Profile where
  parseJSON =
    Core.withObject
      "Profile"
      ( \x ->
          Profile'
            Prelude.<$> (x Core..:? "SetupModeDisabled")
            Prelude.<*> (x Core..:? "PSTNEnabled")
            Prelude.<*> (x Core..:? "AddressBookArn")
            Prelude.<*> (x Core..:? "DistanceUnit")
            Prelude.<*> (x Core..:? "Locale")
            Prelude.<*> (x Core..:? "Address")
            Prelude.<*> (x Core..:? "ProfileArn")
            Prelude.<*> (x Core..:? "WakeWord")
            Prelude.<*> (x Core..:? "MeetingRoomConfiguration")
            Prelude.<*> (x Core..:? "ProfileName")
            Prelude.<*> (x Core..:? "TemperatureUnit")
            Prelude.<*> (x Core..:? "DataRetentionOptIn")
            Prelude.<*> (x Core..:? "Timezone")
            Prelude.<*> (x Core..:? "MaxVolumeLimit")
            Prelude.<*> (x Core..:? "IsDefault")
      )

instance Prelude.Hashable Profile

instance Prelude.NFData Profile
