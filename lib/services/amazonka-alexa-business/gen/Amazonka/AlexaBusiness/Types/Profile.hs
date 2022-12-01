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
-- Module      : Amazonka.AlexaBusiness.Types.Profile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Profile where

import Amazonka.AlexaBusiness.Types.DistanceUnit
import Amazonka.AlexaBusiness.Types.MeetingRoomConfiguration
import Amazonka.AlexaBusiness.Types.TemperatureUnit
import Amazonka.AlexaBusiness.Types.WakeWord
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A room profile with attributes.
--
-- /See:/ 'newProfile' smart constructor.
data Profile = Profile'
  { -- | The setup mode of a room profile.
    setupModeDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The distance unit of a room profile.
    distanceUnit :: Prelude.Maybe DistanceUnit,
    -- | Whether data retention of the profile is enabled.
    dataRetentionOptIn :: Prelude.Maybe Prelude.Bool,
    -- | The name of a room profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The wake word of a room profile.
    wakeWord :: Prelude.Maybe WakeWord,
    -- | The locale of a room profile. (This is currently available only to a
    -- limited preview audience.)
    locale :: Prelude.Maybe Prelude.Text,
    -- | Meeting room settings of a room profile.
    meetingRoomConfiguration :: Prelude.Maybe MeetingRoomConfiguration,
    -- | The time zone of a room profile.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a room profile.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The PSTN setting of a room profile.
    pSTNEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Retrieves if the profile is default or not.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The address of a room profile.
    address :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the address book.
    addressBookArn :: Prelude.Maybe Prelude.Text,
    -- | The max volume limit of a room profile.
    maxVolumeLimit :: Prelude.Maybe Prelude.Int,
    -- | The temperature unit of a room profile.
    temperatureUnit :: Prelude.Maybe TemperatureUnit
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
-- 'distanceUnit', 'profile_distanceUnit' - The distance unit of a room profile.
--
-- 'dataRetentionOptIn', 'profile_dataRetentionOptIn' - Whether data retention of the profile is enabled.
--
-- 'profileName', 'profile_profileName' - The name of a room profile.
--
-- 'wakeWord', 'profile_wakeWord' - The wake word of a room profile.
--
-- 'locale', 'profile_locale' - The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
--
-- 'meetingRoomConfiguration', 'profile_meetingRoomConfiguration' - Meeting room settings of a room profile.
--
-- 'timezone', 'profile_timezone' - The time zone of a room profile.
--
-- 'profileArn', 'profile_profileArn' - The ARN of a room profile.
--
-- 'pSTNEnabled', 'profile_pSTNEnabled' - The PSTN setting of a room profile.
--
-- 'isDefault', 'profile_isDefault' - Retrieves if the profile is default or not.
--
-- 'address', 'profile_address' - The address of a room profile.
--
-- 'addressBookArn', 'profile_addressBookArn' - The ARN of the address book.
--
-- 'maxVolumeLimit', 'profile_maxVolumeLimit' - The max volume limit of a room profile.
--
-- 'temperatureUnit', 'profile_temperatureUnit' - The temperature unit of a room profile.
newProfile ::
  Profile
newProfile =
  Profile'
    { setupModeDisabled = Prelude.Nothing,
      distanceUnit = Prelude.Nothing,
      dataRetentionOptIn = Prelude.Nothing,
      profileName = Prelude.Nothing,
      wakeWord = Prelude.Nothing,
      locale = Prelude.Nothing,
      meetingRoomConfiguration = Prelude.Nothing,
      timezone = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      pSTNEnabled = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      address = Prelude.Nothing,
      addressBookArn = Prelude.Nothing,
      maxVolumeLimit = Prelude.Nothing,
      temperatureUnit = Prelude.Nothing
    }

-- | The setup mode of a room profile.
profile_setupModeDisabled :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_setupModeDisabled = Lens.lens (\Profile' {setupModeDisabled} -> setupModeDisabled) (\s@Profile' {} a -> s {setupModeDisabled = a} :: Profile)

-- | The distance unit of a room profile.
profile_distanceUnit :: Lens.Lens' Profile (Prelude.Maybe DistanceUnit)
profile_distanceUnit = Lens.lens (\Profile' {distanceUnit} -> distanceUnit) (\s@Profile' {} a -> s {distanceUnit = a} :: Profile)

-- | Whether data retention of the profile is enabled.
profile_dataRetentionOptIn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_dataRetentionOptIn = Lens.lens (\Profile' {dataRetentionOptIn} -> dataRetentionOptIn) (\s@Profile' {} a -> s {dataRetentionOptIn = a} :: Profile)

-- | The name of a room profile.
profile_profileName :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileName = Lens.lens (\Profile' {profileName} -> profileName) (\s@Profile' {} a -> s {profileName = a} :: Profile)

-- | The wake word of a room profile.
profile_wakeWord :: Lens.Lens' Profile (Prelude.Maybe WakeWord)
profile_wakeWord = Lens.lens (\Profile' {wakeWord} -> wakeWord) (\s@Profile' {} a -> s {wakeWord = a} :: Profile)

-- | The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
profile_locale :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_locale = Lens.lens (\Profile' {locale} -> locale) (\s@Profile' {} a -> s {locale = a} :: Profile)

-- | Meeting room settings of a room profile.
profile_meetingRoomConfiguration :: Lens.Lens' Profile (Prelude.Maybe MeetingRoomConfiguration)
profile_meetingRoomConfiguration = Lens.lens (\Profile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@Profile' {} a -> s {meetingRoomConfiguration = a} :: Profile)

-- | The time zone of a room profile.
profile_timezone :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_timezone = Lens.lens (\Profile' {timezone} -> timezone) (\s@Profile' {} a -> s {timezone = a} :: Profile)

-- | The ARN of a room profile.
profile_profileArn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileArn = Lens.lens (\Profile' {profileArn} -> profileArn) (\s@Profile' {} a -> s {profileArn = a} :: Profile)

-- | The PSTN setting of a room profile.
profile_pSTNEnabled :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_pSTNEnabled = Lens.lens (\Profile' {pSTNEnabled} -> pSTNEnabled) (\s@Profile' {} a -> s {pSTNEnabled = a} :: Profile)

-- | Retrieves if the profile is default or not.
profile_isDefault :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_isDefault = Lens.lens (\Profile' {isDefault} -> isDefault) (\s@Profile' {} a -> s {isDefault = a} :: Profile)

-- | The address of a room profile.
profile_address :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_address = Lens.lens (\Profile' {address} -> address) (\s@Profile' {} a -> s {address = a} :: Profile)

-- | The ARN of the address book.
profile_addressBookArn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_addressBookArn = Lens.lens (\Profile' {addressBookArn} -> addressBookArn) (\s@Profile' {} a -> s {addressBookArn = a} :: Profile)

-- | The max volume limit of a room profile.
profile_maxVolumeLimit :: Lens.Lens' Profile (Prelude.Maybe Prelude.Int)
profile_maxVolumeLimit = Lens.lens (\Profile' {maxVolumeLimit} -> maxVolumeLimit) (\s@Profile' {} a -> s {maxVolumeLimit = a} :: Profile)

-- | The temperature unit of a room profile.
profile_temperatureUnit :: Lens.Lens' Profile (Prelude.Maybe TemperatureUnit)
profile_temperatureUnit = Lens.lens (\Profile' {temperatureUnit} -> temperatureUnit) (\s@Profile' {} a -> s {temperatureUnit = a} :: Profile)

instance Core.FromJSON Profile where
  parseJSON =
    Core.withObject
      "Profile"
      ( \x ->
          Profile'
            Prelude.<$> (x Core..:? "SetupModeDisabled")
            Prelude.<*> (x Core..:? "DistanceUnit")
            Prelude.<*> (x Core..:? "DataRetentionOptIn")
            Prelude.<*> (x Core..:? "ProfileName")
            Prelude.<*> (x Core..:? "WakeWord")
            Prelude.<*> (x Core..:? "Locale")
            Prelude.<*> (x Core..:? "MeetingRoomConfiguration")
            Prelude.<*> (x Core..:? "Timezone")
            Prelude.<*> (x Core..:? "ProfileArn")
            Prelude.<*> (x Core..:? "PSTNEnabled")
            Prelude.<*> (x Core..:? "IsDefault")
            Prelude.<*> (x Core..:? "Address")
            Prelude.<*> (x Core..:? "AddressBookArn")
            Prelude.<*> (x Core..:? "MaxVolumeLimit")
            Prelude.<*> (x Core..:? "TemperatureUnit")
      )

instance Prelude.Hashable Profile where
  hashWithSalt _salt Profile' {..} =
    _salt `Prelude.hashWithSalt` setupModeDisabled
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` dataRetentionOptIn
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` wakeWord
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` meetingRoomConfiguration
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` pSTNEnabled
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` addressBookArn
      `Prelude.hashWithSalt` maxVolumeLimit
      `Prelude.hashWithSalt` temperatureUnit

instance Prelude.NFData Profile where
  rnf Profile' {..} =
    Prelude.rnf setupModeDisabled
      `Prelude.seq` Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf dataRetentionOptIn
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf wakeWord
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf meetingRoomConfiguration
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf pSTNEnabled
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf address
      `Prelude.seq` Prelude.rnf addressBookArn
      `Prelude.seq` Prelude.rnf maxVolumeLimit
      `Prelude.seq` Prelude.rnf temperatureUnit
