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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Profile where

import Amazonka.AlexaBusiness.Types.DistanceUnit
import Amazonka.AlexaBusiness.Types.MeetingRoomConfiguration
import Amazonka.AlexaBusiness.Types.TemperatureUnit
import Amazonka.AlexaBusiness.Types.WakeWord
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A room profile with attributes.
--
-- /See:/ 'newProfile' smart constructor.
data Profile = Profile'
  { -- | The address of a room profile.
    address :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the address book.
    addressBookArn :: Prelude.Maybe Prelude.Text,
    -- | Whether data retention of the profile is enabled.
    dataRetentionOptIn :: Prelude.Maybe Prelude.Bool,
    -- | The distance unit of a room profile.
    distanceUnit :: Prelude.Maybe DistanceUnit,
    -- | Retrieves if the profile is default or not.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The locale of a room profile. (This is currently available only to a
    -- limited preview audience.)
    locale :: Prelude.Maybe Prelude.Text,
    -- | The max volume limit of a room profile.
    maxVolumeLimit :: Prelude.Maybe Prelude.Int,
    -- | Meeting room settings of a room profile.
    meetingRoomConfiguration :: Prelude.Maybe MeetingRoomConfiguration,
    -- | The PSTN setting of a room profile.
    pSTNEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of a room profile.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a room profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The setup mode of a room profile.
    setupModeDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The temperature unit of a room profile.
    temperatureUnit :: Prelude.Maybe TemperatureUnit,
    -- | The time zone of a room profile.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The wake word of a room profile.
    wakeWord :: Prelude.Maybe WakeWord
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
-- 'address', 'profile_address' - The address of a room profile.
--
-- 'addressBookArn', 'profile_addressBookArn' - The ARN of the address book.
--
-- 'dataRetentionOptIn', 'profile_dataRetentionOptIn' - Whether data retention of the profile is enabled.
--
-- 'distanceUnit', 'profile_distanceUnit' - The distance unit of a room profile.
--
-- 'isDefault', 'profile_isDefault' - Retrieves if the profile is default or not.
--
-- 'locale', 'profile_locale' - The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
--
-- 'maxVolumeLimit', 'profile_maxVolumeLimit' - The max volume limit of a room profile.
--
-- 'meetingRoomConfiguration', 'profile_meetingRoomConfiguration' - Meeting room settings of a room profile.
--
-- 'pSTNEnabled', 'profile_pSTNEnabled' - The PSTN setting of a room profile.
--
-- 'profileArn', 'profile_profileArn' - The ARN of a room profile.
--
-- 'profileName', 'profile_profileName' - The name of a room profile.
--
-- 'setupModeDisabled', 'profile_setupModeDisabled' - The setup mode of a room profile.
--
-- 'temperatureUnit', 'profile_temperatureUnit' - The temperature unit of a room profile.
--
-- 'timezone', 'profile_timezone' - The time zone of a room profile.
--
-- 'wakeWord', 'profile_wakeWord' - The wake word of a room profile.
newProfile ::
  Profile
newProfile =
  Profile'
    { address = Prelude.Nothing,
      addressBookArn = Prelude.Nothing,
      dataRetentionOptIn = Prelude.Nothing,
      distanceUnit = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      locale = Prelude.Nothing,
      maxVolumeLimit = Prelude.Nothing,
      meetingRoomConfiguration = Prelude.Nothing,
      pSTNEnabled = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      profileName = Prelude.Nothing,
      setupModeDisabled = Prelude.Nothing,
      temperatureUnit = Prelude.Nothing,
      timezone = Prelude.Nothing,
      wakeWord = Prelude.Nothing
    }

-- | The address of a room profile.
profile_address :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_address = Lens.lens (\Profile' {address} -> address) (\s@Profile' {} a -> s {address = a} :: Profile)

-- | The ARN of the address book.
profile_addressBookArn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_addressBookArn = Lens.lens (\Profile' {addressBookArn} -> addressBookArn) (\s@Profile' {} a -> s {addressBookArn = a} :: Profile)

-- | Whether data retention of the profile is enabled.
profile_dataRetentionOptIn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_dataRetentionOptIn = Lens.lens (\Profile' {dataRetentionOptIn} -> dataRetentionOptIn) (\s@Profile' {} a -> s {dataRetentionOptIn = a} :: Profile)

-- | The distance unit of a room profile.
profile_distanceUnit :: Lens.Lens' Profile (Prelude.Maybe DistanceUnit)
profile_distanceUnit = Lens.lens (\Profile' {distanceUnit} -> distanceUnit) (\s@Profile' {} a -> s {distanceUnit = a} :: Profile)

-- | Retrieves if the profile is default or not.
profile_isDefault :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_isDefault = Lens.lens (\Profile' {isDefault} -> isDefault) (\s@Profile' {} a -> s {isDefault = a} :: Profile)

-- | The locale of a room profile. (This is currently available only to a
-- limited preview audience.)
profile_locale :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_locale = Lens.lens (\Profile' {locale} -> locale) (\s@Profile' {} a -> s {locale = a} :: Profile)

-- | The max volume limit of a room profile.
profile_maxVolumeLimit :: Lens.Lens' Profile (Prelude.Maybe Prelude.Int)
profile_maxVolumeLimit = Lens.lens (\Profile' {maxVolumeLimit} -> maxVolumeLimit) (\s@Profile' {} a -> s {maxVolumeLimit = a} :: Profile)

-- | Meeting room settings of a room profile.
profile_meetingRoomConfiguration :: Lens.Lens' Profile (Prelude.Maybe MeetingRoomConfiguration)
profile_meetingRoomConfiguration = Lens.lens (\Profile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@Profile' {} a -> s {meetingRoomConfiguration = a} :: Profile)

-- | The PSTN setting of a room profile.
profile_pSTNEnabled :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_pSTNEnabled = Lens.lens (\Profile' {pSTNEnabled} -> pSTNEnabled) (\s@Profile' {} a -> s {pSTNEnabled = a} :: Profile)

-- | The ARN of a room profile.
profile_profileArn :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileArn = Lens.lens (\Profile' {profileArn} -> profileArn) (\s@Profile' {} a -> s {profileArn = a} :: Profile)

-- | The name of a room profile.
profile_profileName :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileName = Lens.lens (\Profile' {profileName} -> profileName) (\s@Profile' {} a -> s {profileName = a} :: Profile)

-- | The setup mode of a room profile.
profile_setupModeDisabled :: Lens.Lens' Profile (Prelude.Maybe Prelude.Bool)
profile_setupModeDisabled = Lens.lens (\Profile' {setupModeDisabled} -> setupModeDisabled) (\s@Profile' {} a -> s {setupModeDisabled = a} :: Profile)

-- | The temperature unit of a room profile.
profile_temperatureUnit :: Lens.Lens' Profile (Prelude.Maybe TemperatureUnit)
profile_temperatureUnit = Lens.lens (\Profile' {temperatureUnit} -> temperatureUnit) (\s@Profile' {} a -> s {temperatureUnit = a} :: Profile)

-- | The time zone of a room profile.
profile_timezone :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_timezone = Lens.lens (\Profile' {timezone} -> timezone) (\s@Profile' {} a -> s {timezone = a} :: Profile)

-- | The wake word of a room profile.
profile_wakeWord :: Lens.Lens' Profile (Prelude.Maybe WakeWord)
profile_wakeWord = Lens.lens (\Profile' {wakeWord} -> wakeWord) (\s@Profile' {} a -> s {wakeWord = a} :: Profile)

instance Data.FromJSON Profile where
  parseJSON =
    Data.withObject
      "Profile"
      ( \x ->
          Profile'
            Prelude.<$> (x Data..:? "Address")
            Prelude.<*> (x Data..:? "AddressBookArn")
            Prelude.<*> (x Data..:? "DataRetentionOptIn")
            Prelude.<*> (x Data..:? "DistanceUnit")
            Prelude.<*> (x Data..:? "IsDefault")
            Prelude.<*> (x Data..:? "Locale")
            Prelude.<*> (x Data..:? "MaxVolumeLimit")
            Prelude.<*> (x Data..:? "MeetingRoomConfiguration")
            Prelude.<*> (x Data..:? "PSTNEnabled")
            Prelude.<*> (x Data..:? "ProfileArn")
            Prelude.<*> (x Data..:? "ProfileName")
            Prelude.<*> (x Data..:? "SetupModeDisabled")
            Prelude.<*> (x Data..:? "TemperatureUnit")
            Prelude.<*> (x Data..:? "Timezone")
            Prelude.<*> (x Data..:? "WakeWord")
      )

instance Prelude.Hashable Profile where
  hashWithSalt _salt Profile' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` addressBookArn
      `Prelude.hashWithSalt` dataRetentionOptIn
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxVolumeLimit
      `Prelude.hashWithSalt` meetingRoomConfiguration
      `Prelude.hashWithSalt` pSTNEnabled
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` setupModeDisabled
      `Prelude.hashWithSalt` temperatureUnit
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` wakeWord

instance Prelude.NFData Profile where
  rnf Profile' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf addressBookArn
      `Prelude.seq` Prelude.rnf dataRetentionOptIn
      `Prelude.seq` Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf maxVolumeLimit
      `Prelude.seq` Prelude.rnf meetingRoomConfiguration
      `Prelude.seq` Prelude.rnf pSTNEnabled
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf setupModeDisabled
      `Prelude.seq` Prelude.rnf temperatureUnit
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf wakeWord
