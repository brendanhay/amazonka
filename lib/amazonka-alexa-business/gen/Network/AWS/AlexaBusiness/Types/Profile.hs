{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Profile where

import Network.AWS.AlexaBusiness.Types.DistanceUnit
import Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.TemperatureUnit
import Network.AWS.AlexaBusiness.Types.WakeWord
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A room profile with attributes.
--
--
--
-- /See:/ 'profile' smart constructor.
data Profile = Profile'
  { _pSetupModeDisabled :: !(Maybe Bool),
    _pPSTNEnabled :: !(Maybe Bool),
    _pAddressBookARN :: !(Maybe Text),
    _pDistanceUnit :: !(Maybe DistanceUnit),
    _pLocale :: !(Maybe Text),
    _pAddress :: !(Maybe Text),
    _pProfileARN :: !(Maybe Text),
    _pWakeWord :: !(Maybe WakeWord),
    _pMeetingRoomConfiguration :: !(Maybe MeetingRoomConfiguration),
    _pProfileName :: !(Maybe Text),
    _pTemperatureUnit :: !(Maybe TemperatureUnit),
    _pTimezone :: !(Maybe Text),
    _pMaxVolumeLimit :: !(Maybe Int),
    _pIsDefault :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Profile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pSetupModeDisabled' - The setup mode of a room profile.
--
-- * 'pPSTNEnabled' - The PSTN setting of a room profile.
--
-- * 'pAddressBookARN' - The ARN of the address book.
--
-- * 'pDistanceUnit' - The distance unit of a room profile.
--
-- * 'pLocale' - The locale of a room profile. (This is currently available only to a limited preview audience.)
--
-- * 'pAddress' - The address of a room profile.
--
-- * 'pProfileARN' - The ARN of a room profile.
--
-- * 'pWakeWord' - The wake word of a room profile.
--
-- * 'pMeetingRoomConfiguration' - Meeting room settings of a room profile.
--
-- * 'pProfileName' - The name of a room profile.
--
-- * 'pTemperatureUnit' - The temperature unit of a room profile.
--
-- * 'pTimezone' - The time zone of a room profile.
--
-- * 'pMaxVolumeLimit' - The max volume limit of a room profile.
--
-- * 'pIsDefault' - Retrieves if the profile is default or not.
profile ::
  Profile
profile =
  Profile'
    { _pSetupModeDisabled = Nothing,
      _pPSTNEnabled = Nothing,
      _pAddressBookARN = Nothing,
      _pDistanceUnit = Nothing,
      _pLocale = Nothing,
      _pAddress = Nothing,
      _pProfileARN = Nothing,
      _pWakeWord = Nothing,
      _pMeetingRoomConfiguration = Nothing,
      _pProfileName = Nothing,
      _pTemperatureUnit = Nothing,
      _pTimezone = Nothing,
      _pMaxVolumeLimit = Nothing,
      _pIsDefault = Nothing
    }

-- | The setup mode of a room profile.
pSetupModeDisabled :: Lens' Profile (Maybe Bool)
pSetupModeDisabled = lens _pSetupModeDisabled (\s a -> s {_pSetupModeDisabled = a})

-- | The PSTN setting of a room profile.
pPSTNEnabled :: Lens' Profile (Maybe Bool)
pPSTNEnabled = lens _pPSTNEnabled (\s a -> s {_pPSTNEnabled = a})

-- | The ARN of the address book.
pAddressBookARN :: Lens' Profile (Maybe Text)
pAddressBookARN = lens _pAddressBookARN (\s a -> s {_pAddressBookARN = a})

-- | The distance unit of a room profile.
pDistanceUnit :: Lens' Profile (Maybe DistanceUnit)
pDistanceUnit = lens _pDistanceUnit (\s a -> s {_pDistanceUnit = a})

-- | The locale of a room profile. (This is currently available only to a limited preview audience.)
pLocale :: Lens' Profile (Maybe Text)
pLocale = lens _pLocale (\s a -> s {_pLocale = a})

-- | The address of a room profile.
pAddress :: Lens' Profile (Maybe Text)
pAddress = lens _pAddress (\s a -> s {_pAddress = a})

-- | The ARN of a room profile.
pProfileARN :: Lens' Profile (Maybe Text)
pProfileARN = lens _pProfileARN (\s a -> s {_pProfileARN = a})

-- | The wake word of a room profile.
pWakeWord :: Lens' Profile (Maybe WakeWord)
pWakeWord = lens _pWakeWord (\s a -> s {_pWakeWord = a})

-- | Meeting room settings of a room profile.
pMeetingRoomConfiguration :: Lens' Profile (Maybe MeetingRoomConfiguration)
pMeetingRoomConfiguration = lens _pMeetingRoomConfiguration (\s a -> s {_pMeetingRoomConfiguration = a})

-- | The name of a room profile.
pProfileName :: Lens' Profile (Maybe Text)
pProfileName = lens _pProfileName (\s a -> s {_pProfileName = a})

-- | The temperature unit of a room profile.
pTemperatureUnit :: Lens' Profile (Maybe TemperatureUnit)
pTemperatureUnit = lens _pTemperatureUnit (\s a -> s {_pTemperatureUnit = a})

-- | The time zone of a room profile.
pTimezone :: Lens' Profile (Maybe Text)
pTimezone = lens _pTimezone (\s a -> s {_pTimezone = a})

-- | The max volume limit of a room profile.
pMaxVolumeLimit :: Lens' Profile (Maybe Int)
pMaxVolumeLimit = lens _pMaxVolumeLimit (\s a -> s {_pMaxVolumeLimit = a})

-- | Retrieves if the profile is default or not.
pIsDefault :: Lens' Profile (Maybe Bool)
pIsDefault = lens _pIsDefault (\s a -> s {_pIsDefault = a})

instance FromJSON Profile where
  parseJSON =
    withObject
      "Profile"
      ( \x ->
          Profile'
            <$> (x .:? "SetupModeDisabled")
            <*> (x .:? "PSTNEnabled")
            <*> (x .:? "AddressBookArn")
            <*> (x .:? "DistanceUnit")
            <*> (x .:? "Locale")
            <*> (x .:? "Address")
            <*> (x .:? "ProfileArn")
            <*> (x .:? "WakeWord")
            <*> (x .:? "MeetingRoomConfiguration")
            <*> (x .:? "ProfileName")
            <*> (x .:? "TemperatureUnit")
            <*> (x .:? "Timezone")
            <*> (x .:? "MaxVolumeLimit")
            <*> (x .:? "IsDefault")
      )

instance Hashable Profile

instance NFData Profile
