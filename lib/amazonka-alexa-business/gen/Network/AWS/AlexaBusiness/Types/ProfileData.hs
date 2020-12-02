{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ProfileData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ProfileData where

import Network.AWS.AlexaBusiness.Types.DistanceUnit
import Network.AWS.AlexaBusiness.Types.TemperatureUnit
import Network.AWS.AlexaBusiness.Types.WakeWord
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The data of a room profile.
--
--
--
-- /See:/ 'profileData' smart constructor.
data ProfileData = ProfileData'
  { _pdDistanceUnit ::
      !(Maybe DistanceUnit),
    _pdLocale :: !(Maybe Text),
    _pdAddress :: !(Maybe Text),
    _pdProfileARN :: !(Maybe Text),
    _pdWakeWord :: !(Maybe WakeWord),
    _pdProfileName :: !(Maybe Text),
    _pdTemperatureUnit :: !(Maybe TemperatureUnit),
    _pdTimezone :: !(Maybe Text),
    _pdIsDefault :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProfileData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdDistanceUnit' - The distance unit of a room profile.
--
-- * 'pdLocale' - The locale of a room profile. (This is currently available only to a limited preview audience.)
--
-- * 'pdAddress' - The address of a room profile.
--
-- * 'pdProfileARN' - The ARN of a room profile.
--
-- * 'pdWakeWord' - The wake word of a room profile.
--
-- * 'pdProfileName' - The name of a room profile.
--
-- * 'pdTemperatureUnit' - The temperature unit of a room profile.
--
-- * 'pdTimezone' - The time zone of a room profile.
--
-- * 'pdIsDefault' - Retrieves if the profile data is default or not.
profileData ::
  ProfileData
profileData =
  ProfileData'
    { _pdDistanceUnit = Nothing,
      _pdLocale = Nothing,
      _pdAddress = Nothing,
      _pdProfileARN = Nothing,
      _pdWakeWord = Nothing,
      _pdProfileName = Nothing,
      _pdTemperatureUnit = Nothing,
      _pdTimezone = Nothing,
      _pdIsDefault = Nothing
    }

-- | The distance unit of a room profile.
pdDistanceUnit :: Lens' ProfileData (Maybe DistanceUnit)
pdDistanceUnit = lens _pdDistanceUnit (\s a -> s {_pdDistanceUnit = a})

-- | The locale of a room profile. (This is currently available only to a limited preview audience.)
pdLocale :: Lens' ProfileData (Maybe Text)
pdLocale = lens _pdLocale (\s a -> s {_pdLocale = a})

-- | The address of a room profile.
pdAddress :: Lens' ProfileData (Maybe Text)
pdAddress = lens _pdAddress (\s a -> s {_pdAddress = a})

-- | The ARN of a room profile.
pdProfileARN :: Lens' ProfileData (Maybe Text)
pdProfileARN = lens _pdProfileARN (\s a -> s {_pdProfileARN = a})

-- | The wake word of a room profile.
pdWakeWord :: Lens' ProfileData (Maybe WakeWord)
pdWakeWord = lens _pdWakeWord (\s a -> s {_pdWakeWord = a})

-- | The name of a room profile.
pdProfileName :: Lens' ProfileData (Maybe Text)
pdProfileName = lens _pdProfileName (\s a -> s {_pdProfileName = a})

-- | The temperature unit of a room profile.
pdTemperatureUnit :: Lens' ProfileData (Maybe TemperatureUnit)
pdTemperatureUnit = lens _pdTemperatureUnit (\s a -> s {_pdTemperatureUnit = a})

-- | The time zone of a room profile.
pdTimezone :: Lens' ProfileData (Maybe Text)
pdTimezone = lens _pdTimezone (\s a -> s {_pdTimezone = a})

-- | Retrieves if the profile data is default or not.
pdIsDefault :: Lens' ProfileData (Maybe Bool)
pdIsDefault = lens _pdIsDefault (\s a -> s {_pdIsDefault = a})

instance FromJSON ProfileData where
  parseJSON =
    withObject
      "ProfileData"
      ( \x ->
          ProfileData'
            <$> (x .:? "DistanceUnit")
            <*> (x .:? "Locale")
            <*> (x .:? "Address")
            <*> (x .:? "ProfileArn")
            <*> (x .:? "WakeWord")
            <*> (x .:? "ProfileName")
            <*> (x .:? "TemperatureUnit")
            <*> (x .:? "Timezone")
            <*> (x .:? "IsDefault")
      )

instance Hashable ProfileData

instance NFData ProfileData
