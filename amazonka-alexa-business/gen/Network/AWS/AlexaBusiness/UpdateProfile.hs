{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing room profile by room profile ARN.
--
--
module Network.AWS.AlexaBusiness.UpdateProfile
    (
    -- * Creating a Request
      updateProfile
    , UpdateProfile
    -- * Request Lenses
    , upSetupModeDisabled
    , upPSTNEnabled
    , upDistanceUnit
    , upAddress
    , upProfileARN
    , upWakeWord
    , upProfileName
    , upTemperatureUnit
    , upTimezone
    , upMaxVolumeLimit

    -- * Destructuring the Response
    , updateProfileResponse
    , UpdateProfileResponse
    -- * Response Lenses
    , uprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { _upSetupModeDisabled :: !(Maybe Bool)
  , _upPSTNEnabled       :: !(Maybe Bool)
  , _upDistanceUnit      :: !(Maybe DistanceUnit)
  , _upAddress           :: !(Maybe Text)
  , _upProfileARN        :: !(Maybe Text)
  , _upWakeWord          :: !(Maybe WakeWord)
  , _upProfileName       :: !(Maybe Text)
  , _upTemperatureUnit   :: !(Maybe TemperatureUnit)
  , _upTimezone          :: !(Maybe Text)
  , _upMaxVolumeLimit    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upSetupModeDisabled' - Whether the setup mode of the profile is enabled.
--
-- * 'upPSTNEnabled' - Whether the PSTN setting of the room profile is enabled.
--
-- * 'upDistanceUnit' - The updated distance unit for the room profile.
--
-- * 'upAddress' - The updated address for the room profile.
--
-- * 'upProfileARN' - The ARN of the room profile to update. Required.
--
-- * 'upWakeWord' - The updated wake word for the room profile.
--
-- * 'upProfileName' - The updated name for the room profile.
--
-- * 'upTemperatureUnit' - The updated temperature unit for the room profile.
--
-- * 'upTimezone' - The updated timezone for the room profile.
--
-- * 'upMaxVolumeLimit' - The updated maximum volume limit for the room profile.
updateProfile
    :: UpdateProfile
updateProfile =
  UpdateProfile'
    { _upSetupModeDisabled = Nothing
    , _upPSTNEnabled = Nothing
    , _upDistanceUnit = Nothing
    , _upAddress = Nothing
    , _upProfileARN = Nothing
    , _upWakeWord = Nothing
    , _upProfileName = Nothing
    , _upTemperatureUnit = Nothing
    , _upTimezone = Nothing
    , _upMaxVolumeLimit = Nothing
    }


-- | Whether the setup mode of the profile is enabled.
upSetupModeDisabled :: Lens' UpdateProfile (Maybe Bool)
upSetupModeDisabled = lens _upSetupModeDisabled (\ s a -> s{_upSetupModeDisabled = a})

-- | Whether the PSTN setting of the room profile is enabled.
upPSTNEnabled :: Lens' UpdateProfile (Maybe Bool)
upPSTNEnabled = lens _upPSTNEnabled (\ s a -> s{_upPSTNEnabled = a})

-- | The updated distance unit for the room profile.
upDistanceUnit :: Lens' UpdateProfile (Maybe DistanceUnit)
upDistanceUnit = lens _upDistanceUnit (\ s a -> s{_upDistanceUnit = a})

-- | The updated address for the room profile.
upAddress :: Lens' UpdateProfile (Maybe Text)
upAddress = lens _upAddress (\ s a -> s{_upAddress = a})

-- | The ARN of the room profile to update. Required.
upProfileARN :: Lens' UpdateProfile (Maybe Text)
upProfileARN = lens _upProfileARN (\ s a -> s{_upProfileARN = a})

-- | The updated wake word for the room profile.
upWakeWord :: Lens' UpdateProfile (Maybe WakeWord)
upWakeWord = lens _upWakeWord (\ s a -> s{_upWakeWord = a})

-- | The updated name for the room profile.
upProfileName :: Lens' UpdateProfile (Maybe Text)
upProfileName = lens _upProfileName (\ s a -> s{_upProfileName = a})

-- | The updated temperature unit for the room profile.
upTemperatureUnit :: Lens' UpdateProfile (Maybe TemperatureUnit)
upTemperatureUnit = lens _upTemperatureUnit (\ s a -> s{_upTemperatureUnit = a})

-- | The updated timezone for the room profile.
upTimezone :: Lens' UpdateProfile (Maybe Text)
upTimezone = lens _upTimezone (\ s a -> s{_upTimezone = a})

-- | The updated maximum volume limit for the room profile.
upMaxVolumeLimit :: Lens' UpdateProfile (Maybe Int)
upMaxVolumeLimit = lens _upMaxVolumeLimit (\ s a -> s{_upMaxVolumeLimit = a})

instance AWSRequest UpdateProfile where
        type Rs UpdateProfile = UpdateProfileResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateProfileResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateProfile where

instance NFData UpdateProfile where

instance ToHeaders UpdateProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.UpdateProfile" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateProfile where
        toJSON UpdateProfile'{..}
          = object
              (catMaybes
                 [("SetupModeDisabled" .=) <$> _upSetupModeDisabled,
                  ("PSTNEnabled" .=) <$> _upPSTNEnabled,
                  ("DistanceUnit" .=) <$> _upDistanceUnit,
                  ("Address" .=) <$> _upAddress,
                  ("ProfileArn" .=) <$> _upProfileARN,
                  ("WakeWord" .=) <$> _upWakeWord,
                  ("ProfileName" .=) <$> _upProfileName,
                  ("TemperatureUnit" .=) <$> _upTemperatureUnit,
                  ("Timezone" .=) <$> _upTimezone,
                  ("MaxVolumeLimit" .=) <$> _upMaxVolumeLimit])

instance ToPath UpdateProfile where
        toPath = const "/"

instance ToQuery UpdateProfile where
        toQuery = const mempty

-- | /See:/ 'updateProfileResponse' smart constructor.
newtype UpdateProfileResponse = UpdateProfileResponse'
  { _uprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsResponseStatus' - -- | The response status code.
updateProfileResponse
    :: Int -- ^ 'uprsResponseStatus'
    -> UpdateProfileResponse
updateProfileResponse pResponseStatus_ =
  UpdateProfileResponse' {_uprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uprsResponseStatus :: Lens' UpdateProfileResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\ s a -> s{_uprsResponseStatus = a})

instance NFData UpdateProfileResponse where
