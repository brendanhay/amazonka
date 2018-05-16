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
-- Module      : Network.AWS.AlexaBusiness.CreateProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new room profile with the specified details.
--
--
module Network.AWS.AlexaBusiness.CreateProfile
    (
    -- * Creating a Request
      createProfile
    , CreateProfile
    -- * Request Lenses
    , cpSetupModeDisabled
    , cpPSTNEnabled
    , cpClientRequestToken
    , cpMaxVolumeLimit
    , cpProfileName
    , cpTimezone
    , cpAddress
    , cpDistanceUnit
    , cpTemperatureUnit
    , cpWakeWord

    -- * Destructuring the Response
    , createProfileResponse
    , CreateProfileResponse
    -- * Response Lenses
    , cprsProfileARN
    , cprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProfile' smart constructor.
data CreateProfile = CreateProfile'
  { _cpSetupModeDisabled  :: !(Maybe Bool)
  , _cpPSTNEnabled        :: !(Maybe Bool)
  , _cpClientRequestToken :: !(Maybe Text)
  , _cpMaxVolumeLimit     :: !(Maybe Int)
  , _cpProfileName        :: !Text
  , _cpTimezone           :: !Text
  , _cpAddress            :: !Text
  , _cpDistanceUnit       :: !DistanceUnit
  , _cpTemperatureUnit    :: !TemperatureUnit
  , _cpWakeWord           :: !WakeWord
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpSetupModeDisabled' - Whether room profile setup is enabled.
--
-- * 'cpPSTNEnabled' - Whether PSTN calling is enabled.
--
-- * 'cpClientRequestToken' - The user-specified token that is used during the creation of a profile.
--
-- * 'cpMaxVolumeLimit' - The maximum volume limit for a room profile.
--
-- * 'cpProfileName' - The name of a room profile.
--
-- * 'cpTimezone' - The time zone used by a room profile.
--
-- * 'cpAddress' - The valid address for the room.
--
-- * 'cpDistanceUnit' - The distance unit to be used by devices in the profile.
--
-- * 'cpTemperatureUnit' - The temperature unit to be used by devices in the profile.
--
-- * 'cpWakeWord' - A wake word for Alexa, Echo, Amazon, or a computer.
createProfile
    :: Text -- ^ 'cpProfileName'
    -> Text -- ^ 'cpTimezone'
    -> Text -- ^ 'cpAddress'
    -> DistanceUnit -- ^ 'cpDistanceUnit'
    -> TemperatureUnit -- ^ 'cpTemperatureUnit'
    -> WakeWord -- ^ 'cpWakeWord'
    -> CreateProfile
createProfile pProfileName_ pTimezone_ pAddress_ pDistanceUnit_ pTemperatureUnit_ pWakeWord_ =
  CreateProfile'
    { _cpSetupModeDisabled = Nothing
    , _cpPSTNEnabled = Nothing
    , _cpClientRequestToken = Nothing
    , _cpMaxVolumeLimit = Nothing
    , _cpProfileName = pProfileName_
    , _cpTimezone = pTimezone_
    , _cpAddress = pAddress_
    , _cpDistanceUnit = pDistanceUnit_
    , _cpTemperatureUnit = pTemperatureUnit_
    , _cpWakeWord = pWakeWord_
    }


-- | Whether room profile setup is enabled.
cpSetupModeDisabled :: Lens' CreateProfile (Maybe Bool)
cpSetupModeDisabled = lens _cpSetupModeDisabled (\ s a -> s{_cpSetupModeDisabled = a})

-- | Whether PSTN calling is enabled.
cpPSTNEnabled :: Lens' CreateProfile (Maybe Bool)
cpPSTNEnabled = lens _cpPSTNEnabled (\ s a -> s{_cpPSTNEnabled = a})

-- | The user-specified token that is used during the creation of a profile.
cpClientRequestToken :: Lens' CreateProfile (Maybe Text)
cpClientRequestToken = lens _cpClientRequestToken (\ s a -> s{_cpClientRequestToken = a})

-- | The maximum volume limit for a room profile.
cpMaxVolumeLimit :: Lens' CreateProfile (Maybe Int)
cpMaxVolumeLimit = lens _cpMaxVolumeLimit (\ s a -> s{_cpMaxVolumeLimit = a})

-- | The name of a room profile.
cpProfileName :: Lens' CreateProfile Text
cpProfileName = lens _cpProfileName (\ s a -> s{_cpProfileName = a})

-- | The time zone used by a room profile.
cpTimezone :: Lens' CreateProfile Text
cpTimezone = lens _cpTimezone (\ s a -> s{_cpTimezone = a})

-- | The valid address for the room.
cpAddress :: Lens' CreateProfile Text
cpAddress = lens _cpAddress (\ s a -> s{_cpAddress = a})

-- | The distance unit to be used by devices in the profile.
cpDistanceUnit :: Lens' CreateProfile DistanceUnit
cpDistanceUnit = lens _cpDistanceUnit (\ s a -> s{_cpDistanceUnit = a})

-- | The temperature unit to be used by devices in the profile.
cpTemperatureUnit :: Lens' CreateProfile TemperatureUnit
cpTemperatureUnit = lens _cpTemperatureUnit (\ s a -> s{_cpTemperatureUnit = a})

-- | A wake word for Alexa, Echo, Amazon, or a computer.
cpWakeWord :: Lens' CreateProfile WakeWord
cpWakeWord = lens _cpWakeWord (\ s a -> s{_cpWakeWord = a})

instance AWSRequest CreateProfile where
        type Rs CreateProfile = CreateProfileResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 CreateProfileResponse' <$>
                   (x .?> "ProfileArn") <*> (pure (fromEnum s)))

instance Hashable CreateProfile where

instance NFData CreateProfile where

instance ToHeaders CreateProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.CreateProfile" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProfile where
        toJSON CreateProfile'{..}
          = object
              (catMaybes
                 [("SetupModeDisabled" .=) <$> _cpSetupModeDisabled,
                  ("PSTNEnabled" .=) <$> _cpPSTNEnabled,
                  ("ClientRequestToken" .=) <$> _cpClientRequestToken,
                  ("MaxVolumeLimit" .=) <$> _cpMaxVolumeLimit,
                  Just ("ProfileName" .= _cpProfileName),
                  Just ("Timezone" .= _cpTimezone),
                  Just ("Address" .= _cpAddress),
                  Just ("DistanceUnit" .= _cpDistanceUnit),
                  Just ("TemperatureUnit" .= _cpTemperatureUnit),
                  Just ("WakeWord" .= _cpWakeWord)])

instance ToPath CreateProfile where
        toPath = const "/"

instance ToQuery CreateProfile where
        toQuery = const mempty

-- | /See:/ 'createProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { _cprsProfileARN     :: !(Maybe Text)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsProfileARN' - The ARN of the newly created room profile in the response.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createProfileResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreateProfileResponse
createProfileResponse pResponseStatus_ =
  CreateProfileResponse'
    {_cprsProfileARN = Nothing, _cprsResponseStatus = pResponseStatus_}


-- | The ARN of the newly created room profile in the response.
cprsProfileARN :: Lens' CreateProfileResponse (Maybe Text)
cprsProfileARN = lens _cprsProfileARN (\ s a -> s{_cprsProfileARN = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreateProfileResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreateProfileResponse where
