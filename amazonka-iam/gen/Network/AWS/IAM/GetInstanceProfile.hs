{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetInstanceProfile
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about the specified instance profile, including
-- the instance profile\'s path, GUID, ARN, and role. For more information
-- about instance profiles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
-- For more information about ARNs, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html#Identifiers_ARNs ARNs>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetInstanceProfile.html>
module Network.AWS.IAM.GetInstanceProfile
    (
    -- * Request
      GetInstanceProfile
    -- ** Request constructor
    , getInstanceProfile
    -- ** Request lenses
    , gipInstanceProfileName

    -- * Response
    , GetInstanceProfileResponse
    -- ** Response constructor
    , getInstanceProfileResponse
    -- ** Response lenses
    , giprInstanceProfile
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipInstanceProfileName'
newtype GetInstanceProfile = GetInstanceProfile'{_gipInstanceProfileName :: Text} deriving (Eq, Read, Show)

-- | 'GetInstanceProfile' smart constructor.
getInstanceProfile :: Text -> GetInstanceProfile
getInstanceProfile pInstanceProfileName = GetInstanceProfile'{_gipInstanceProfileName = pInstanceProfileName};

-- | The name of the instance profile to get information about.
gipInstanceProfileName :: Lens' GetInstanceProfile Text
gipInstanceProfileName = lens _gipInstanceProfileName (\ s a -> s{_gipInstanceProfileName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest GetInstanceProfile where
        type Sv GetInstanceProfile = IAM
        type Rs GetInstanceProfile =
             GetInstanceProfileResponse
        request = post
        response
          = receiveXMLWrapper "GetInstanceProfileResult"
              (\ s h x ->
                 GetInstanceProfileResponse' <$>
                   (x .@ "InstanceProfile"))

instance ToHeaders GetInstanceProfile where
        toHeaders = const mempty

instance ToPath GetInstanceProfile where
        toPath = const "/"

instance ToQuery GetInstanceProfile where
        toQuery GetInstanceProfile'{..}
          = mconcat
              ["Action" =: ("GetInstanceProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "InstanceProfileName" =: _gipInstanceProfileName]

-- | /See:/ 'getInstanceProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprInstanceProfile'
newtype GetInstanceProfileResponse = GetInstanceProfileResponse'{_giprInstanceProfile :: InstanceProfile} deriving (Eq, Read, Show)

-- | 'GetInstanceProfileResponse' smart constructor.
getInstanceProfileResponse :: InstanceProfile -> GetInstanceProfileResponse
getInstanceProfileResponse pInstanceProfile = GetInstanceProfileResponse'{_giprInstanceProfile = pInstanceProfile};

-- | Information about the instance profile.
giprInstanceProfile :: Lens' GetInstanceProfileResponse InstanceProfile
giprInstanceProfile = lens _giprInstanceProfile (\ s a -> s{_giprInstanceProfile = a});
