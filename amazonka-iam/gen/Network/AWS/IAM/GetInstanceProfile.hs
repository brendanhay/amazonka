{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetInstanceProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified instance profile, including
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
    , giprsStatus
    , giprsInstanceProfile
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getInstanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipInstanceProfileName'
newtype GetInstanceProfile = GetInstanceProfile'
    { _gipInstanceProfileName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetInstanceProfile' smart constructor.
getInstanceProfile :: Text -> GetInstanceProfile
getInstanceProfile pInstanceProfileName_ =
    GetInstanceProfile'
    { _gipInstanceProfileName = pInstanceProfileName_
    }

-- | The name of the instance profile to get information about.
gipInstanceProfileName :: Lens' GetInstanceProfile Text
gipInstanceProfileName = lens _gipInstanceProfileName (\ s a -> s{_gipInstanceProfileName = a});

instance AWSRequest GetInstanceProfile where
        type Sv GetInstanceProfile = IAM
        type Rs GetInstanceProfile =
             GetInstanceProfileResponse
        request = postQuery
        response
          = receiveXMLWrapper "GetInstanceProfileResult"
              (\ s h x ->
                 GetInstanceProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "InstanceProfile"))

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

-- | Contains the response to a successful GetInstanceProfile request.
--
-- /See:/ 'getInstanceProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprsStatus'
--
-- * 'giprsInstanceProfile'
data GetInstanceProfileResponse = GetInstanceProfileResponse'
    { _giprsStatus          :: !Int
    , _giprsInstanceProfile :: !InstanceProfile
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetInstanceProfileResponse' smart constructor.
getInstanceProfileResponse :: Int -> InstanceProfile -> GetInstanceProfileResponse
getInstanceProfileResponse pStatus_ pInstanceProfile_ =
    GetInstanceProfileResponse'
    { _giprsStatus = pStatus_
    , _giprsInstanceProfile = pInstanceProfile_
    }

-- | FIXME: Undocumented member.
giprsStatus :: Lens' GetInstanceProfileResponse Int
giprsStatus = lens _giprsStatus (\ s a -> s{_giprsStatus = a});

-- | Information about the instance profile.
giprsInstanceProfile :: Lens' GetInstanceProfileResponse InstanceProfile
giprsInstanceProfile = lens _giprsInstanceProfile (\ s a -> s{_giprsInstanceProfile = a});
