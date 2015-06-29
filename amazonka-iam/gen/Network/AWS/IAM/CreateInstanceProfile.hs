{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.CreateInstanceProfile
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

-- | Creates a new instance profile. For information about instance profiles,
-- go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- For information about the number of instance profiles you can create,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateInstanceProfile.html>
module Network.AWS.IAM.CreateInstanceProfile
    (
    -- * Request
      CreateInstanceProfile
    -- ** Request constructor
    , createInstanceProfile
    -- ** Request lenses
    , cipPath
    , cipInstanceProfileName

    -- * Response
    , CreateInstanceProfileResponse
    -- ** Response constructor
    , createInstanceProfileResponse
    -- ** Response lenses
    , ciprStatus
    , ciprInstanceProfile
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createInstanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cipPath'
--
-- * 'cipInstanceProfileName'
data CreateInstanceProfile = CreateInstanceProfile'
    { _cipPath                :: !(Maybe Text)
    , _cipInstanceProfileName :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateInstanceProfile' smart constructor.
createInstanceProfile :: Text -> CreateInstanceProfile
createInstanceProfile pInstanceProfileName =
    CreateInstanceProfile'
    { _cipPath = Nothing
    , _cipInstanceProfileName = pInstanceProfileName
    }

-- | The path to the instance profile. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
cipPath :: Lens' CreateInstanceProfile (Maybe Text)
cipPath = lens _cipPath (\ s a -> s{_cipPath = a});

-- | The name of the instance profile to create.
cipInstanceProfileName :: Lens' CreateInstanceProfile Text
cipInstanceProfileName = lens _cipInstanceProfileName (\ s a -> s{_cipInstanceProfileName = a});

instance AWSRequest CreateInstanceProfile where
        type Sv CreateInstanceProfile = IAM
        type Rs CreateInstanceProfile =
             CreateInstanceProfileResponse
        request = post
        response
          = receiveXMLWrapper "CreateInstanceProfileResult"
              (\ s h x ->
                 CreateInstanceProfileResponse' <$>
                   (pure s) <*> (x .@ "InstanceProfile"))

instance ToHeaders CreateInstanceProfile where
        toHeaders = const mempty

instance ToPath CreateInstanceProfile where
        toPath = const "/"

instance ToQuery CreateInstanceProfile where
        toQuery CreateInstanceProfile'{..}
          = mconcat
              ["Action" =: ("CreateInstanceProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cipPath,
               "InstanceProfileName" =: _cipInstanceProfileName]

-- | Contains the response to a successful CreateInstanceProfile request.
--
-- /See:/ 'createInstanceProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciprStatus'
--
-- * 'ciprInstanceProfile'
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
    { _ciprStatus          :: !Status
    , _ciprInstanceProfile :: !InstanceProfile
    } deriving (Eq,Show)

-- | 'CreateInstanceProfileResponse' smart constructor.
createInstanceProfileResponse :: Status -> InstanceProfile -> CreateInstanceProfileResponse
createInstanceProfileResponse pStatus pInstanceProfile =
    CreateInstanceProfileResponse'
    { _ciprStatus = pStatus
    , _ciprInstanceProfile = pInstanceProfile
    }

-- | FIXME: Undocumented member.
ciprStatus :: Lens' CreateInstanceProfileResponse Status
ciprStatus = lens _ciprStatus (\ s a -> s{_ciprStatus = a});

-- | Information about the instance profile.
ciprInstanceProfile :: Lens' CreateInstanceProfileResponse InstanceProfile
ciprInstanceProfile = lens _ciprInstanceProfile (\ s a -> s{_ciprInstanceProfile = a});
