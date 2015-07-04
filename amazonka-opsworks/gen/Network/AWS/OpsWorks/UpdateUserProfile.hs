{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.OpsWorks.UpdateUserProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates a specified user profile.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateUserProfile.html>
module Network.AWS.OpsWorks.UpdateUserProfile
    (
    -- * Request
      UpdateUserProfile
    -- ** Request constructor
    , updateUserProfile
    -- ** Request lenses
    , uupSSHUsername
    , uupSSHPublicKey
    , uupAllowSelfManagement
    , uupIAMUserARN

    -- * Response
    , UpdateUserProfileResponse
    -- ** Response constructor
    , updateUserProfileResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateUserProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uupSSHUsername'
--
-- * 'uupSSHPublicKey'
--
-- * 'uupAllowSelfManagement'
--
-- * 'uupIAMUserARN'
data UpdateUserProfile = UpdateUserProfile'
    { _uupSSHUsername         :: !(Maybe Text)
    , _uupSSHPublicKey        :: !(Maybe Text)
    , _uupAllowSelfManagement :: !(Maybe Bool)
    , _uupIAMUserARN          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateUserProfile' smart constructor.
updateUserProfile :: Text -> UpdateUserProfile
updateUserProfile pIAMUserARN =
    UpdateUserProfile'
    { _uupSSHUsername = Nothing
    , _uupSSHPublicKey = Nothing
    , _uupAllowSelfManagement = Nothing
    , _uupIAMUserARN = pIAMUserARN
    }

-- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks removes them. For example, @my.name@
-- will be changed to @myname@. If you do not specify an SSH user name, AWS
-- OpsWorks generates one from the IAM user name.
uupSSHUsername :: Lens' UpdateUserProfile (Maybe Text)
uupSSHUsername = lens _uupSSHUsername (\ s a -> s{_uupSSHUsername = a});

-- | The user\'s new SSH public key.
uupSSHPublicKey :: Lens' UpdateUserProfile (Maybe Text)
uupSSHPublicKey = lens _uupSSHPublicKey (\ s a -> s{_uupSSHPublicKey = a});

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
uupAllowSelfManagement :: Lens' UpdateUserProfile (Maybe Bool)
uupAllowSelfManagement = lens _uupAllowSelfManagement (\ s a -> s{_uupAllowSelfManagement = a});

-- | The user IAM ARN.
uupIAMUserARN :: Lens' UpdateUserProfile Text
uupIAMUserARN = lens _uupIAMUserARN (\ s a -> s{_uupIAMUserARN = a});

instance AWSRequest UpdateUserProfile where
        type Sv UpdateUserProfile = OpsWorks
        type Rs UpdateUserProfile = UpdateUserProfileResponse
        request = postJSON
        response = receiveNull UpdateUserProfileResponse'

instance ToHeaders UpdateUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserProfile where
        toJSON UpdateUserProfile'{..}
          = object
              ["SshUsername" .= _uupSSHUsername,
               "SshPublicKey" .= _uupSSHPublicKey,
               "AllowSelfManagement" .= _uupAllowSelfManagement,
               "IamUserArn" .= _uupIAMUserARN]

instance ToPath UpdateUserProfile where
        toPath = const "/"

instance ToQuery UpdateUserProfile where
        toQuery = const mempty

-- | /See:/ 'updateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse =
    UpdateUserProfileResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateUserProfileResponse' smart constructor.
updateUserProfileResponse :: UpdateUserProfileResponse
updateUserProfileResponse = UpdateUserProfileResponse'
