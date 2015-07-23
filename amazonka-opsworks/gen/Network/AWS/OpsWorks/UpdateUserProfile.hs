{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateUserProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified user profile.
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
    , uuprqSSHUsername
    , uuprqSSHPublicKey
    , uuprqAllowSelfManagement
    , uuprqIAMUserARN

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
-- * 'uuprqSSHUsername'
--
-- * 'uuprqSSHPublicKey'
--
-- * 'uuprqAllowSelfManagement'
--
-- * 'uuprqIAMUserARN'
data UpdateUserProfile = UpdateUserProfile'
    { _uuprqSSHUsername         :: !(Maybe Text)
    , _uuprqSSHPublicKey        :: !(Maybe Text)
    , _uuprqAllowSelfManagement :: !(Maybe Bool)
    , _uuprqIAMUserARN          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateUserProfile' smart constructor.
updateUserProfile :: Text -> UpdateUserProfile
updateUserProfile pIAMUserARN_ =
    UpdateUserProfile'
    { _uuprqSSHUsername = Nothing
    , _uuprqSSHPublicKey = Nothing
    , _uuprqAllowSelfManagement = Nothing
    , _uuprqIAMUserARN = pIAMUserARN_
    }

-- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks removes them. For example, @my.name@
-- will be changed to @myname@. If you do not specify an SSH user name, AWS
-- OpsWorks generates one from the IAM user name.
uuprqSSHUsername :: Lens' UpdateUserProfile (Maybe Text)
uuprqSSHUsername = lens _uuprqSSHUsername (\ s a -> s{_uuprqSSHUsername = a});

-- | The user\'s new SSH public key.
uuprqSSHPublicKey :: Lens' UpdateUserProfile (Maybe Text)
uuprqSSHPublicKey = lens _uuprqSSHPublicKey (\ s a -> s{_uuprqSSHPublicKey = a});

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
uuprqAllowSelfManagement :: Lens' UpdateUserProfile (Maybe Bool)
uuprqAllowSelfManagement = lens _uuprqAllowSelfManagement (\ s a -> s{_uuprqAllowSelfManagement = a});

-- | The user IAM ARN.
uuprqIAMUserARN :: Lens' UpdateUserProfile Text
uuprqIAMUserARN = lens _uuprqIAMUserARN (\ s a -> s{_uuprqIAMUserARN = a});

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
              ["SshUsername" .= _uuprqSSHUsername,
               "SshPublicKey" .= _uuprqSSHPublicKey,
               "AllowSelfManagement" .= _uuprqAllowSelfManagement,
               "IamUserArn" .= _uuprqIAMUserARN]

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
