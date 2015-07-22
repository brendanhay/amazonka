{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateUserProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user profile.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateUserProfile.html>
module Network.AWS.OpsWorks.CreateUserProfile
    (
    -- * Request
      CreateUserProfile
    -- ** Request constructor
    , createUserProfile
    -- ** Request lenses
    , cuprqSSHUsername
    , cuprqSSHPublicKey
    , cuprqAllowSelfManagement
    , cuprqIAMUserARN

    -- * Response
    , CreateUserProfileResponse
    -- ** Response constructor
    , createUserProfileResponse
    -- ** Response lenses
    , cuprsIAMUserARN
    , cuprsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createUserProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cuprqSSHUsername'
--
-- * 'cuprqSSHPublicKey'
--
-- * 'cuprqAllowSelfManagement'
--
-- * 'cuprqIAMUserARN'
data CreateUserProfile = CreateUserProfile'
    { _cuprqSSHUsername         :: !(Maybe Text)
    , _cuprqSSHPublicKey        :: !(Maybe Text)
    , _cuprqAllowSelfManagement :: !(Maybe Bool)
    , _cuprqIAMUserARN          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateUserProfile' smart constructor.
createUserProfile :: Text -> CreateUserProfile
createUserProfile pIAMUserARN =
    CreateUserProfile'
    { _cuprqSSHUsername = Nothing
    , _cuprqSSHPublicKey = Nothing
    , _cuprqAllowSelfManagement = Nothing
    , _cuprqIAMUserARN = pIAMUserARN
    }

-- | The user\'s SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], \'-\', and \'_\'. If the specified name includes other
-- punctuation marks, AWS OpsWorks removes them. For example, @my.name@
-- will be changed to @myname@. If you do not specify an SSH user name, AWS
-- OpsWorks generates one from the IAM user name.
cuprqSSHUsername :: Lens' CreateUserProfile (Maybe Text)
cuprqSSHUsername = lens _cuprqSSHUsername (\ s a -> s{_cuprqSSHUsername = a});

-- | The user\'s public SSH key.
cuprqSSHPublicKey :: Lens' CreateUserProfile (Maybe Text)
cuprqSSHPublicKey = lens _cuprqSSHPublicKey (\ s a -> s{_cuprqSSHPublicKey = a});

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User\'s Public SSH Key>.
cuprqAllowSelfManagement :: Lens' CreateUserProfile (Maybe Bool)
cuprqAllowSelfManagement = lens _cuprqAllowSelfManagement (\ s a -> s{_cuprqAllowSelfManagement = a});

-- | The user\'s IAM ARN.
cuprqIAMUserARN :: Lens' CreateUserProfile Text
cuprqIAMUserARN = lens _cuprqIAMUserARN (\ s a -> s{_cuprqIAMUserARN = a});

instance AWSRequest CreateUserProfile where
        type Sv CreateUserProfile = OpsWorks
        type Rs CreateUserProfile = CreateUserProfileResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateUserProfileResponse' <$>
                   (x .?> "IamUserArn") <*> (pure (fromEnum s)))

instance ToHeaders CreateUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.CreateUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUserProfile where
        toJSON CreateUserProfile'{..}
          = object
              ["SshUsername" .= _cuprqSSHUsername,
               "SshPublicKey" .= _cuprqSSHPublicKey,
               "AllowSelfManagement" .= _cuprqAllowSelfManagement,
               "IamUserArn" .= _cuprqIAMUserARN]

instance ToPath CreateUserProfile where
        toPath = const "/"

instance ToQuery CreateUserProfile where
        toQuery = const mempty

-- | Contains the response to a @CreateUserProfile@ request.
--
-- /See:/ 'createUserProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cuprsIAMUserARN'
--
-- * 'cuprsStatus'
data CreateUserProfileResponse = CreateUserProfileResponse'
    { _cuprsIAMUserARN :: !(Maybe Text)
    , _cuprsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateUserProfileResponse' smart constructor.
createUserProfileResponse :: Int -> CreateUserProfileResponse
createUserProfileResponse pStatus =
    CreateUserProfileResponse'
    { _cuprsIAMUserARN = Nothing
    , _cuprsStatus = pStatus
    }

-- | The user\'s IAM ARN.
cuprsIAMUserARN :: Lens' CreateUserProfileResponse (Maybe Text)
cuprsIAMUserARN = lens _cuprsIAMUserARN (\ s a -> s{_cuprsIAMUserARN = a});

-- | FIXME: Undocumented member.
cuprsStatus :: Lens' CreateUserProfileResponse Int
cuprsStatus = lens _cuprsStatus (\ s a -> s{_cuprsStatus = a});
