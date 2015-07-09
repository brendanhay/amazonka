{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateMyUserProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Updates a user\'s SSH public key.
--
-- __Required Permissions__: To use this action, an IAM user must have
-- self-management enabled or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateMyUserProfile.html>
module Network.AWS.OpsWorks.UpdateMyUserProfile
    (
    -- * Request
      UpdateMyUserProfile
    -- ** Request constructor
    , updateMyUserProfile
    -- ** Request lenses
    , umupSSHPublicKey

    -- * Response
    , UpdateMyUserProfileResponse
    -- ** Response constructor
    , updateMyUserProfileResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateMyUserProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umupSSHPublicKey'
newtype UpdateMyUserProfile = UpdateMyUserProfile'
    { _umupSSHPublicKey :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateMyUserProfile' smart constructor.
updateMyUserProfile :: UpdateMyUserProfile
updateMyUserProfile =
    UpdateMyUserProfile'
    { _umupSSHPublicKey = Nothing
    }

-- | The user\'s SSH public key.
umupSSHPublicKey :: Lens' UpdateMyUserProfile (Maybe Text)
umupSSHPublicKey = lens _umupSSHPublicKey (\ s a -> s{_umupSSHPublicKey = a});

instance AWSRequest UpdateMyUserProfile where
        type Sv UpdateMyUserProfile = OpsWorks
        type Rs UpdateMyUserProfile =
             UpdateMyUserProfileResponse
        request = postJSON
        response = receiveNull UpdateMyUserProfileResponse'

instance ToHeaders UpdateMyUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateMyUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateMyUserProfile where
        toJSON UpdateMyUserProfile'{..}
          = object ["SshPublicKey" .= _umupSSHPublicKey]

instance ToPath UpdateMyUserProfile where
        toPath = const "/"

instance ToQuery UpdateMyUserProfile where
        toQuery = const mempty

-- | /See:/ 'updateMyUserProfileResponse' smart constructor.
data UpdateMyUserProfileResponse =
    UpdateMyUserProfileResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateMyUserProfileResponse' smart constructor.
updateMyUserProfileResponse :: UpdateMyUserProfileResponse
updateMyUserProfileResponse = UpdateMyUserProfileResponse'
