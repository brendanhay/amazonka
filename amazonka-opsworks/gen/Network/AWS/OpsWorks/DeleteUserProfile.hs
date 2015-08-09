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
-- Module      : Network.AWS.OpsWorks.DeleteUserProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteUserProfile.html AWS API Reference> for DeleteUserProfile.
module Network.AWS.OpsWorks.DeleteUserProfile
    (
    -- * Creating a Request
      DeleteUserProfile
    , deleteUserProfile
    -- * Request Lenses
    , dupIAMUserARN

    -- * Destructuring the Response
    , DeleteUserProfileResponse
    , deleteUserProfileResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dupIAMUserARN'
newtype DeleteUserProfile = DeleteUserProfile'
    { _dupIAMUserARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteUserProfile' smart constructor.
deleteUserProfile :: Text -> DeleteUserProfile
deleteUserProfile pIAMUserARN_ = 
    DeleteUserProfile'
    { _dupIAMUserARN = pIAMUserARN_
    }

-- | The user\'s IAM ARN.
dupIAMUserARN :: Lens' DeleteUserProfile Text
dupIAMUserARN = lens _dupIAMUserARN (\ s a -> s{_dupIAMUserARN = a});

instance AWSRequest DeleteUserProfile where
        type Sv DeleteUserProfile = OpsWorks
        type Rs DeleteUserProfile = DeleteUserProfileResponse
        request = postJSON
        response = receiveNull DeleteUserProfileResponse'

instance ToHeaders DeleteUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeleteUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUserProfile where
        toJSON DeleteUserProfile'{..}
          = object ["IamUserArn" .= _dupIAMUserARN]

instance ToPath DeleteUserProfile where
        toPath = const "/"

instance ToQuery DeleteUserProfile where
        toQuery = const mempty

-- | /See:/ 'deleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse =
    DeleteUserProfileResponse' 
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteUserProfileResponse' smart constructor.
deleteUserProfileResponse :: DeleteUserProfileResponse
deleteUserProfileResponse = DeleteUserProfileResponse'
