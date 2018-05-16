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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DeleteUserProfile
    (
    -- * Creating a Request
      deleteUserProfile
    , DeleteUserProfile
    -- * Request Lenses
    , dupIAMUserARN

    -- * Destructuring the Response
    , deleteUserProfileResponse
    , DeleteUserProfileResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserProfile' smart constructor.
newtype DeleteUserProfile = DeleteUserProfile'
  { _dupIAMUserARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupIAMUserARN' - The user's IAM ARN. This can also be a federated user's ARN.
deleteUserProfile
    :: Text -- ^ 'dupIAMUserARN'
    -> DeleteUserProfile
deleteUserProfile pIAMUserARN_ =
  DeleteUserProfile' {_dupIAMUserARN = pIAMUserARN_}


-- | The user's IAM ARN. This can also be a federated user's ARN.
dupIAMUserARN :: Lens' DeleteUserProfile Text
dupIAMUserARN = lens _dupIAMUserARN (\ s a -> s{_dupIAMUserARN = a})

instance AWSRequest DeleteUserProfile where
        type Rs DeleteUserProfile = DeleteUserProfileResponse
        request = postJSON opsWorks
        response = receiveNull DeleteUserProfileResponse'

instance Hashable DeleteUserProfile where

instance NFData DeleteUserProfile where

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
          = object
              (catMaybes [Just ("IamUserArn" .= _dupIAMUserARN)])

instance ToPath DeleteUserProfile where
        toPath = const "/"

instance ToQuery DeleteUserProfile where
        toQuery = const mempty

-- | /See:/ 'deleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse =
  DeleteUserProfileResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserProfileResponse' with the minimum fields required to make a request.
--
deleteUserProfileResponse
    :: DeleteUserProfileResponse
deleteUserProfileResponse = DeleteUserProfileResponse'


instance NFData DeleteUserProfileResponse where
