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
-- Module      : Network.AWS.OpsWorks.UpdateMyUserProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user's SSH public key.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have self-management enabled or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UpdateMyUserProfile
    (
    -- * Creating a Request
      updateMyUserProfile
    , UpdateMyUserProfile
    -- * Request Lenses
    , umupSSHPublicKey

    -- * Destructuring the Response
    , updateMyUserProfileResponse
    , UpdateMyUserProfileResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateMyUserProfile' smart constructor.
newtype UpdateMyUserProfile = UpdateMyUserProfile'
  { _umupSSHPublicKey :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMyUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umupSSHPublicKey' - The user's SSH public key.
updateMyUserProfile
    :: UpdateMyUserProfile
updateMyUserProfile = UpdateMyUserProfile' {_umupSSHPublicKey = Nothing}


-- | The user's SSH public key.
umupSSHPublicKey :: Lens' UpdateMyUserProfile (Maybe Text)
umupSSHPublicKey = lens _umupSSHPublicKey (\ s a -> s{_umupSSHPublicKey = a})

instance AWSRequest UpdateMyUserProfile where
        type Rs UpdateMyUserProfile =
             UpdateMyUserProfileResponse
        request = postJSON opsWorks
        response = receiveNull UpdateMyUserProfileResponse'

instance Hashable UpdateMyUserProfile where

instance NFData UpdateMyUserProfile where

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
          = object
              (catMaybes
                 [("SshPublicKey" .=) <$> _umupSSHPublicKey])

instance ToPath UpdateMyUserProfile where
        toPath = const "/"

instance ToQuery UpdateMyUserProfile where
        toQuery = const mempty

-- | /See:/ 'updateMyUserProfileResponse' smart constructor.
data UpdateMyUserProfileResponse =
  UpdateMyUserProfileResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMyUserProfileResponse' with the minimum fields required to make a request.
--
updateMyUserProfileResponse
    :: UpdateMyUserProfileResponse
updateMyUserProfileResponse = UpdateMyUserProfileResponse'


instance NFData UpdateMyUserProfileResponse where
