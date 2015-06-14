{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.UpdateLoginProfile
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

-- | Changes the password for the specified user.
--
-- Users can change their own passwords by calling ChangePassword. For more
-- information about modifying passwords, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateLoginProfile.html>
module Network.AWS.IAM.UpdateLoginProfile
    (
    -- * Request
      UpdateLoginProfile
    -- ** Request constructor
    , updateLoginProfile
    -- ** Request lenses
    , ulpPasswordResetRequired
    , ulpUserName
    , ulpPassword

    -- * Response
    , UpdateLoginProfileResponse
    -- ** Response constructor
    , updateLoginProfileResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'updateLoginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ulpPasswordResetRequired'
--
-- * 'ulpUserName'
--
-- * 'ulpPassword'
data UpdateLoginProfile = UpdateLoginProfile'{_ulpPasswordResetRequired :: Maybe Bool, _ulpUserName :: Text, _ulpPassword :: Sensitive Text} deriving (Eq, Read, Show)

-- | 'UpdateLoginProfile' smart constructor.
updateLoginProfile :: Text -> Text -> UpdateLoginProfile
updateLoginProfile pUserName pPassword = UpdateLoginProfile'{_ulpPasswordResetRequired = Nothing, _ulpUserName = pUserName, _ulpPassword = _Sensitive # pPassword};

-- | Require the specified user to set a new password on next sign-in.
ulpPasswordResetRequired :: Lens' UpdateLoginProfile (Maybe Bool)
ulpPasswordResetRequired = lens _ulpPasswordResetRequired (\ s a -> s{_ulpPasswordResetRequired = a});

-- | The name of the user whose password you want to update.
ulpUserName :: Lens' UpdateLoginProfile Text
ulpUserName = lens _ulpUserName (\ s a -> s{_ulpUserName = a});

-- | The new password for the specified user.
ulpPassword :: Lens' UpdateLoginProfile Text
ulpPassword = lens _ulpPassword (\ s a -> s{_ulpPassword = a}) . _Sensitive;

instance AWSRequest UpdateLoginProfile where
        type Sv UpdateLoginProfile = IAM
        type Rs UpdateLoginProfile =
             UpdateLoginProfileResponse
        request = post
        response = receiveNull UpdateLoginProfileResponse'

instance ToHeaders UpdateLoginProfile where
        toHeaders = const mempty

instance ToPath UpdateLoginProfile where
        toPath = const "/"

instance ToQuery UpdateLoginProfile where
        toQuery UpdateLoginProfile'{..}
          = mconcat
              ["Action" =: ("UpdateLoginProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PasswordResetRequired" =: _ulpPasswordResetRequired,
               "UserName" =: _ulpUserName,
               "Password" =: _ulpPassword]

-- | /See:/ 'updateLoginProfileResponse' smart constructor.
data UpdateLoginProfileResponse = UpdateLoginProfileResponse' deriving (Eq, Read, Show)

-- | 'UpdateLoginProfileResponse' smart constructor.
updateLoginProfileResponse :: UpdateLoginProfileResponse
updateLoginProfileResponse = UpdateLoginProfileResponse';
