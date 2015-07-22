{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateLoginProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for the specified user.
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
    , ulprqPassword
    , ulprqPasswordResetRequired
    , ulprqUserName

    -- * Response
    , UpdateLoginProfileResponse
    -- ** Response constructor
    , updateLoginProfileResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateLoginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ulprqPassword'
--
-- * 'ulprqPasswordResetRequired'
--
-- * 'ulprqUserName'
data UpdateLoginProfile = UpdateLoginProfile'
    { _ulprqPassword              :: !(Maybe (Sensitive Text))
    , _ulprqPasswordResetRequired :: !(Maybe Bool)
    , _ulprqUserName              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateLoginProfile' smart constructor.
updateLoginProfile :: Text -> UpdateLoginProfile
updateLoginProfile pUserName =
    UpdateLoginProfile'
    { _ulprqPassword = Nothing
    , _ulprqPasswordResetRequired = Nothing
    , _ulprqUserName = pUserName
    }

-- | The new password for the specified user.
ulprqPassword :: Lens' UpdateLoginProfile (Maybe Text)
ulprqPassword = lens _ulprqPassword (\ s a -> s{_ulprqPassword = a}) . mapping _Sensitive;

-- | Require the specified user to set a new password on next sign-in.
ulprqPasswordResetRequired :: Lens' UpdateLoginProfile (Maybe Bool)
ulprqPasswordResetRequired = lens _ulprqPasswordResetRequired (\ s a -> s{_ulprqPasswordResetRequired = a});

-- | The name of the user whose password you want to update.
ulprqUserName :: Lens' UpdateLoginProfile Text
ulprqUserName = lens _ulprqUserName (\ s a -> s{_ulprqUserName = a});

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
               "Password" =: _ulprqPassword,
               "PasswordResetRequired" =:
                 _ulprqPasswordResetRequired,
               "UserName" =: _ulprqUserName]

-- | /See:/ 'updateLoginProfileResponse' smart constructor.
data UpdateLoginProfileResponse =
    UpdateLoginProfileResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateLoginProfileResponse' smart constructor.
updateLoginProfileResponse :: UpdateLoginProfileResponse
updateLoginProfileResponse = UpdateLoginProfileResponse'
