{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.CreateLoginProfile
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

-- | Creates a password for the specified user, giving the user the ability
-- to access AWS services through the AWS Management Console. For more
-- information about managing passwords, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateLoginProfile.html>
module Network.AWS.IAM.CreateLoginProfile
    (
    -- * Request
      CreateLoginProfile
    -- ** Request constructor
    , createLoginProfile
    -- ** Request lenses
    , clpPasswordResetRequired
    , clpUserName
    , clpPassword

    -- * Response
    , CreateLoginProfileResponse
    -- ** Response constructor
    , createLoginProfileResponse
    -- ** Response lenses
    , clprLoginProfile
    , clprStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLoginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clpPasswordResetRequired'
--
-- * 'clpUserName'
--
-- * 'clpPassword'
data CreateLoginProfile = CreateLoginProfile'
    { _clpPasswordResetRequired :: Maybe Bool
    , _clpUserName              :: Text
    , _clpPassword              :: Sensitive Text
    } deriving (Eq,Read,Show)

-- | 'CreateLoginProfile' smart constructor.
createLoginProfile :: Text -> Text -> CreateLoginProfile
createLoginProfile pUserName pPassword =
    CreateLoginProfile'
    { _clpPasswordResetRequired = Nothing
    , _clpUserName = pUserName
    , _clpPassword = _Sensitive # pPassword
    }

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
clpPasswordResetRequired :: Lens' CreateLoginProfile (Maybe Bool)
clpPasswordResetRequired = lens _clpPasswordResetRequired (\ s a -> s{_clpPasswordResetRequired = a});

-- | The name of the user to create a password for.
clpUserName :: Lens' CreateLoginProfile Text
clpUserName = lens _clpUserName (\ s a -> s{_clpUserName = a});

-- | The new password for the user.
clpPassword :: Lens' CreateLoginProfile Text
clpPassword = lens _clpPassword (\ s a -> s{_clpPassword = a}) . _Sensitive;

instance AWSRequest CreateLoginProfile where
        type Sv CreateLoginProfile = IAM
        type Rs CreateLoginProfile =
             CreateLoginProfileResponse
        request = post
        response
          = receiveXMLWrapper "CreateLoginProfileResult"
              (\ s h x ->
                 CreateLoginProfileResponse' <$>
                   (x .@ "LoginProfile") <*> (pure (fromEnum s)))

instance ToHeaders CreateLoginProfile where
        toHeaders = const mempty

instance ToPath CreateLoginProfile where
        toPath = const "/"

instance ToQuery CreateLoginProfile where
        toQuery CreateLoginProfile'{..}
          = mconcat
              ["Action" =: ("CreateLoginProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PasswordResetRequired" =: _clpPasswordResetRequired,
               "UserName" =: _clpUserName,
               "Password" =: _clpPassword]

-- | Contains the response to a successful CreateLoginProfile request.
--
-- /See:/ 'createLoginProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clprLoginProfile'
--
-- * 'clprStatus'
data CreateLoginProfileResponse = CreateLoginProfileResponse'
    { _clprLoginProfile :: LoginProfile
    , _clprStatus       :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateLoginProfileResponse' smart constructor.
createLoginProfileResponse :: LoginProfile -> Int -> CreateLoginProfileResponse
createLoginProfileResponse pLoginProfile pStatus =
    CreateLoginProfileResponse'
    { _clprLoginProfile = pLoginProfile
    , _clprStatus = pStatus
    }

-- | The user name and password create date.
clprLoginProfile :: Lens' CreateLoginProfileResponse LoginProfile
clprLoginProfile = lens _clprLoginProfile (\ s a -> s{_clprLoginProfile = a});

-- | FIXME: Undocumented member.
clprStatus :: Lens' CreateLoginProfileResponse Int
clprStatus = lens _clprStatus (\ s a -> s{_clprStatus = a});
