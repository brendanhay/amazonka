{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateLoginProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a password for the specified user, giving the user the ability
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
    , clprqPasswordResetRequired
    , clprqUserName
    , clprqPassword

    -- * Response
    , CreateLoginProfileResponse
    -- ** Response constructor
    , createLoginProfileResponse
    -- ** Response lenses
    , clprsStatus
    , clprsLoginProfile
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLoginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clprqPasswordResetRequired'
--
-- * 'clprqUserName'
--
-- * 'clprqPassword'
data CreateLoginProfile = CreateLoginProfile'
    { _clprqPasswordResetRequired :: !(Maybe Bool)
    , _clprqUserName              :: !Text
    , _clprqPassword              :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoginProfile' smart constructor.
createLoginProfile :: Text -> Text -> CreateLoginProfile
createLoginProfile pUserName pPassword =
    CreateLoginProfile'
    { _clprqPasswordResetRequired = Nothing
    , _clprqUserName = pUserName
    , _clprqPassword = _Sensitive # pPassword
    }

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
clprqPasswordResetRequired :: Lens' CreateLoginProfile (Maybe Bool)
clprqPasswordResetRequired = lens _clprqPasswordResetRequired (\ s a -> s{_clprqPasswordResetRequired = a});

-- | The name of the user to create a password for.
clprqUserName :: Lens' CreateLoginProfile Text
clprqUserName = lens _clprqUserName (\ s a -> s{_clprqUserName = a});

-- | The new password for the user.
clprqPassword :: Lens' CreateLoginProfile Text
clprqPassword = lens _clprqPassword (\ s a -> s{_clprqPassword = a}) . _Sensitive;

instance AWSRequest CreateLoginProfile where
        type Sv CreateLoginProfile = IAM
        type Rs CreateLoginProfile =
             CreateLoginProfileResponse
        request = post
        response
          = receiveXMLWrapper "CreateLoginProfileResult"
              (\ s h x ->
                 CreateLoginProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "LoginProfile"))

instance ToHeaders CreateLoginProfile where
        toHeaders = const mempty

instance ToPath CreateLoginProfile where
        toPath = const "/"

instance ToQuery CreateLoginProfile where
        toQuery CreateLoginProfile'{..}
          = mconcat
              ["Action" =: ("CreateLoginProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PasswordResetRequired" =:
                 _clprqPasswordResetRequired,
               "UserName" =: _clprqUserName,
               "Password" =: _clprqPassword]

-- | Contains the response to a successful CreateLoginProfile request.
--
-- /See:/ 'createLoginProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clprsStatus'
--
-- * 'clprsLoginProfile'
data CreateLoginProfileResponse = CreateLoginProfileResponse'
    { _clprsStatus       :: !Int
    , _clprsLoginProfile :: !LoginProfile
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLoginProfileResponse' smart constructor.
createLoginProfileResponse :: Int -> LoginProfile -> CreateLoginProfileResponse
createLoginProfileResponse pStatus pLoginProfile =
    CreateLoginProfileResponse'
    { _clprsStatus = pStatus
    , _clprsLoginProfile = pLoginProfile
    }

-- | FIXME: Undocumented member.
clprsStatus :: Lens' CreateLoginProfileResponse Int
clprsStatus = lens _clprsStatus (\ s a -> s{_clprsStatus = a});

-- | The user name and password create date.
clprsLoginProfile :: Lens' CreateLoginProfileResponse LoginProfile
clprsLoginProfile = lens _clprsLoginProfile (\ s a -> s{_clprsLoginProfile = a});
