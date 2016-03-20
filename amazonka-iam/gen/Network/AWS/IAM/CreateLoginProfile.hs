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
-- Module      : Network.AWS.IAM.CreateLoginProfile
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a password for the specified user, giving the user the ability
-- to access AWS services through the AWS Management Console. For more
-- information about managing passwords, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords>
-- in the /Using IAM/ guide.
module Network.AWS.IAM.CreateLoginProfile
    (
    -- * Creating a Request
      createLoginProfile
    , CreateLoginProfile
    -- * Request Lenses
    , clpPasswordResetRequired
    , clpUserName
    , clpPassword

    -- * Destructuring the Response
    , createLoginProfileResponse
    , CreateLoginProfileResponse
    -- * Response Lenses
    , clprsResponseStatus
    , clprsLoginProfile
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLoginProfile' smart constructor.
data CreateLoginProfile = CreateLoginProfile'
    { _clpPasswordResetRequired :: !(Maybe Bool)
    , _clpUserName              :: !Text
    , _clpPassword              :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clpPasswordResetRequired'
--
-- * 'clpUserName'
--
-- * 'clpPassword'
createLoginProfile
    :: Text -- ^ 'clpUserName'
    -> Text -- ^ 'clpPassword'
    -> CreateLoginProfile
createLoginProfile pUserName_ pPassword_ =
    CreateLoginProfile'
    { _clpPasswordResetRequired = Nothing
    , _clpUserName = pUserName_
    , _clpPassword = _Sensitive # pPassword_
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
        type Rs CreateLoginProfile =
             CreateLoginProfileResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreateLoginProfileResult"
              (\ s h x ->
                 CreateLoginProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "LoginProfile"))

instance Hashable CreateLoginProfile

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

-- | Contains the response to a successful < CreateLoginProfile> request.
--
-- /See:/ 'createLoginProfileResponse' smart constructor.
data CreateLoginProfileResponse = CreateLoginProfileResponse'
    { _clprsResponseStatus :: !Int
    , _clprsLoginProfile   :: !LoginProfile
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLoginProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clprsResponseStatus'
--
-- * 'clprsLoginProfile'
createLoginProfileResponse
    :: Int -- ^ 'clprsResponseStatus'
    -> LoginProfile -- ^ 'clprsLoginProfile'
    -> CreateLoginProfileResponse
createLoginProfileResponse pResponseStatus_ pLoginProfile_ =
    CreateLoginProfileResponse'
    { _clprsResponseStatus = pResponseStatus_
    , _clprsLoginProfile = pLoginProfile_
    }

-- | The response status code.
clprsResponseStatus :: Lens' CreateLoginProfileResponse Int
clprsResponseStatus = lens _clprsResponseStatus (\ s a -> s{_clprsResponseStatus = a});

-- | The user name and password create date.
clprsLoginProfile :: Lens' CreateLoginProfileResponse LoginProfile
clprsLoginProfile = lens _clprsLoginProfile (\ s a -> s{_clprsLoginProfile = a});
