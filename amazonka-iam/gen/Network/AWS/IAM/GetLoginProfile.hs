{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetLoginProfile
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

-- | Retrieves the user name and password-creation date for the specified
-- user. If the user has not been assigned a password, the action returns a
-- 404 (@NoSuchEntity@) error.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetLoginProfile.html>
module Network.AWS.IAM.GetLoginProfile
    (
    -- * Request
      GetLoginProfile
    -- ** Request constructor
    , getLoginProfile
    -- ** Request lenses
    , glpUserName

    -- * Response
    , GetLoginProfileResponse
    -- ** Response constructor
    , getLoginProfileResponse
    -- ** Response lenses
    , glprLoginProfile
    , glprStatusCode
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLoginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glpUserName'
newtype GetLoginProfile = GetLoginProfile'{_glpUserName :: Text} deriving (Eq, Read, Show)

-- | 'GetLoginProfile' smart constructor.
getLoginProfile :: Text -> GetLoginProfile
getLoginProfile pUserName = GetLoginProfile'{_glpUserName = pUserName};

-- | The name of the user whose login profile you want to retrieve.
glpUserName :: Lens' GetLoginProfile Text
glpUserName = lens _glpUserName (\ s a -> s{_glpUserName = a});

instance AWSRequest GetLoginProfile where
        type Sv GetLoginProfile = IAM
        type Rs GetLoginProfile = GetLoginProfileResponse
        request = post
        response
          = receiveXMLWrapper "GetLoginProfileResult"
              (\ s h x ->
                 GetLoginProfileResponse' <$>
                   (x .@ "LoginProfile") <*> (pure (fromEnum s)))

instance ToHeaders GetLoginProfile where
        toHeaders = const mempty

instance ToPath GetLoginProfile where
        toPath = const "/"

instance ToQuery GetLoginProfile where
        toQuery GetLoginProfile'{..}
          = mconcat
              ["Action" =: ("GetLoginProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _glpUserName]

-- | Contains the response to a successful GetLoginProfile request.
--
-- /See:/ 'getLoginProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glprLoginProfile'
--
-- * 'glprStatusCode'
data GetLoginProfileResponse = GetLoginProfileResponse'{_glprLoginProfile :: LoginProfile, _glprStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'GetLoginProfileResponse' smart constructor.
getLoginProfileResponse :: LoginProfile -> Int -> GetLoginProfileResponse
getLoginProfileResponse pLoginProfile pStatusCode = GetLoginProfileResponse'{_glprLoginProfile = pLoginProfile, _glprStatusCode = pStatusCode};

-- | The user name and password create date for the user.
glprLoginProfile :: Lens' GetLoginProfileResponse LoginProfile
glprLoginProfile = lens _glprLoginProfile (\ s a -> s{_glprLoginProfile = a});

-- | FIXME: Undocumented member.
glprStatusCode :: Lens' GetLoginProfileResponse Int
glprStatusCode = lens _glprStatusCode (\ s a -> s{_glprStatusCode = a});
