{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetUser
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

-- | Retrieves information about the specified user, including the user\'s
-- creation date, path, unique ID, and ARN.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID used to sign the request.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUser.html>
module Network.AWS.IAM.GetUser
    (
    -- * Request
      GetUser
    -- ** Request constructor
    , getUser
    -- ** Request lenses
    , guUserName

    -- * Response
    , GetUserResponse
    -- ** Response constructor
    , getUserResponse
    -- ** Response lenses
    , gurUser
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'guUserName'
newtype GetUser = GetUser'{_guUserName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetUser' smart constructor.
getUser :: GetUser
getUser = GetUser'{_guUserName = Nothing};

-- | The name of the user to get information about.
--
-- This parameter is optional. If it is not included, it defaults to the
-- user making the request.
guUserName :: Lens' GetUser (Maybe Text)
guUserName = lens _guUserName (\ s a -> s{_guUserName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest GetUser where
        type Sv GetUser = IAM
        type Rs GetUser = GetUserResponse
        request = post
        response
          = receiveXMLWrapper "GetUserResult"
              (\ s h x -> GetUserResponse' <$> (x .@ "User"))

instance ToHeaders GetUser where
        toHeaders = const mempty

instance ToPath GetUser where
        toPath = const "/"

instance ToQuery GetUser where
        toQuery GetUser'{..}
          = mconcat
              ["Action" =: ("GetUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _guUserName]

-- | /See:/ 'getUserResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gurUser'
newtype GetUserResponse = GetUserResponse'{_gurUser :: User} deriving (Eq, Read, Show)

-- | 'GetUserResponse' smart constructor.
getUserResponse :: User -> GetUserResponse
getUserResponse pUser = GetUserResponse'{_gurUser = pUser};

-- | Information about the user.
gurUser :: Lens' GetUserResponse User
gurUser = lens _gurUser (\ s a -> s{_gurUser = a});
