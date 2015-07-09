{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetUser
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified user, including the user\'s
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
    , gurStatus
    , gurUser
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'guUserName'
newtype GetUser = GetUser'
    { _guUserName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetUser' smart constructor.
getUser :: GetUser
getUser =
    GetUser'
    { _guUserName = Nothing
    }

-- | The name of the user to get information about.
--
-- This parameter is optional. If it is not included, it defaults to the
-- user making the request.
guUserName :: Lens' GetUser (Maybe Text)
guUserName = lens _guUserName (\ s a -> s{_guUserName = a});

instance AWSRequest GetUser where
        type Sv GetUser = IAM
        type Rs GetUser = GetUserResponse
        request = post
        response
          = receiveXMLWrapper "GetUserResult"
              (\ s h x ->
                 GetUserResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "User"))

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

-- | Contains the response to a successful GetUser request.
--
-- /See:/ 'getUserResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gurStatus'
--
-- * 'gurUser'
data GetUserResponse = GetUserResponse'
    { _gurStatus :: !Int
    , _gurUser   :: !User
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetUserResponse' smart constructor.
getUserResponse :: Int -> User -> GetUserResponse
getUserResponse pStatus pUser =
    GetUserResponse'
    { _gurStatus = pStatus
    , _gurUser = pUser
    }

-- | FIXME: Undocumented member.
gurStatus :: Lens' GetUserResponse Int
gurStatus = lens _gurStatus (\ s a -> s{_gurStatus = a});

-- | Information about the user.
gurUser :: Lens' GetUserResponse User
gurUser = lens _gurUser (\ s a -> s{_gurUser = a});
