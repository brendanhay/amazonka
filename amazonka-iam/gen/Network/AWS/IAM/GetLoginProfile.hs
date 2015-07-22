{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetLoginProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the user name and password-creation date for the specified
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
    , glprqUserName

    -- * Response
    , GetLoginProfileResponse
    -- ** Response constructor
    , getLoginProfileResponse
    -- ** Response lenses
    , glprsStatus
    , glprsLoginProfile
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getLoginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glprqUserName'
newtype GetLoginProfile = GetLoginProfile'
    { _glprqUserName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetLoginProfile' smart constructor.
getLoginProfile :: Text -> GetLoginProfile
getLoginProfile pUserName =
    GetLoginProfile'
    { _glprqUserName = pUserName
    }

-- | The name of the user whose login profile you want to retrieve.
glprqUserName :: Lens' GetLoginProfile Text
glprqUserName = lens _glprqUserName (\ s a -> s{_glprqUserName = a});

instance AWSRequest GetLoginProfile where
        type Sv GetLoginProfile = IAM
        type Rs GetLoginProfile = GetLoginProfileResponse
        request = post
        response
          = receiveXMLWrapper "GetLoginProfileResult"
              (\ s h x ->
                 GetLoginProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "LoginProfile"))

instance ToHeaders GetLoginProfile where
        toHeaders = const mempty

instance ToPath GetLoginProfile where
        toPath = const "/"

instance ToQuery GetLoginProfile where
        toQuery GetLoginProfile'{..}
          = mconcat
              ["Action" =: ("GetLoginProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _glprqUserName]

-- | Contains the response to a successful GetLoginProfile request.
--
-- /See:/ 'getLoginProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glprsStatus'
--
-- * 'glprsLoginProfile'
data GetLoginProfileResponse = GetLoginProfileResponse'
    { _glprsStatus       :: !Int
    , _glprsLoginProfile :: !LoginProfile
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetLoginProfileResponse' smart constructor.
getLoginProfileResponse :: Int -> LoginProfile -> GetLoginProfileResponse
getLoginProfileResponse pStatus pLoginProfile =
    GetLoginProfileResponse'
    { _glprsStatus = pStatus
    , _glprsLoginProfile = pLoginProfile
    }

-- | FIXME: Undocumented member.
glprsStatus :: Lens' GetLoginProfileResponse Int
glprsStatus = lens _glprsStatus (\ s a -> s{_glprsStatus = a});

-- | The user name and password create date for the user.
glprsLoginProfile :: Lens' GetLoginProfileResponse LoginProfile
glprsLoginProfile = lens _glprsLoginProfile (\ s a -> s{_glprsLoginProfile = a});
