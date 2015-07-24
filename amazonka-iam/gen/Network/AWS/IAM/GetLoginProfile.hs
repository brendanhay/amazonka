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
    , glpUserName

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
-- * 'glpUserName'
newtype GetLoginProfile = GetLoginProfile'
    { _glpUserName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetLoginProfile' smart constructor.
getLoginProfile :: Text -> GetLoginProfile
getLoginProfile pUserName_ =
    GetLoginProfile'
    { _glpUserName = pUserName_
    }

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
               "UserName" =: _glpUserName]

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
getLoginProfileResponse pStatus_ pLoginProfile_ =
    GetLoginProfileResponse'
    { _glprsStatus = pStatus_
    , _glprsLoginProfile = pLoginProfile_
    }

-- | FIXME: Undocumented member.
glprsStatus :: Lens' GetLoginProfileResponse Int
glprsStatus = lens _glprsStatus (\ s a -> s{_glprsStatus = a});

-- | The user name and password create date for the user.
glprsLoginProfile :: Lens' GetLoginProfileResponse LoginProfile
glprsLoginProfile = lens _glprsLoginProfile (\ s a -> s{_glprsLoginProfile = a});
