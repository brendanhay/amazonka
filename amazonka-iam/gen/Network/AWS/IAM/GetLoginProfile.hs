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
-- Module      : Network.AWS.IAM.GetLoginProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the user name and password-creation date for the specified
-- user. If the user has not been assigned a password, the action returns a
-- 404 ('NoSuchEntity') error.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetLoginProfile.html AWS API Reference> for GetLoginProfile.
module Network.AWS.IAM.GetLoginProfile
    (
    -- * Creating a Request
      getLoginProfile
    , GetLoginProfile
    -- * Request Lenses
    , glpUserName

    -- * Destructuring the Response
    , getLoginProfileResponse
    , GetLoginProfileResponse
    -- * Response Lenses
    , glprsStatus
    , glprsLoginProfile
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getLoginProfile' smart constructor.
newtype GetLoginProfile = GetLoginProfile'
    { _glpUserName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetLoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glpUserName'
getLoginProfile
    :: Text -- ^ 'glpUserName'
    -> GetLoginProfile
getLoginProfile pUserName_ =
    GetLoginProfile'
    { _glpUserName = pUserName_
    }

-- | The name of the user whose login profile you want to retrieve.
glpUserName :: Lens' GetLoginProfile Text
glpUserName = lens _glpUserName (\ s a -> s{_glpUserName = a});

instance AWSRequest GetLoginProfile where
        type Rs GetLoginProfile = GetLoginProfileResponse
        request = postQuery iAM
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
data GetLoginProfileResponse = GetLoginProfileResponse'
    { _glprsStatus       :: !Int
    , _glprsLoginProfile :: !LoginProfile
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetLoginProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glprsStatus'
--
-- * 'glprsLoginProfile'
getLoginProfileResponse
    :: Int -- ^ 'glprsStatus'
    -> LoginProfile -- ^ 'glprsLoginProfile'
    -> GetLoginProfileResponse
getLoginProfileResponse pStatus_ pLoginProfile_ =
    GetLoginProfileResponse'
    { _glprsStatus = pStatus_
    , _glprsLoginProfile = pLoginProfile_
    }

-- | The response status code.
glprsStatus :: Lens' GetLoginProfileResponse Int
glprsStatus = lens _glprsStatus (\ s a -> s{_glprsStatus = a});

-- | The user name and password create date for the user.
glprsLoginProfile :: Lens' GetLoginProfileResponse LoginProfile
glprsLoginProfile = lens _glprsLoginProfile (\ s a -> s{_glprsLoginProfile = a});
