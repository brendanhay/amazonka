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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the user name and password-creation date for the specified IAM user. If the user has not been assigned a password, the operation returns a 404 (@NoSuchEntity@ ) error.
--
--
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
    , glprsResponseStatus
    , glprsLoginProfile
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLoginProfile' smart constructor.
newtype GetLoginProfile = GetLoginProfile'
  { _glpUserName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glpUserName' - The name of the user whose login profile you want to retrieve. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
getLoginProfile
    :: Text -- ^ 'glpUserName'
    -> GetLoginProfile
getLoginProfile pUserName_ = GetLoginProfile' {_glpUserName = pUserName_}


-- | The name of the user whose login profile you want to retrieve. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
glpUserName :: Lens' GetLoginProfile Text
glpUserName = lens _glpUserName (\ s a -> s{_glpUserName = a})

instance AWSRequest GetLoginProfile where
        type Rs GetLoginProfile = GetLoginProfileResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetLoginProfileResult"
              (\ s h x ->
                 GetLoginProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "LoginProfile"))

instance Hashable GetLoginProfile where

instance NFData GetLoginProfile where

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

-- | Contains the response to a successful 'GetLoginProfile' request.
--
--
--
-- /See:/ 'getLoginProfileResponse' smart constructor.
data GetLoginProfileResponse = GetLoginProfileResponse'
  { _glprsResponseStatus :: !Int
  , _glprsLoginProfile   :: !LoginProfile
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLoginProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glprsResponseStatus' - -- | The response status code.
--
-- * 'glprsLoginProfile' - A structure containing the user name and password create date for the user.
getLoginProfileResponse
    :: Int -- ^ 'glprsResponseStatus'
    -> LoginProfile -- ^ 'glprsLoginProfile'
    -> GetLoginProfileResponse
getLoginProfileResponse pResponseStatus_ pLoginProfile_ =
  GetLoginProfileResponse'
    { _glprsResponseStatus = pResponseStatus_
    , _glprsLoginProfile = pLoginProfile_
    }


-- | -- | The response status code.
glprsResponseStatus :: Lens' GetLoginProfileResponse Int
glprsResponseStatus = lens _glprsResponseStatus (\ s a -> s{_glprsResponseStatus = a})

-- | A structure containing the user name and password create date for the user.
glprsLoginProfile :: Lens' GetLoginProfileResponse LoginProfile
glprsLoginProfile = lens _glprsLoginProfile (\ s a -> s{_glprsLoginProfile = a})

instance NFData GetLoginProfileResponse where
