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
-- Module      : Network.AWS.IAM.GetUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified IAM user, including the user's creation date, path, unique ID, and ARN.
--
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID used to sign the request to this API.
--
module Network.AWS.IAM.GetUser
    (
    -- * Creating a Request
      getUser
    , GetUser
    -- * Request Lenses
    , guUserName

    -- * Destructuring the Response
    , getUserResponse
    , GetUserResponse
    -- * Response Lenses
    , gursResponseStatus
    , gursUser
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUser' smart constructor.
newtype GetUser = GetUser'
  { _guUserName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guUserName' - The name of the user to get information about. This parameter is optional. If it is not included, it defaults to the user making the request. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
getUser
    :: GetUser
getUser = GetUser' {_guUserName = Nothing}


-- | The name of the user to get information about. This parameter is optional. If it is not included, it defaults to the user making the request. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
guUserName :: Lens' GetUser (Maybe Text)
guUserName = lens _guUserName (\ s a -> s{_guUserName = a})

instance AWSRequest GetUser where
        type Rs GetUser = GetUserResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetUserResult"
              (\ s h x ->
                 GetUserResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "User"))

instance Hashable GetUser where

instance NFData GetUser where

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

-- | Contains the response to a successful 'GetUser' request.
--
--
--
-- /See:/ 'getUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { _gursResponseStatus :: !Int
  , _gursUser           :: !User
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gursResponseStatus' - -- | The response status code.
--
-- * 'gursUser' - A structure containing details about the IAM user.
getUserResponse
    :: Int -- ^ 'gursResponseStatus'
    -> User -- ^ 'gursUser'
    -> GetUserResponse
getUserResponse pResponseStatus_ pUser_ =
  GetUserResponse' {_gursResponseStatus = pResponseStatus_, _gursUser = pUser_}


-- | -- | The response status code.
gursResponseStatus :: Lens' GetUserResponse Int
gursResponseStatus = lens _gursResponseStatus (\ s a -> s{_gursResponseStatus = a})

-- | A structure containing details about the IAM user.
gursUser :: Lens' GetUserResponse User
gursUser = lens _gursUser (\ s a -> s{_gursUser = a})

instance NFData GetUserResponse where
