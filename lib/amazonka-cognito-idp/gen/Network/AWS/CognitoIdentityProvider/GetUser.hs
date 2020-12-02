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
-- Module      : Network.AWS.CognitoIdentityProvider.GetUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attributes and metadata for a user.
--
--
module Network.AWS.CognitoIdentityProvider.GetUser
    (
    -- * Creating a Request
      getUser
    , GetUser
    -- * Request Lenses
    , guAccessToken

    -- * Destructuring the Response
    , getUserResponse
    , GetUserResponse
    -- * Response Lenses
    , gursUserMFASettingList
    , gursMFAOptions
    , gursPreferredMFASetting
    , gursResponseStatus
    , gursUsername
    , gursUserAttributes
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to get information about the user.
--
--
--
-- /See:/ 'getUser' smart constructor.
newtype GetUser = GetUser'
  { _guAccessToken :: Sensitive Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guAccessToken' - The access token returned by the server response to get information about the user.
getUser
    :: Text -- ^ 'guAccessToken'
    -> GetUser
getUser pAccessToken_ = GetUser' {_guAccessToken = _Sensitive # pAccessToken_}


-- | The access token returned by the server response to get information about the user.
guAccessToken :: Lens' GetUser Text
guAccessToken = lens _guAccessToken (\ s a -> s{_guAccessToken = a}) . _Sensitive

instance AWSRequest GetUser where
        type Rs GetUser = GetUserResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetUserResponse' <$>
                   (x .?> "UserMFASettingList" .!@ mempty) <*>
                     (x .?> "MFAOptions" .!@ mempty)
                     <*> (x .?> "PreferredMfaSetting")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "Username")
                     <*> (x .?> "UserAttributes" .!@ mempty))

instance Hashable GetUser where

instance NFData GetUser where

instance ToHeaders GetUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetUser" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetUser where
        toJSON GetUser'{..}
          = object
              (catMaybes [Just ("AccessToken" .= _guAccessToken)])

instance ToPath GetUser where
        toPath = const "/"

instance ToQuery GetUser where
        toQuery = const mempty

-- | Represents the response from the server from the request to get information about the user.
--
--
--
-- /See:/ 'getUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { _gursUserMFASettingList  :: !(Maybe [Text])
  , _gursMFAOptions          :: !(Maybe [MFAOptionType])
  , _gursPreferredMFASetting :: !(Maybe Text)
  , _gursResponseStatus      :: !Int
  , _gursUsername            :: !(Sensitive Text)
  , _gursUserAttributes      :: ![AttributeType]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gursUserMFASettingList' - The list of the user's MFA settings.
--
-- * 'gursMFAOptions' - Specifies the options for MFA (e.g., email or phone number).
--
-- * 'gursPreferredMFASetting' - The user's preferred MFA setting.
--
-- * 'gursResponseStatus' - -- | The response status code.
--
-- * 'gursUsername' - The user name of the user you wish to retrieve from the get user request.
--
-- * 'gursUserAttributes' - An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
getUserResponse
    :: Int -- ^ 'gursResponseStatus'
    -> Text -- ^ 'gursUsername'
    -> GetUserResponse
getUserResponse pResponseStatus_ pUsername_ =
  GetUserResponse'
    { _gursUserMFASettingList = Nothing
    , _gursMFAOptions = Nothing
    , _gursPreferredMFASetting = Nothing
    , _gursResponseStatus = pResponseStatus_
    , _gursUsername = _Sensitive # pUsername_
    , _gursUserAttributes = mempty
    }


-- | The list of the user's MFA settings.
gursUserMFASettingList :: Lens' GetUserResponse [Text]
gursUserMFASettingList = lens _gursUserMFASettingList (\ s a -> s{_gursUserMFASettingList = a}) . _Default . _Coerce

-- | Specifies the options for MFA (e.g., email or phone number).
gursMFAOptions :: Lens' GetUserResponse [MFAOptionType]
gursMFAOptions = lens _gursMFAOptions (\ s a -> s{_gursMFAOptions = a}) . _Default . _Coerce

-- | The user's preferred MFA setting.
gursPreferredMFASetting :: Lens' GetUserResponse (Maybe Text)
gursPreferredMFASetting = lens _gursPreferredMFASetting (\ s a -> s{_gursPreferredMFASetting = a})

-- | -- | The response status code.
gursResponseStatus :: Lens' GetUserResponse Int
gursResponseStatus = lens _gursResponseStatus (\ s a -> s{_gursResponseStatus = a})

-- | The user name of the user you wish to retrieve from the get user request.
gursUsername :: Lens' GetUserResponse Text
gursUsername = lens _gursUsername (\ s a -> s{_gursUsername = a}) . _Sensitive

-- | An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
gursUserAttributes :: Lens' GetUserResponse [AttributeType]
gursUserAttributes = lens _gursUserAttributes (\ s a -> s{_gursUserAttributes = a}) . _Coerce

instance NFData GetUserResponse where
