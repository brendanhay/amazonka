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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminGetUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified user by user name in a user pool as an administrator. Works on any user.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminGetUser
    (
    -- * Creating a Request
      adminGetUser
    , AdminGetUser
    -- * Request Lenses
    , aguUserPoolId
    , aguUsername

    -- * Destructuring the Response
    , adminGetUserResponse
    , AdminGetUserResponse
    -- * Response Lenses
    , agursEnabled
    , agursUserStatus
    , agursUserAttributes
    , agursUserCreateDate
    , agursUserMFASettingList
    , agursMFAOptions
    , agursUserLastModifiedDate
    , agursPreferredMFASetting
    , agursResponseStatus
    , agursUsername
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to get the specified user as an administrator.
--
--
--
-- /See:/ 'adminGetUser' smart constructor.
data AdminGetUser = AdminGetUser'
  { _aguUserPoolId :: !Text
  , _aguUsername   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminGetUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aguUserPoolId' - The user pool ID for the user pool where you want to get information about the user.
--
-- * 'aguUsername' - The user name of the user you wish to retrieve.
adminGetUser
    :: Text -- ^ 'aguUserPoolId'
    -> Text -- ^ 'aguUsername'
    -> AdminGetUser
adminGetUser pUserPoolId_ pUsername_ =
  AdminGetUser'
    {_aguUserPoolId = pUserPoolId_, _aguUsername = _Sensitive # pUsername_}


-- | The user pool ID for the user pool where you want to get information about the user.
aguUserPoolId :: Lens' AdminGetUser Text
aguUserPoolId = lens _aguUserPoolId (\ s a -> s{_aguUserPoolId = a})

-- | The user name of the user you wish to retrieve.
aguUsername :: Lens' AdminGetUser Text
aguUsername = lens _aguUsername (\ s a -> s{_aguUsername = a}) . _Sensitive

instance AWSRequest AdminGetUser where
        type Rs AdminGetUser = AdminGetUserResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 AdminGetUserResponse' <$>
                   (x .?> "Enabled") <*> (x .?> "UserStatus") <*>
                     (x .?> "UserAttributes" .!@ mempty)
                     <*> (x .?> "UserCreateDate")
                     <*> (x .?> "UserMFASettingList" .!@ mempty)
                     <*> (x .?> "MFAOptions" .!@ mempty)
                     <*> (x .?> "UserLastModifiedDate")
                     <*> (x .?> "PreferredMfaSetting")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "Username"))

instance Hashable AdminGetUser where

instance NFData AdminGetUser where

instance ToHeaders AdminGetUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminGetUser" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminGetUser where
        toJSON AdminGetUser'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _aguUserPoolId),
                  Just ("Username" .= _aguUsername)])

instance ToPath AdminGetUser where
        toPath = const "/"

instance ToQuery AdminGetUser where
        toQuery = const mempty

-- | Represents the response from the server from the request to get the specified user as an administrator.
--
--
--
-- /See:/ 'adminGetUserResponse' smart constructor.
data AdminGetUserResponse = AdminGetUserResponse'
  { _agursEnabled              :: !(Maybe Bool)
  , _agursUserStatus           :: !(Maybe UserStatusType)
  , _agursUserAttributes       :: !(Maybe [AttributeType])
  , _agursUserCreateDate       :: !(Maybe POSIX)
  , _agursUserMFASettingList   :: !(Maybe [Text])
  , _agursMFAOptions           :: !(Maybe [MFAOptionType])
  , _agursUserLastModifiedDate :: !(Maybe POSIX)
  , _agursPreferredMFASetting  :: !(Maybe Text)
  , _agursResponseStatus       :: !Int
  , _agursUsername             :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminGetUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agursEnabled' - Indicates that the status is enabled.
--
-- * 'agursUserStatus' - The user status. Can be one of the following:     * UNCONFIRMED - User has been created but not confirmed.     * CONFIRMED - User has been confirmed.     * ARCHIVED - User is no longer active.     * COMPROMISED - User is disabled due to a potential security threat.     * UNKNOWN - User status is not known.
--
-- * 'agursUserAttributes' - An array of name-value pairs representing user attributes.
--
-- * 'agursUserCreateDate' - The date the user was created.
--
-- * 'agursUserMFASettingList' - The list of the user's MFA settings.
--
-- * 'agursMFAOptions' - Specifies the options for MFA (e.g., email or phone number).
--
-- * 'agursUserLastModifiedDate' - The date the user was last modified.
--
-- * 'agursPreferredMFASetting' - The user's preferred MFA setting.
--
-- * 'agursResponseStatus' - -- | The response status code.
--
-- * 'agursUsername' - The user name of the user about whom you are receiving information.
adminGetUserResponse
    :: Int -- ^ 'agursResponseStatus'
    -> Text -- ^ 'agursUsername'
    -> AdminGetUserResponse
adminGetUserResponse pResponseStatus_ pUsername_ =
  AdminGetUserResponse'
    { _agursEnabled = Nothing
    , _agursUserStatus = Nothing
    , _agursUserAttributes = Nothing
    , _agursUserCreateDate = Nothing
    , _agursUserMFASettingList = Nothing
    , _agursMFAOptions = Nothing
    , _agursUserLastModifiedDate = Nothing
    , _agursPreferredMFASetting = Nothing
    , _agursResponseStatus = pResponseStatus_
    , _agursUsername = _Sensitive # pUsername_
    }


-- | Indicates that the status is enabled.
agursEnabled :: Lens' AdminGetUserResponse (Maybe Bool)
agursEnabled = lens _agursEnabled (\ s a -> s{_agursEnabled = a})

-- | The user status. Can be one of the following:     * UNCONFIRMED - User has been created but not confirmed.     * CONFIRMED - User has been confirmed.     * ARCHIVED - User is no longer active.     * COMPROMISED - User is disabled due to a potential security threat.     * UNKNOWN - User status is not known.
agursUserStatus :: Lens' AdminGetUserResponse (Maybe UserStatusType)
agursUserStatus = lens _agursUserStatus (\ s a -> s{_agursUserStatus = a})

-- | An array of name-value pairs representing user attributes.
agursUserAttributes :: Lens' AdminGetUserResponse [AttributeType]
agursUserAttributes = lens _agursUserAttributes (\ s a -> s{_agursUserAttributes = a}) . _Default . _Coerce

-- | The date the user was created.
agursUserCreateDate :: Lens' AdminGetUserResponse (Maybe UTCTime)
agursUserCreateDate = lens _agursUserCreateDate (\ s a -> s{_agursUserCreateDate = a}) . mapping _Time

-- | The list of the user's MFA settings.
agursUserMFASettingList :: Lens' AdminGetUserResponse [Text]
agursUserMFASettingList = lens _agursUserMFASettingList (\ s a -> s{_agursUserMFASettingList = a}) . _Default . _Coerce

-- | Specifies the options for MFA (e.g., email or phone number).
agursMFAOptions :: Lens' AdminGetUserResponse [MFAOptionType]
agursMFAOptions = lens _agursMFAOptions (\ s a -> s{_agursMFAOptions = a}) . _Default . _Coerce

-- | The date the user was last modified.
agursUserLastModifiedDate :: Lens' AdminGetUserResponse (Maybe UTCTime)
agursUserLastModifiedDate = lens _agursUserLastModifiedDate (\ s a -> s{_agursUserLastModifiedDate = a}) . mapping _Time

-- | The user's preferred MFA setting.
agursPreferredMFASetting :: Lens' AdminGetUserResponse (Maybe Text)
agursPreferredMFASetting = lens _agursPreferredMFASetting (\ s a -> s{_agursPreferredMFASetting = a})

-- | -- | The response status code.
agursResponseStatus :: Lens' AdminGetUserResponse Int
agursResponseStatus = lens _agursResponseStatus (\ s a -> s{_agursResponseStatus = a})

-- | The user name of the user about whom you are receiving information.
agursUsername :: Lens' AdminGetUserResponse Text
agursUsername = lens _agursUsername (\ s a -> s{_agursUsername = a}) . _Sensitive

instance NFData AdminGetUserResponse where
