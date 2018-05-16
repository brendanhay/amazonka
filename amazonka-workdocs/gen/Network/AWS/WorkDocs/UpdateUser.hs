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
-- Module      : Network.AWS.WorkDocs.UpdateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified user, and grants or revokes administrative privileges to the Amazon WorkDocs site.
--
--
module Network.AWS.WorkDocs.UpdateUser
    (
    -- * Creating a Request
      updateUser
    , UpdateUser
    -- * Request Lenses
    , uuGivenName
    , uuGrantPoweruserPrivileges
    , uuLocale
    , uuAuthenticationToken
    , uuStorageRule
    , uuType
    , uuSurname
    , uuTimeZoneId
    , uuUserId

    -- * Destructuring the Response
    , updateUserResponse
    , UpdateUserResponse
    -- * Response Lenses
    , uursUser
    , uursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'updateUser' smart constructor.
data UpdateUser = UpdateUser'
  { _uuGivenName                :: !(Maybe Text)
  , _uuGrantPoweruserPrivileges :: !(Maybe BooleanEnumType)
  , _uuLocale                   :: !(Maybe LocaleType)
  , _uuAuthenticationToken      :: !(Maybe (Sensitive Text))
  , _uuStorageRule              :: !(Maybe StorageRuleType)
  , _uuType                     :: !(Maybe UserType)
  , _uuSurname                  :: !(Maybe Text)
  , _uuTimeZoneId               :: !(Maybe Text)
  , _uuUserId                   :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuGivenName' - The given name of the user.
--
-- * 'uuGrantPoweruserPrivileges' - Boolean value to determine whether the user is granted Poweruser privileges.
--
-- * 'uuLocale' - The locale of the user.
--
-- * 'uuAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'uuStorageRule' - The amount of storage for the user.
--
-- * 'uuType' - The type of the user.
--
-- * 'uuSurname' - The surname of the user.
--
-- * 'uuTimeZoneId' - The time zone ID of the user.
--
-- * 'uuUserId' - The ID of the user.
updateUser
    :: Text -- ^ 'uuUserId'
    -> UpdateUser
updateUser pUserId_ =
  UpdateUser'
    { _uuGivenName = Nothing
    , _uuGrantPoweruserPrivileges = Nothing
    , _uuLocale = Nothing
    , _uuAuthenticationToken = Nothing
    , _uuStorageRule = Nothing
    , _uuType = Nothing
    , _uuSurname = Nothing
    , _uuTimeZoneId = Nothing
    , _uuUserId = pUserId_
    }


-- | The given name of the user.
uuGivenName :: Lens' UpdateUser (Maybe Text)
uuGivenName = lens _uuGivenName (\ s a -> s{_uuGivenName = a})

-- | Boolean value to determine whether the user is granted Poweruser privileges.
uuGrantPoweruserPrivileges :: Lens' UpdateUser (Maybe BooleanEnumType)
uuGrantPoweruserPrivileges = lens _uuGrantPoweruserPrivileges (\ s a -> s{_uuGrantPoweruserPrivileges = a})

-- | The locale of the user.
uuLocale :: Lens' UpdateUser (Maybe LocaleType)
uuLocale = lens _uuLocale (\ s a -> s{_uuLocale = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
uuAuthenticationToken :: Lens' UpdateUser (Maybe Text)
uuAuthenticationToken = lens _uuAuthenticationToken (\ s a -> s{_uuAuthenticationToken = a}) . mapping _Sensitive

-- | The amount of storage for the user.
uuStorageRule :: Lens' UpdateUser (Maybe StorageRuleType)
uuStorageRule = lens _uuStorageRule (\ s a -> s{_uuStorageRule = a})

-- | The type of the user.
uuType :: Lens' UpdateUser (Maybe UserType)
uuType = lens _uuType (\ s a -> s{_uuType = a})

-- | The surname of the user.
uuSurname :: Lens' UpdateUser (Maybe Text)
uuSurname = lens _uuSurname (\ s a -> s{_uuSurname = a})

-- | The time zone ID of the user.
uuTimeZoneId :: Lens' UpdateUser (Maybe Text)
uuTimeZoneId = lens _uuTimeZoneId (\ s a -> s{_uuTimeZoneId = a})

-- | The ID of the user.
uuUserId :: Lens' UpdateUser Text
uuUserId = lens _uuUserId (\ s a -> s{_uuUserId = a})

instance AWSRequest UpdateUser where
        type Rs UpdateUser = UpdateUserResponse
        request = patchJSON workDocs
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUserResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable UpdateUser where

instance NFData UpdateUser where

instance ToHeaders UpdateUser where
        toHeaders UpdateUser'{..}
          = mconcat
              ["Authentication" =# _uuAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON UpdateUser where
        toJSON UpdateUser'{..}
          = object
              (catMaybes
                 [("GivenName" .=) <$> _uuGivenName,
                  ("GrantPoweruserPrivileges" .=) <$>
                    _uuGrantPoweruserPrivileges,
                  ("Locale" .=) <$> _uuLocale,
                  ("StorageRule" .=) <$> _uuStorageRule,
                  ("Type" .=) <$> _uuType,
                  ("Surname" .=) <$> _uuSurname,
                  ("TimeZoneId" .=) <$> _uuTimeZoneId])

instance ToPath UpdateUser where
        toPath UpdateUser'{..}
          = mconcat ["/api/v1/users/", toBS _uuUserId]

instance ToQuery UpdateUser where
        toQuery = const mempty

-- | /See:/ 'updateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { _uursUser           :: !(Maybe User)
  , _uursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uursUser' - The user information.
--
-- * 'uursResponseStatus' - -- | The response status code.
updateUserResponse
    :: Int -- ^ 'uursResponseStatus'
    -> UpdateUserResponse
updateUserResponse pResponseStatus_ =
  UpdateUserResponse'
    {_uursUser = Nothing, _uursResponseStatus = pResponseStatus_}


-- | The user information.
uursUser :: Lens' UpdateUserResponse (Maybe User)
uursUser = lens _uursUser (\ s a -> s{_uursUser = a})

-- | -- | The response status code.
uursResponseStatus :: Lens' UpdateUserResponse Int
uursResponseStatus = lens _uursResponseStatus (\ s a -> s{_uursResponseStatus = a})

instance NFData UpdateUserResponse where
