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
-- Module      : Network.AWS.WorkDocs.CreateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user in a Simple AD or Microsoft AD directory. The status of a newly created user is "ACTIVE". New users can access Amazon WorkDocs.
--
--
module Network.AWS.WorkDocs.CreateUser
    (
    -- * Creating a Request
      createUser
    , CreateUser
    -- * Request Lenses
    , cuAuthenticationToken
    , cuStorageRule
    , cuEmailAddress
    , cuTimeZoneId
    , cuOrganizationId
    , cuUsername
    , cuGivenName
    , cuSurname
    , cuPassword

    -- * Destructuring the Response
    , createUserResponse
    , CreateUserResponse
    -- * Response Lenses
    , cursUser
    , cursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'createUser' smart constructor.
data CreateUser = CreateUser'
  { _cuAuthenticationToken :: !(Maybe (Sensitive Text))
  , _cuStorageRule         :: !(Maybe StorageRuleType)
  , _cuEmailAddress        :: !(Maybe Text)
  , _cuTimeZoneId          :: !(Maybe Text)
  , _cuOrganizationId      :: !(Maybe Text)
  , _cuUsername            :: !Text
  , _cuGivenName           :: !Text
  , _cuSurname             :: !Text
  , _cuPassword            :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'cuStorageRule' - The amount of storage for the user.
--
-- * 'cuEmailAddress' - The email address of the user.
--
-- * 'cuTimeZoneId' - The time zone ID of the user.
--
-- * 'cuOrganizationId' - The ID of the organization.
--
-- * 'cuUsername' - The login name of the user.
--
-- * 'cuGivenName' - The given name of the user.
--
-- * 'cuSurname' - The surname of the user.
--
-- * 'cuPassword' - The password of the user.
createUser
    :: Text -- ^ 'cuUsername'
    -> Text -- ^ 'cuGivenName'
    -> Text -- ^ 'cuSurname'
    -> Text -- ^ 'cuPassword'
    -> CreateUser
createUser pUsername_ pGivenName_ pSurname_ pPassword_ =
  CreateUser'
    { _cuAuthenticationToken = Nothing
    , _cuStorageRule = Nothing
    , _cuEmailAddress = Nothing
    , _cuTimeZoneId = Nothing
    , _cuOrganizationId = Nothing
    , _cuUsername = pUsername_
    , _cuGivenName = pGivenName_
    , _cuSurname = pSurname_
    , _cuPassword = _Sensitive # pPassword_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
cuAuthenticationToken :: Lens' CreateUser (Maybe Text)
cuAuthenticationToken = lens _cuAuthenticationToken (\ s a -> s{_cuAuthenticationToken = a}) . mapping _Sensitive

-- | The amount of storage for the user.
cuStorageRule :: Lens' CreateUser (Maybe StorageRuleType)
cuStorageRule = lens _cuStorageRule (\ s a -> s{_cuStorageRule = a})

-- | The email address of the user.
cuEmailAddress :: Lens' CreateUser (Maybe Text)
cuEmailAddress = lens _cuEmailAddress (\ s a -> s{_cuEmailAddress = a})

-- | The time zone ID of the user.
cuTimeZoneId :: Lens' CreateUser (Maybe Text)
cuTimeZoneId = lens _cuTimeZoneId (\ s a -> s{_cuTimeZoneId = a})

-- | The ID of the organization.
cuOrganizationId :: Lens' CreateUser (Maybe Text)
cuOrganizationId = lens _cuOrganizationId (\ s a -> s{_cuOrganizationId = a})

-- | The login name of the user.
cuUsername :: Lens' CreateUser Text
cuUsername = lens _cuUsername (\ s a -> s{_cuUsername = a})

-- | The given name of the user.
cuGivenName :: Lens' CreateUser Text
cuGivenName = lens _cuGivenName (\ s a -> s{_cuGivenName = a})

-- | The surname of the user.
cuSurname :: Lens' CreateUser Text
cuSurname = lens _cuSurname (\ s a -> s{_cuSurname = a})

-- | The password of the user.
cuPassword :: Lens' CreateUser Text
cuPassword = lens _cuPassword (\ s a -> s{_cuPassword = a}) . _Sensitive

instance AWSRequest CreateUser where
        type Rs CreateUser = CreateUserResponse
        request = postJSON workDocs
        response
          = receiveJSON
              (\ s h x ->
                 CreateUserResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable CreateUser where

instance NFData CreateUser where

instance ToHeaders CreateUser where
        toHeaders CreateUser'{..}
          = mconcat
              ["Authentication" =# _cuAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateUser where
        toJSON CreateUser'{..}
          = object
              (catMaybes
                 [("StorageRule" .=) <$> _cuStorageRule,
                  ("EmailAddress" .=) <$> _cuEmailAddress,
                  ("TimeZoneId" .=) <$> _cuTimeZoneId,
                  ("OrganizationId" .=) <$> _cuOrganizationId,
                  Just ("Username" .= _cuUsername),
                  Just ("GivenName" .= _cuGivenName),
                  Just ("Surname" .= _cuSurname),
                  Just ("Password" .= _cuPassword)])

instance ToPath CreateUser where
        toPath = const "/api/v1/users"

instance ToQuery CreateUser where
        toQuery = const mempty

-- | /See:/ 'createUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { _cursUser           :: !(Maybe User)
  , _cursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cursUser' - The user information.
--
-- * 'cursResponseStatus' - -- | The response status code.
createUserResponse
    :: Int -- ^ 'cursResponseStatus'
    -> CreateUserResponse
createUserResponse pResponseStatus_ =
  CreateUserResponse'
    {_cursUser = Nothing, _cursResponseStatus = pResponseStatus_}


-- | The user information.
cursUser :: Lens' CreateUserResponse (Maybe User)
cursUser = lens _cursUser (\ s a -> s{_cursUser = a})

-- | -- | The response status code.
cursResponseStatus :: Lens' CreateUserResponse Int
cursResponseStatus = lens _cursResponseStatus (\ s a -> s{_cursResponseStatus = a})

instance NFData CreateUserResponse where
