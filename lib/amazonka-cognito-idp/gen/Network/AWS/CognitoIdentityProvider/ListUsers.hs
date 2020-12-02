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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users in the Amazon Cognito user pool.
--
--
module Network.AWS.CognitoIdentityProvider.ListUsers
    (
    -- * Creating a Request
      listUsers
    , ListUsers
    -- * Request Lenses
    , luPaginationToken
    , luAttributesToGet
    , luLimit
    , luFilter
    , luUserPoolId

    -- * Destructuring the Response
    , listUsersResponse
    , ListUsersResponse
    -- * Response Lenses
    , lursPaginationToken
    , lursUsers
    , lursResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to list users.
--
--
--
-- /See:/ 'listUsers' smart constructor.
data ListUsers = ListUsers'
  { _luPaginationToken :: !(Maybe Text)
  , _luAttributesToGet :: !(Maybe [Text])
  , _luLimit           :: !(Maybe Nat)
  , _luFilter          :: !(Maybe Text)
  , _luUserPoolId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luPaginationToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'luAttributesToGet' - An array of strings, where each string is the name of a user attribute to be returned for each user in the search results. If the array is null, all attributes are returned.
--
-- * 'luLimit' - Maximum number of users to be returned.
--
-- * 'luFilter' - A filter string of the form "/AttributeName/ /Filter-Type/ "/AttributeValue/ "". Quotation marks within the filter string must be escaped using the backslash (\) character. For example, "@family_name@ = \"Reddy\"".     * /AttributeName/ : The name of the attribute to search for. You can only search for one attribute at a time.     * /Filter-Type/ : For an exact match, use =, for example, "@given_name@ = \"Jon\"". For a prefix ("starts with") match, use ^=, for example, "@given_name@ ^= \"Jon\"".      * /AttributeValue/ : The attribute value that must be matched for each user. If the filter string is empty, @ListUsers@ returns all users in the user pool. You can only search for the following standard attributes:     * @username@ (case-sensitive)     * @email@      * @phone_number@      * @name@      * @given_name@      * @family_name@      * @preferred_username@      * @cognito:user_status@ (called __Enabled__ in the Console) (case-sensitive)     * @status@ (case-insensitive)     * @sub@  Custom attributes are not searchable. For more information, see <http://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-using-listusers-api Searching for Users Using the ListUsers API> and <http://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-listusers-api-examples Examples of Using the ListUsers API> in the /Amazon Cognito Developer Guide/ .
--
-- * 'luUserPoolId' - The user pool ID for the user pool on which the search should be performed.
listUsers
    :: Text -- ^ 'luUserPoolId'
    -> ListUsers
listUsers pUserPoolId_ =
  ListUsers'
    { _luPaginationToken = Nothing
    , _luAttributesToGet = Nothing
    , _luLimit = Nothing
    , _luFilter = Nothing
    , _luUserPoolId = pUserPoolId_
    }


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
luPaginationToken :: Lens' ListUsers (Maybe Text)
luPaginationToken = lens _luPaginationToken (\ s a -> s{_luPaginationToken = a})

-- | An array of strings, where each string is the name of a user attribute to be returned for each user in the search results. If the array is null, all attributes are returned.
luAttributesToGet :: Lens' ListUsers [Text]
luAttributesToGet = lens _luAttributesToGet (\ s a -> s{_luAttributesToGet = a}) . _Default . _Coerce

-- | Maximum number of users to be returned.
luLimit :: Lens' ListUsers (Maybe Natural)
luLimit = lens _luLimit (\ s a -> s{_luLimit = a}) . mapping _Nat

-- | A filter string of the form "/AttributeName/ /Filter-Type/ "/AttributeValue/ "". Quotation marks within the filter string must be escaped using the backslash (\) character. For example, "@family_name@ = \"Reddy\"".     * /AttributeName/ : The name of the attribute to search for. You can only search for one attribute at a time.     * /Filter-Type/ : For an exact match, use =, for example, "@given_name@ = \"Jon\"". For a prefix ("starts with") match, use ^=, for example, "@given_name@ ^= \"Jon\"".      * /AttributeValue/ : The attribute value that must be matched for each user. If the filter string is empty, @ListUsers@ returns all users in the user pool. You can only search for the following standard attributes:     * @username@ (case-sensitive)     * @email@      * @phone_number@      * @name@      * @given_name@      * @family_name@      * @preferred_username@      * @cognito:user_status@ (called __Enabled__ in the Console) (case-sensitive)     * @status@ (case-insensitive)     * @sub@  Custom attributes are not searchable. For more information, see <http://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-using-listusers-api Searching for Users Using the ListUsers API> and <http://docs.aws.amazon.com/cognito/latest/developerguide/how-to-manage-user-accounts.html#cognito-user-pools-searching-for-users-listusers-api-examples Examples of Using the ListUsers API> in the /Amazon Cognito Developer Guide/ .
luFilter :: Lens' ListUsers (Maybe Text)
luFilter = lens _luFilter (\ s a -> s{_luFilter = a})

-- | The user pool ID for the user pool on which the search should be performed.
luUserPoolId :: Lens' ListUsers Text
luUserPoolId = lens _luUserPoolId (\ s a -> s{_luUserPoolId = a})

instance AWSRequest ListUsers where
        type Rs ListUsers = ListUsersResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ListUsersResponse' <$>
                   (x .?> "PaginationToken") <*>
                     (x .?> "Users" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListUsers where

instance NFData ListUsers where

instance ToHeaders ListUsers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ListUsers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUsers where
        toJSON ListUsers'{..}
          = object
              (catMaybes
                 [("PaginationToken" .=) <$> _luPaginationToken,
                  ("AttributesToGet" .=) <$> _luAttributesToGet,
                  ("Limit" .=) <$> _luLimit,
                  ("Filter" .=) <$> _luFilter,
                  Just ("UserPoolId" .= _luUserPoolId)])

instance ToPath ListUsers where
        toPath = const "/"

instance ToQuery ListUsers where
        toQuery = const mempty

-- | The response from the request to list users.
--
--
--
-- /See:/ 'listUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { _lursPaginationToken :: !(Maybe Text)
  , _lursUsers           :: !(Maybe [UserType])
  , _lursResponseStatus  :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursPaginationToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lursUsers' - The users returned in the request to list users.
--
-- * 'lursResponseStatus' - -- | The response status code.
listUsersResponse
    :: Int -- ^ 'lursResponseStatus'
    -> ListUsersResponse
listUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { _lursPaginationToken = Nothing
    , _lursUsers = Nothing
    , _lursResponseStatus = pResponseStatus_
    }


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lursPaginationToken :: Lens' ListUsersResponse (Maybe Text)
lursPaginationToken = lens _lursPaginationToken (\ s a -> s{_lursPaginationToken = a})

-- | The users returned in the request to list users.
lursUsers :: Lens' ListUsersResponse [UserType]
lursUsers = lens _lursUsers (\ s a -> s{_lursUsers = a}) . _Default . _Coerce

-- | -- | The response status code.
lursResponseStatus :: Lens' ListUsersResponse Int
lursResponseStatus = lens _lursResponseStatus (\ s a -> s{_lursResponseStatus = a})

instance NFData ListUsersResponse where
