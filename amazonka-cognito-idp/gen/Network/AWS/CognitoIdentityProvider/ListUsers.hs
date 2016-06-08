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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users in the Amazon Cognito user pool.
module Network.AWS.CognitoIdentityProvider.ListUsers
    (
    -- * Creating a Request
      listUsers
    , ListUsers
    -- * Request Lenses
    , luPaginationToken
    , luAttributesToGet
    , luUserStatus
    , luLimit
    , luUserPoolId

    -- * Destructuring the Response
    , listUsersResponse
    , ListUsersResponse
    -- * Response Lenses
    , lursPaginationToken
    , lursUsers
    , lursResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to list users.
--
-- /See:/ 'listUsers' smart constructor.
data ListUsers = ListUsers'
    { _luPaginationToken :: !(Maybe Text)
    , _luAttributesToGet :: !(Maybe [Text])
    , _luUserStatus      :: !(Maybe UserStatusType)
    , _luLimit           :: !(Maybe Nat)
    , _luUserPoolId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luPaginationToken'
--
-- * 'luAttributesToGet'
--
-- * 'luUserStatus'
--
-- * 'luLimit'
--
-- * 'luUserPoolId'
listUsers
    :: Text -- ^ 'luUserPoolId'
    -> ListUsers
listUsers pUserPoolId_ =
    ListUsers'
    { _luPaginationToken = Nothing
    , _luAttributesToGet = Nothing
    , _luUserStatus = Nothing
    , _luLimit = Nothing
    , _luUserPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
luPaginationToken :: Lens' ListUsers (Maybe Text)
luPaginationToken = lens _luPaginationToken (\ s a -> s{_luPaginationToken = a});

-- | The attributes to get from the request to list users.
luAttributesToGet :: Lens' ListUsers [Text]
luAttributesToGet = lens _luAttributesToGet (\ s a -> s{_luAttributesToGet = a}) . _Default . _Coerce;

-- | The user status. Can be one of the following:
--
-- -   UNCONFIRMED - User has been created but not confirmed.
-- -   CONFIRMED - User has been confirmed.
-- -   ARCHIVED - User is no longer active.
-- -   COMPROMISED - User is disabled due to a potential security threat.
-- -   UNKNOWN - User status is not known.
luUserStatus :: Lens' ListUsers (Maybe UserStatusType)
luUserStatus = lens _luUserStatus (\ s a -> s{_luUserStatus = a});

-- | The limit of the request to list users.
luLimit :: Lens' ListUsers (Maybe Natural)
luLimit = lens _luLimit (\ s a -> s{_luLimit = a}) . mapping _Nat;

-- | The user pool ID for which you want to list users.
luUserPoolId :: Lens' ListUsers Text
luUserPoolId = lens _luUserPoolId (\ s a -> s{_luUserPoolId = a});

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

instance Hashable ListUsers

instance NFData ListUsers

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
                  ("UserStatus" .=) <$> _luUserStatus,
                  ("Limit" .=) <$> _luLimit,
                  Just ("UserPoolId" .= _luUserPoolId)])

instance ToPath ListUsers where
        toPath = const "/"

instance ToQuery ListUsers where
        toQuery = const mempty

-- | The response from the request to list users.
--
-- /See:/ 'listUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
    { _lursPaginationToken :: !(Maybe Text)
    , _lursUsers           :: !(Maybe [UserType])
    , _lursResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursPaginationToken'
--
-- * 'lursUsers'
--
-- * 'lursResponseStatus'
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
lursPaginationToken = lens _lursPaginationToken (\ s a -> s{_lursPaginationToken = a});

-- | The users returned in the request to list users.
lursUsers :: Lens' ListUsersResponse [UserType]
lursUsers = lens _lursUsers (\ s a -> s{_lursUsers = a}) . _Default . _Coerce;

-- | The response status code.
lursResponseStatus :: Lens' ListUsersResponse Int
lursResponseStatus = lens _lursResponseStatus (\ s a -> s{_lursResponseStatus = a});

instance NFData ListUsersResponse
