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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUsersInGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users in the specified group.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.ListUsersInGroup
    (
    -- * Creating a Request
      listUsersInGroup
    , ListUsersInGroup
    -- * Request Lenses
    , luigNextToken
    , luigLimit
    , luigUserPoolId
    , luigGroupName

    -- * Destructuring the Response
    , listUsersInGroupResponse
    , ListUsersInGroupResponse
    -- * Response Lenses
    , luigrsUsers
    , luigrsNextToken
    , luigrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUsersInGroup' smart constructor.
data ListUsersInGroup = ListUsersInGroup'
  { _luigNextToken  :: !(Maybe Text)
  , _luigLimit      :: !(Maybe Nat)
  , _luigUserPoolId :: !Text
  , _luigGroupName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsersInGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luigNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'luigLimit' - The limit of the request to list users.
--
-- * 'luigUserPoolId' - The user pool ID for the user pool.
--
-- * 'luigGroupName' - The name of the group.
listUsersInGroup
    :: Text -- ^ 'luigUserPoolId'
    -> Text -- ^ 'luigGroupName'
    -> ListUsersInGroup
listUsersInGroup pUserPoolId_ pGroupName_ =
  ListUsersInGroup'
    { _luigNextToken = Nothing
    , _luigLimit = Nothing
    , _luigUserPoolId = pUserPoolId_
    , _luigGroupName = pGroupName_
    }


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
luigNextToken :: Lens' ListUsersInGroup (Maybe Text)
luigNextToken = lens _luigNextToken (\ s a -> s{_luigNextToken = a})

-- | The limit of the request to list users.
luigLimit :: Lens' ListUsersInGroup (Maybe Natural)
luigLimit = lens _luigLimit (\ s a -> s{_luigLimit = a}) . mapping _Nat

-- | The user pool ID for the user pool.
luigUserPoolId :: Lens' ListUsersInGroup Text
luigUserPoolId = lens _luigUserPoolId (\ s a -> s{_luigUserPoolId = a})

-- | The name of the group.
luigGroupName :: Lens' ListUsersInGroup Text
luigGroupName = lens _luigGroupName (\ s a -> s{_luigGroupName = a})

instance AWSRequest ListUsersInGroup where
        type Rs ListUsersInGroup = ListUsersInGroupResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ListUsersInGroupResponse' <$>
                   (x .?> "Users" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListUsersInGroup where

instance NFData ListUsersInGroup where

instance ToHeaders ListUsersInGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ListUsersInGroup"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUsersInGroup where
        toJSON ListUsersInGroup'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _luigNextToken,
                  ("Limit" .=) <$> _luigLimit,
                  Just ("UserPoolId" .= _luigUserPoolId),
                  Just ("GroupName" .= _luigGroupName)])

instance ToPath ListUsersInGroup where
        toPath = const "/"

instance ToQuery ListUsersInGroup where
        toQuery = const mempty

-- | /See:/ 'listUsersInGroupResponse' smart constructor.
data ListUsersInGroupResponse = ListUsersInGroupResponse'
  { _luigrsUsers          :: !(Maybe [UserType])
  , _luigrsNextToken      :: !(Maybe Text)
  , _luigrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsersInGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luigrsUsers' - The users returned in the request to list users.
--
-- * 'luigrsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'luigrsResponseStatus' - -- | The response status code.
listUsersInGroupResponse
    :: Int -- ^ 'luigrsResponseStatus'
    -> ListUsersInGroupResponse
listUsersInGroupResponse pResponseStatus_ =
  ListUsersInGroupResponse'
    { _luigrsUsers = Nothing
    , _luigrsNextToken = Nothing
    , _luigrsResponseStatus = pResponseStatus_
    }


-- | The users returned in the request to list users.
luigrsUsers :: Lens' ListUsersInGroupResponse [UserType]
luigrsUsers = lens _luigrsUsers (\ s a -> s{_luigrsUsers = a}) . _Default . _Coerce

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
luigrsNextToken :: Lens' ListUsersInGroupResponse (Maybe Text)
luigrsNextToken = lens _luigrsNextToken (\ s a -> s{_luigrsNextToken = a})

-- | -- | The response status code.
luigrsResponseStatus :: Lens' ListUsersInGroupResponse Int
luigrsResponseStatus = lens _luigrsResponseStatus (\ s a -> s{_luigrsResponseStatus = a})

instance NFData ListUsersInGroupResponse where
