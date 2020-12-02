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
-- Module      : Network.AWS.CognitoIdentityProvider.ListGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups associated with a user pool.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.ListGroups
    (
    -- * Creating a Request
      listGroups
    , ListGroups
    -- * Request Lenses
    , lgNextToken
    , lgLimit
    , lgUserPoolId

    -- * Destructuring the Response
    , listGroupsResponse
    , ListGroupsResponse
    -- * Response Lenses
    , lgrsGroups
    , lgrsNextToken
    , lgrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGroups' smart constructor.
data ListGroups = ListGroups'
  { _lgNextToken  :: !(Maybe Text)
  , _lgLimit      :: !(Maybe Nat)
  , _lgUserPoolId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lgLimit' - The limit of the request to list groups.
--
-- * 'lgUserPoolId' - The user pool ID for the user pool.
listGroups
    :: Text -- ^ 'lgUserPoolId'
    -> ListGroups
listGroups pUserPoolId_ =
  ListGroups'
    {_lgNextToken = Nothing, _lgLimit = Nothing, _lgUserPoolId = pUserPoolId_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lgNextToken :: Lens' ListGroups (Maybe Text)
lgNextToken = lens _lgNextToken (\ s a -> s{_lgNextToken = a})

-- | The limit of the request to list groups.
lgLimit :: Lens' ListGroups (Maybe Natural)
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . mapping _Nat

-- | The user pool ID for the user pool.
lgUserPoolId :: Lens' ListGroups Text
lgUserPoolId = lens _lgUserPoolId (\ s a -> s{_lgUserPoolId = a})

instance AWSRequest ListGroups where
        type Rs ListGroups = ListGroupsResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ListGroupsResponse' <$>
                   (x .?> "Groups" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListGroups where

instance NFData ListGroups where

instance ToHeaders ListGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ListGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGroups where
        toJSON ListGroups'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lgNextToken,
                  ("Limit" .=) <$> _lgLimit,
                  Just ("UserPoolId" .= _lgUserPoolId)])

instance ToPath ListGroups where
        toPath = const "/"

instance ToQuery ListGroups where
        toQuery = const mempty

-- | /See:/ 'listGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { _lgrsGroups         :: !(Maybe [GroupType])
  , _lgrsNextToken      :: !(Maybe Text)
  , _lgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsGroups' - The group objects for the groups.
--
-- * 'lgrsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lgrsResponseStatus' - -- | The response status code.
listGroupsResponse
    :: Int -- ^ 'lgrsResponseStatus'
    -> ListGroupsResponse
listGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { _lgrsGroups = Nothing
    , _lgrsNextToken = Nothing
    , _lgrsResponseStatus = pResponseStatus_
    }


-- | The group objects for the groups.
lgrsGroups :: Lens' ListGroupsResponse [GroupType]
lgrsGroups = lens _lgrsGroups (\ s a -> s{_lgrsGroups = a}) . _Default . _Coerce

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lgrsNextToken :: Lens' ListGroupsResponse (Maybe Text)
lgrsNextToken = lens _lgrsNextToken (\ s a -> s{_lgrsNextToken = a})

-- | -- | The response status code.
lgrsResponseStatus :: Lens' ListGroupsResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\ s a -> s{_lgrsResponseStatus = a})

instance NFData ListGroupsResponse where
