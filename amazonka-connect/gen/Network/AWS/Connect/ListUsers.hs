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
-- Module      : Network.AWS.Connect.ListUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @UserSummaryList@ , which is an array of @UserSummary@ objects.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUsers
    (
    -- * Creating a Request
      listUsers
    , ListUsers
    -- * Request Lenses
    , luNextToken
    , luMaxResults
    , luInstanceId

    -- * Destructuring the Response
    , listUsersResponse
    , ListUsersResponse
    -- * Response Lenses
    , lursNextToken
    , lursUserSummaryList
    , lursResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUsers' smart constructor.
data ListUsers = ListUsers'
  { _luNextToken  :: !(Maybe Text)
  , _luMaxResults :: !(Maybe Nat)
  , _luInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'luMaxResults' - The maximum number of results to return in the response.
--
-- * 'luInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
listUsers
    :: Text -- ^ 'luInstanceId'
    -> ListUsers
listUsers pInstanceId_ =
  ListUsers'
    { _luNextToken = Nothing
    , _luMaxResults = Nothing
    , _luInstanceId = pInstanceId_
    }


-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
luNextToken :: Lens' ListUsers (Maybe Text)
luNextToken = lens _luNextToken (\ s a -> s{_luNextToken = a})

-- | The maximum number of results to return in the response.
luMaxResults :: Lens' ListUsers (Maybe Natural)
luMaxResults = lens _luMaxResults (\ s a -> s{_luMaxResults = a}) . mapping _Nat

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
luInstanceId :: Lens' ListUsers Text
luInstanceId = lens _luInstanceId (\ s a -> s{_luInstanceId = a})

instance AWSPager ListUsers where
        page rq rs
          | stop (rs ^. lursNextToken) = Nothing
          | stop (rs ^. lursUserSummaryList) = Nothing
          | otherwise =
            Just $ rq & luNextToken .~ rs ^. lursNextToken

instance AWSRequest ListUsers where
        type Rs ListUsers = ListUsersResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 ListUsersResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "UserSummaryList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListUsers where

instance NFData ListUsers where

instance ToHeaders ListUsers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListUsers where
        toPath ListUsers'{..}
          = mconcat ["/users-summary/", toBS _luInstanceId]

instance ToQuery ListUsers where
        toQuery ListUsers'{..}
          = mconcat
              ["nextToken" =: _luNextToken,
               "maxResults" =: _luMaxResults]

-- | /See:/ 'listUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { _lursNextToken       :: !(Maybe Text)
  , _lursUserSummaryList :: !(Maybe [UserSummary])
  , _lursResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursNextToken' - A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results.
--
-- * 'lursUserSummaryList' - An array of @UserSummary@ objects that contain information about the users in your instance.
--
-- * 'lursResponseStatus' - -- | The response status code.
listUsersResponse
    :: Int -- ^ 'lursResponseStatus'
    -> ListUsersResponse
listUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { _lursNextToken = Nothing
    , _lursUserSummaryList = Nothing
    , _lursResponseStatus = pResponseStatus_
    }


-- | A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results.
lursNextToken :: Lens' ListUsersResponse (Maybe Text)
lursNextToken = lens _lursNextToken (\ s a -> s{_lursNextToken = a})

-- | An array of @UserSummary@ objects that contain information about the users in your instance.
lursUserSummaryList :: Lens' ListUsersResponse [UserSummary]
lursUserSummaryList = lens _lursUserSummaryList (\ s a -> s{_lursUserSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
lursResponseStatus :: Lens' ListUsersResponse Int
lursResponseStatus = lens _lursResponseStatus (\ s a -> s{_lursResponseStatus = a})

instance NFData ListUsersResponse where
