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
-- Module      : Network.AWS.SageMaker.ListWorkteams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of work teams that you have defined in a region. The list may be empty if no work team satisfies the filter specified in the @NameContains@ parameter.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListWorkteams
    (
    -- * Creating a Request
      listWorkteams
    , ListWorkteams
    -- * Request Lenses
    , lwNameContains
    , lwNextToken
    , lwSortOrder
    , lwMaxResults
    , lwSortBy

    -- * Destructuring the Response
    , listWorkteamsResponse
    , ListWorkteamsResponse
    -- * Response Lenses
    , lwrsNextToken
    , lwrsResponseStatus
    , lwrsWorkteams
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listWorkteams' smart constructor.
data ListWorkteams = ListWorkteams'
  { _lwNameContains :: !(Maybe Text)
  , _lwNextToken    :: !(Maybe Text)
  , _lwSortOrder    :: !(Maybe SortOrder)
  , _lwMaxResults   :: !(Maybe Nat)
  , _lwSortBy       :: !(Maybe ListWorkteamsSortByOptions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWorkteams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwNameContains' - A string in the work team's name. This filter returns only work teams whose name contains the specified string.
--
-- * 'lwNextToken' - If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- * 'lwSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lwMaxResults' - The maximum number of work teams to return in each page of the response.
--
-- * 'lwSortBy' - The field to sort results by. The default is @CreationTime@ .
listWorkteams
    :: ListWorkteams
listWorkteams =
  ListWorkteams'
    { _lwNameContains = Nothing
    , _lwNextToken = Nothing
    , _lwSortOrder = Nothing
    , _lwMaxResults = Nothing
    , _lwSortBy = Nothing
    }


-- | A string in the work team's name. This filter returns only work teams whose name contains the specified string.
lwNameContains :: Lens' ListWorkteams (Maybe Text)
lwNameContains = lens _lwNameContains (\ s a -> s{_lwNameContains = a})

-- | If the result of the previous @ListWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
lwNextToken :: Lens' ListWorkteams (Maybe Text)
lwNextToken = lens _lwNextToken (\ s a -> s{_lwNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lwSortOrder :: Lens' ListWorkteams (Maybe SortOrder)
lwSortOrder = lens _lwSortOrder (\ s a -> s{_lwSortOrder = a})

-- | The maximum number of work teams to return in each page of the response.
lwMaxResults :: Lens' ListWorkteams (Maybe Natural)
lwMaxResults = lens _lwMaxResults (\ s a -> s{_lwMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @CreationTime@ .
lwSortBy :: Lens' ListWorkteams (Maybe ListWorkteamsSortByOptions)
lwSortBy = lens _lwSortBy (\ s a -> s{_lwSortBy = a})

instance AWSPager ListWorkteams where
        page rq rs
          | stop (rs ^. lwrsNextToken) = Nothing
          | stop (rs ^. lwrsWorkteams) = Nothing
          | otherwise =
            Just $ rq & lwNextToken .~ rs ^. lwrsNextToken

instance AWSRequest ListWorkteams where
        type Rs ListWorkteams = ListWorkteamsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListWorkteamsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "Workteams" .!@ mempty))

instance Hashable ListWorkteams where

instance NFData ListWorkteams where

instance ToHeaders ListWorkteams where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListWorkteams" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListWorkteams where
        toJSON ListWorkteams'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lwNameContains,
                  ("NextToken" .=) <$> _lwNextToken,
                  ("SortOrder" .=) <$> _lwSortOrder,
                  ("MaxResults" .=) <$> _lwMaxResults,
                  ("SortBy" .=) <$> _lwSortBy])

instance ToPath ListWorkteams where
        toPath = const "/"

instance ToQuery ListWorkteams where
        toQuery = const mempty

-- | /See:/ 'listWorkteamsResponse' smart constructor.
data ListWorkteamsResponse = ListWorkteamsResponse'
  { _lwrsNextToken      :: !(Maybe Text)
  , _lwrsResponseStatus :: !Int
  , _lwrsWorkteams      :: ![Workteam]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWorkteamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
--
-- * 'lwrsResponseStatus' - -- | The response status code.
--
-- * 'lwrsWorkteams' - An array of @Workteam@ objects, each describing a work team.
listWorkteamsResponse
    :: Int -- ^ 'lwrsResponseStatus'
    -> ListWorkteamsResponse
listWorkteamsResponse pResponseStatus_ =
  ListWorkteamsResponse'
    { _lwrsNextToken = Nothing
    , _lwrsResponseStatus = pResponseStatus_
    , _lwrsWorkteams = mempty
    }


-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
lwrsNextToken :: Lens' ListWorkteamsResponse (Maybe Text)
lwrsNextToken = lens _lwrsNextToken (\ s a -> s{_lwrsNextToken = a})

-- | -- | The response status code.
lwrsResponseStatus :: Lens' ListWorkteamsResponse Int
lwrsResponseStatus = lens _lwrsResponseStatus (\ s a -> s{_lwrsResponseStatus = a})

-- | An array of @Workteam@ objects, each describing a work team.
lwrsWorkteams :: Lens' ListWorkteamsResponse [Workteam]
lwrsWorkteams = lens _lwrsWorkteams (\ s a -> s{_lwrsWorkteams = a}) . _Coerce

instance NFData ListWorkteamsResponse where
