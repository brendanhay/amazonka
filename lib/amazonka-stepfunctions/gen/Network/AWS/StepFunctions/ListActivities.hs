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
-- Module      : Network.AWS.StepFunctions.ListActivities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing activities.
--
--
-- If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged.
--
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListActivities
    (
    -- * Creating a Request
      listActivities
    , ListActivities
    -- * Request Lenses
    , laNextToken
    , laMaxResults

    -- * Destructuring the Response
    , listActivitiesResponse
    , ListActivitiesResponse
    -- * Response Lenses
    , larsNextToken
    , larsResponseStatus
    , larsActivities
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'listActivities' smart constructor.
data ListActivities = ListActivities'
  { _laNextToken  :: !(Maybe Text)
  , _laMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListActivities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken' - If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
--
-- * 'laMaxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 100. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
listActivities
    :: ListActivities
listActivities =
  ListActivities' {_laNextToken = Nothing, _laMaxResults = Nothing}


-- | If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
laNextToken :: Lens' ListActivities (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 100. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
laMaxResults :: Lens' ListActivities (Maybe Natural)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a}) . mapping _Nat

instance AWSPager ListActivities where
        page rq rs
          | stop (rs ^. larsNextToken) = Nothing
          | stop (rs ^. larsActivities) = Nothing
          | otherwise =
            Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListActivities where
        type Rs ListActivities = ListActivitiesResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 ListActivitiesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "activities" .!@ mempty))

instance Hashable ListActivities where

instance NFData ListActivities where

instance ToHeaders ListActivities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.ListActivities" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListActivities where
        toJSON ListActivities'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _laNextToken,
                  ("maxResults" .=) <$> _laMaxResults])

instance ToPath ListActivities where
        toPath = const "/"

instance ToQuery ListActivities where
        toQuery = const mempty

-- | /See:/ 'listActivitiesResponse' smart constructor.
data ListActivitiesResponse = ListActivitiesResponse'
  { _larsNextToken      :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  , _larsActivities     :: ![ActivityListItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsNextToken' - If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
--
-- * 'larsResponseStatus' - -- | The response status code.
--
-- * 'larsActivities' - The list of activities.
listActivitiesResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListActivitiesResponse
listActivitiesResponse pResponseStatus_ =
  ListActivitiesResponse'
    { _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    , _larsActivities = mempty
    }


-- | If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
larsNextToken :: Lens' ListActivitiesResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListActivitiesResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

-- | The list of activities.
larsActivities :: Lens' ListActivitiesResponse [ActivityListItem]
larsActivities = lens _larsActivities (\ s a -> s{_larsActivities = a}) . _Coerce

instance NFData ListActivitiesResponse where
