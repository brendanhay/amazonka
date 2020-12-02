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
-- Module      : Network.AWS.StepFunctions.ListStateMachines
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing state machines.
--
--
-- If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged.
--
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListStateMachines
    (
    -- * Creating a Request
      listStateMachines
    , ListStateMachines
    -- * Request Lenses
    , lsmNextToken
    , lsmMaxResults

    -- * Destructuring the Response
    , listStateMachinesResponse
    , ListStateMachinesResponse
    -- * Response Lenses
    , lsmrsNextToken
    , lsmrsResponseStatus
    , lsmrsStateMachines
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'listStateMachines' smart constructor.
data ListStateMachines = ListStateMachines'
  { _lsmNextToken  :: !(Maybe Text)
  , _lsmMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStateMachines' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsmNextToken' - If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
--
-- * 'lsmMaxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 100. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
listStateMachines
    :: ListStateMachines
listStateMachines =
  ListStateMachines' {_lsmNextToken = Nothing, _lsmMaxResults = Nothing}


-- | If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
lsmNextToken :: Lens' ListStateMachines (Maybe Text)
lsmNextToken = lens _lsmNextToken (\ s a -> s{_lsmNextToken = a})

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 100. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
lsmMaxResults :: Lens' ListStateMachines (Maybe Natural)
lsmMaxResults = lens _lsmMaxResults (\ s a -> s{_lsmMaxResults = a}) . mapping _Nat

instance AWSPager ListStateMachines where
        page rq rs
          | stop (rs ^. lsmrsNextToken) = Nothing
          | stop (rs ^. lsmrsStateMachines) = Nothing
          | otherwise =
            Just $ rq & lsmNextToken .~ rs ^. lsmrsNextToken

instance AWSRequest ListStateMachines where
        type Rs ListStateMachines = ListStateMachinesResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 ListStateMachinesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "stateMachines" .!@ mempty))

instance Hashable ListStateMachines where

instance NFData ListStateMachines where

instance ToHeaders ListStateMachines where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.ListStateMachines" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListStateMachines where
        toJSON ListStateMachines'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lsmNextToken,
                  ("maxResults" .=) <$> _lsmMaxResults])

instance ToPath ListStateMachines where
        toPath = const "/"

instance ToQuery ListStateMachines where
        toQuery = const mempty

-- | /See:/ 'listStateMachinesResponse' smart constructor.
data ListStateMachinesResponse = ListStateMachinesResponse'
  { _lsmrsNextToken      :: !(Maybe Text)
  , _lsmrsResponseStatus :: !Int
  , _lsmrsStateMachines  :: ![StateMachineListItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStateMachinesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsmrsNextToken' - If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
--
-- * 'lsmrsResponseStatus' - -- | The response status code.
--
-- * 'lsmrsStateMachines' - Undocumented member.
listStateMachinesResponse
    :: Int -- ^ 'lsmrsResponseStatus'
    -> ListStateMachinesResponse
listStateMachinesResponse pResponseStatus_ =
  ListStateMachinesResponse'
    { _lsmrsNextToken = Nothing
    , _lsmrsResponseStatus = pResponseStatus_
    , _lsmrsStateMachines = mempty
    }


-- | If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
lsmrsNextToken :: Lens' ListStateMachinesResponse (Maybe Text)
lsmrsNextToken = lens _lsmrsNextToken (\ s a -> s{_lsmrsNextToken = a})

-- | -- | The response status code.
lsmrsResponseStatus :: Lens' ListStateMachinesResponse Int
lsmrsResponseStatus = lens _lsmrsResponseStatus (\ s a -> s{_lsmrsResponseStatus = a})

-- | Undocumented member.
lsmrsStateMachines :: Lens' ListStateMachinesResponse [StateMachineListItem]
lsmrsStateMachines = lens _lsmrsStateMachines (\ s a -> s{_lsmrsStateMachines = a}) . _Coerce

instance NFData ListStateMachinesResponse where
