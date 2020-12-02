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
-- Module      : Network.AWS.IoT.ListJobExecutionsForThing
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for the specified thing.
--
--
module Network.AWS.IoT.ListJobExecutionsForThing
    (
    -- * Creating a Request
      listJobExecutionsForThing
    , ListJobExecutionsForThing
    -- * Request Lenses
    , ljeftStatus
    , ljeftNextToken
    , ljeftMaxResults
    , ljeftThingName

    -- * Destructuring the Response
    , listJobExecutionsForThingResponse
    , ListJobExecutionsForThingResponse
    -- * Response Lenses
    , ljeftrsExecutionSummaries
    , ljeftrsNextToken
    , ljeftrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJobExecutionsForThing' smart constructor.
data ListJobExecutionsForThing = ListJobExecutionsForThing'
  { _ljeftStatus     :: !(Maybe JobExecutionStatus)
  , _ljeftNextToken  :: !(Maybe Text)
  , _ljeftMaxResults :: !(Maybe Nat)
  , _ljeftThingName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobExecutionsForThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljeftStatus' - An optional filter that lets you search for jobs that have the specified status.
--
-- * 'ljeftNextToken' - The token to retrieve the next set of results.
--
-- * 'ljeftMaxResults' - The maximum number of results to be returned per request.
--
-- * 'ljeftThingName' - The thing name.
listJobExecutionsForThing
    :: Text -- ^ 'ljeftThingName'
    -> ListJobExecutionsForThing
listJobExecutionsForThing pThingName_ =
  ListJobExecutionsForThing'
    { _ljeftStatus = Nothing
    , _ljeftNextToken = Nothing
    , _ljeftMaxResults = Nothing
    , _ljeftThingName = pThingName_
    }


-- | An optional filter that lets you search for jobs that have the specified status.
ljeftStatus :: Lens' ListJobExecutionsForThing (Maybe JobExecutionStatus)
ljeftStatus = lens _ljeftStatus (\ s a -> s{_ljeftStatus = a})

-- | The token to retrieve the next set of results.
ljeftNextToken :: Lens' ListJobExecutionsForThing (Maybe Text)
ljeftNextToken = lens _ljeftNextToken (\ s a -> s{_ljeftNextToken = a})

-- | The maximum number of results to be returned per request.
ljeftMaxResults :: Lens' ListJobExecutionsForThing (Maybe Natural)
ljeftMaxResults = lens _ljeftMaxResults (\ s a -> s{_ljeftMaxResults = a}) . mapping _Nat

-- | The thing name.
ljeftThingName :: Lens' ListJobExecutionsForThing Text
ljeftThingName = lens _ljeftThingName (\ s a -> s{_ljeftThingName = a})

instance AWSRequest ListJobExecutionsForThing where
        type Rs ListJobExecutionsForThing =
             ListJobExecutionsForThingResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListJobExecutionsForThingResponse' <$>
                   (x .?> "executionSummaries" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListJobExecutionsForThing where

instance NFData ListJobExecutionsForThing where

instance ToHeaders ListJobExecutionsForThing where
        toHeaders = const mempty

instance ToPath ListJobExecutionsForThing where
        toPath ListJobExecutionsForThing'{..}
          = mconcat ["/things/", toBS _ljeftThingName, "/jobs"]

instance ToQuery ListJobExecutionsForThing where
        toQuery ListJobExecutionsForThing'{..}
          = mconcat
              ["status" =: _ljeftStatus,
               "nextToken" =: _ljeftNextToken,
               "maxResults" =: _ljeftMaxResults]

-- | /See:/ 'listJobExecutionsForThingResponse' smart constructor.
data ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse'
  { _ljeftrsExecutionSummaries :: !(Maybe [JobExecutionSummaryForThing])
  , _ljeftrsNextToken          :: !(Maybe Text)
  , _ljeftrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobExecutionsForThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljeftrsExecutionSummaries' - A list of job execution summaries.
--
-- * 'ljeftrsNextToken' - The token for the next set of results, or __null__ if there are no additional results.
--
-- * 'ljeftrsResponseStatus' - -- | The response status code.
listJobExecutionsForThingResponse
    :: Int -- ^ 'ljeftrsResponseStatus'
    -> ListJobExecutionsForThingResponse
listJobExecutionsForThingResponse pResponseStatus_ =
  ListJobExecutionsForThingResponse'
    { _ljeftrsExecutionSummaries = Nothing
    , _ljeftrsNextToken = Nothing
    , _ljeftrsResponseStatus = pResponseStatus_
    }


-- | A list of job execution summaries.
ljeftrsExecutionSummaries :: Lens' ListJobExecutionsForThingResponse [JobExecutionSummaryForThing]
ljeftrsExecutionSummaries = lens _ljeftrsExecutionSummaries (\ s a -> s{_ljeftrsExecutionSummaries = a}) . _Default . _Coerce

-- | The token for the next set of results, or __null__ if there are no additional results.
ljeftrsNextToken :: Lens' ListJobExecutionsForThingResponse (Maybe Text)
ljeftrsNextToken = lens _ljeftrsNextToken (\ s a -> s{_ljeftrsNextToken = a})

-- | -- | The response status code.
ljeftrsResponseStatus :: Lens' ListJobExecutionsForThingResponse Int
ljeftrsResponseStatus = lens _ljeftrsResponseStatus (\ s a -> s{_ljeftrsResponseStatus = a})

instance NFData ListJobExecutionsForThingResponse
         where
