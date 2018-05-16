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
-- Module      : Network.AWS.IoTAnalytics.ListPipelines
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of pipelines.
--
--
module Network.AWS.IoTAnalytics.ListPipelines
    (
    -- * Creating a Request
      listPipelines
    , ListPipelines
    -- * Request Lenses
    , lpNextToken
    , lpMaxResults

    -- * Destructuring the Response
    , listPipelinesResponse
    , ListPipelinesResponse
    -- * Response Lenses
    , lprsPipelineSummaries
    , lprsNextToken
    , lprsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { _lpNextToken  :: !(Maybe Text)
  , _lpMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelines' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - The token for the next set of results.
--
-- * 'lpMaxResults' - The maximum number of results to return in this request. The default value is 100.
listPipelines
    :: ListPipelines
listPipelines = ListPipelines' {_lpNextToken = Nothing, _lpMaxResults = Nothing}


-- | The token for the next set of results.
lpNextToken :: Lens' ListPipelines (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

-- | The maximum number of results to return in this request. The default value is 100.
lpMaxResults :: Lens' ListPipelines (Maybe Natural)
lpMaxResults = lens _lpMaxResults (\ s a -> s{_lpMaxResults = a}) . mapping _Nat

instance AWSRequest ListPipelines where
        type Rs ListPipelines = ListPipelinesResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' <$>
                   (x .?> "pipelineSummaries" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPipelines where

instance NFData ListPipelines where

instance ToHeaders ListPipelines where
        toHeaders = const mempty

instance ToPath ListPipelines where
        toPath = const "/pipelines"

instance ToQuery ListPipelines where
        toQuery ListPipelines'{..}
          = mconcat
              ["nextToken" =: _lpNextToken,
               "maxResults" =: _lpMaxResults]

-- | /See:/ 'listPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { _lprsPipelineSummaries :: !(Maybe [PipelineSummary])
  , _lprsNextToken         :: !(Maybe Text)
  , _lprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelinesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsPipelineSummaries' - A list of "PipelineSummary" objects.
--
-- * 'lprsNextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPipelinesResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPipelinesResponse
listPipelinesResponse pResponseStatus_ =
  ListPipelinesResponse'
    { _lprsPipelineSummaries = Nothing
    , _lprsNextToken = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | A list of "PipelineSummary" objects.
lprsPipelineSummaries :: Lens' ListPipelinesResponse [PipelineSummary]
lprsPipelineSummaries = lens _lprsPipelineSummaries (\ s a -> s{_lprsPipelineSummaries = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
lprsNextToken :: Lens' ListPipelinesResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPipelinesResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPipelinesResponse where
