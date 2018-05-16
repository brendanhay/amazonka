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
-- Module      : Network.AWS.IoTAnalytics.ListDatasets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about data sets.
--
--
module Network.AWS.IoTAnalytics.ListDatasets
    (
    -- * Creating a Request
      listDatasets
    , ListDatasets
    -- * Request Lenses
    , lNextToken
    , lMaxResults

    -- * Destructuring the Response
    , listDatasetsResponse
    , ListDatasetsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsDatasetSummaries
    , lrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { _lNextToken  :: !(Maybe Text)
  , _lMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDatasets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - The token for the next set of results.
--
-- * 'lMaxResults' - The maximum number of results to return in this request. The default value is 100.
listDatasets
    :: ListDatasets
listDatasets = ListDatasets' {_lNextToken = Nothing, _lMaxResults = Nothing}


-- | The token for the next set of results.
lNextToken :: Lens' ListDatasets (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The maximum number of results to return in this request. The default value is 100.
lMaxResults :: Lens' ListDatasets (Maybe Natural)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a}) . mapping _Nat

instance AWSRequest ListDatasets where
        type Rs ListDatasets = ListDatasetsResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 ListDatasetsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "datasetSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDatasets where

instance NFData ListDatasets where

instance ToHeaders ListDatasets where
        toHeaders = const mempty

instance ToPath ListDatasets where
        toPath = const "/datasets"

instance ToQuery ListDatasets where
        toQuery ListDatasets'{..}
          = mconcat
              ["nextToken" =: _lNextToken,
               "maxResults" =: _lMaxResults]

-- | /See:/ 'listDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { _lrsNextToken        :: !(Maybe Text)
  , _lrsDatasetSummaries :: !(Maybe [DatasetSummary])
  , _lrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDatasetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- * 'lrsDatasetSummaries' - A list of "DatasetSummary" objects.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listDatasetsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListDatasetsResponse
listDatasetsResponse pResponseStatus_ =
  ListDatasetsResponse'
    { _lrsNextToken = Nothing
    , _lrsDatasetSummaries = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | The token to retrieve the next set of results, or @null@ if there are no more results.
lrsNextToken :: Lens' ListDatasetsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | A list of "DatasetSummary" objects.
lrsDatasetSummaries :: Lens' ListDatasetsResponse [DatasetSummary]
lrsDatasetSummaries = lens _lrsDatasetSummaries (\ s a -> s{_lrsDatasetSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListDatasetsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListDatasetsResponse where
