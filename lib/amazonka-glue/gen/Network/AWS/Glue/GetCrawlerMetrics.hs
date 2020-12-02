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
-- Module      : Network.AWS.Glue.GetCrawlerMetrics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metrics about specified crawlers.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetCrawlerMetrics
    (
    -- * Creating a Request
      getCrawlerMetrics
    , GetCrawlerMetrics
    -- * Request Lenses
    , gcmNextToken
    , gcmMaxResults
    , gcmCrawlerNameList

    -- * Destructuring the Response
    , getCrawlerMetricsResponse
    , GetCrawlerMetricsResponse
    -- * Response Lenses
    , gcmrsCrawlerMetricsList
    , gcmrsNextToken
    , gcmrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCrawlerMetrics' smart constructor.
data GetCrawlerMetrics = GetCrawlerMetrics'
  { _gcmNextToken       :: !(Maybe Text)
  , _gcmMaxResults      :: !(Maybe Nat)
  , _gcmCrawlerNameList :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCrawlerMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gcmMaxResults' - The maximum size of a list to return.
--
-- * 'gcmCrawlerNameList' - A list of the names of crawlers about which to retrieve metrics.
getCrawlerMetrics
    :: GetCrawlerMetrics
getCrawlerMetrics =
  GetCrawlerMetrics'
    { _gcmNextToken = Nothing
    , _gcmMaxResults = Nothing
    , _gcmCrawlerNameList = Nothing
    }


-- | A continuation token, if this is a continuation call.
gcmNextToken :: Lens' GetCrawlerMetrics (Maybe Text)
gcmNextToken = lens _gcmNextToken (\ s a -> s{_gcmNextToken = a})

-- | The maximum size of a list to return.
gcmMaxResults :: Lens' GetCrawlerMetrics (Maybe Natural)
gcmMaxResults = lens _gcmMaxResults (\ s a -> s{_gcmMaxResults = a}) . mapping _Nat

-- | A list of the names of crawlers about which to retrieve metrics.
gcmCrawlerNameList :: Lens' GetCrawlerMetrics [Text]
gcmCrawlerNameList = lens _gcmCrawlerNameList (\ s a -> s{_gcmCrawlerNameList = a}) . _Default . _Coerce

instance AWSPager GetCrawlerMetrics where
        page rq rs
          | stop (rs ^. gcmrsNextToken) = Nothing
          | stop (rs ^. gcmrsCrawlerMetricsList) = Nothing
          | otherwise =
            Just $ rq & gcmNextToken .~ rs ^. gcmrsNextToken

instance AWSRequest GetCrawlerMetrics where
        type Rs GetCrawlerMetrics = GetCrawlerMetricsResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetCrawlerMetricsResponse' <$>
                   (x .?> "CrawlerMetricsList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetCrawlerMetrics where

instance NFData GetCrawlerMetrics where

instance ToHeaders GetCrawlerMetrics where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetCrawlerMetrics" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCrawlerMetrics where
        toJSON GetCrawlerMetrics'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gcmNextToken,
                  ("MaxResults" .=) <$> _gcmMaxResults,
                  ("CrawlerNameList" .=) <$> _gcmCrawlerNameList])

instance ToPath GetCrawlerMetrics where
        toPath = const "/"

instance ToQuery GetCrawlerMetrics where
        toQuery = const mempty

-- | /See:/ 'getCrawlerMetricsResponse' smart constructor.
data GetCrawlerMetricsResponse = GetCrawlerMetricsResponse'
  { _gcmrsCrawlerMetricsList :: !(Maybe [CrawlerMetrics])
  , _gcmrsNextToken          :: !(Maybe Text)
  , _gcmrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCrawlerMetricsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmrsCrawlerMetricsList' - A list of metrics for the specified crawler.
--
-- * 'gcmrsNextToken' - A continuation token, if the returned list does not contain the last metric available.
--
-- * 'gcmrsResponseStatus' - -- | The response status code.
getCrawlerMetricsResponse
    :: Int -- ^ 'gcmrsResponseStatus'
    -> GetCrawlerMetricsResponse
getCrawlerMetricsResponse pResponseStatus_ =
  GetCrawlerMetricsResponse'
    { _gcmrsCrawlerMetricsList = Nothing
    , _gcmrsNextToken = Nothing
    , _gcmrsResponseStatus = pResponseStatus_
    }


-- | A list of metrics for the specified crawler.
gcmrsCrawlerMetricsList :: Lens' GetCrawlerMetricsResponse [CrawlerMetrics]
gcmrsCrawlerMetricsList = lens _gcmrsCrawlerMetricsList (\ s a -> s{_gcmrsCrawlerMetricsList = a}) . _Default . _Coerce

-- | A continuation token, if the returned list does not contain the last metric available.
gcmrsNextToken :: Lens' GetCrawlerMetricsResponse (Maybe Text)
gcmrsNextToken = lens _gcmrsNextToken (\ s a -> s{_gcmrsNextToken = a})

-- | -- | The response status code.
gcmrsResponseStatus :: Lens' GetCrawlerMetricsResponse Int
gcmrsResponseStatus = lens _gcmrsResponseStatus (\ s a -> s{_gcmrsResponseStatus = a})

instance NFData GetCrawlerMetricsResponse where
