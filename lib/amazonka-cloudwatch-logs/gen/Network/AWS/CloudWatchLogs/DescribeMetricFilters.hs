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
-- Module      : Network.AWS.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified metric filters. You can list all the metric filters or filter the results by log name, prefix, metric name, or metric namespace. The results are ASCII-sorted by filter name.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeMetricFilters
    (
    -- * Creating a Request
      describeMetricFilters
    , DescribeMetricFilters
    -- * Request Lenses
    , dmfFilterNamePrefix
    , dmfMetricName
    , dmfLogGroupName
    , dmfNextToken
    , dmfMetricNamespace
    , dmfLimit

    -- * Destructuring the Response
    , describeMetricFiltersResponse
    , DescribeMetricFiltersResponse
    -- * Response Lenses
    , dmfrsNextToken
    , dmfrsMetricFilters
    , dmfrsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeMetricFilters' smart constructor.
data DescribeMetricFilters = DescribeMetricFilters'
  { _dmfFilterNamePrefix :: !(Maybe Text)
  , _dmfMetricName       :: !(Maybe Text)
  , _dmfLogGroupName     :: !(Maybe Text)
  , _dmfNextToken        :: !(Maybe Text)
  , _dmfMetricNamespace  :: !(Maybe Text)
  , _dmfLimit            :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMetricFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmfFilterNamePrefix' - The prefix to match.
--
-- * 'dmfMetricName' - Undocumented member.
--
-- * 'dmfLogGroupName' - The name of the log group.
--
-- * 'dmfNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dmfMetricNamespace' - The namespace of the CloudWatch metric.
--
-- * 'dmfLimit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
describeMetricFilters
    :: DescribeMetricFilters
describeMetricFilters =
  DescribeMetricFilters'
    { _dmfFilterNamePrefix = Nothing
    , _dmfMetricName = Nothing
    , _dmfLogGroupName = Nothing
    , _dmfNextToken = Nothing
    , _dmfMetricNamespace = Nothing
    , _dmfLimit = Nothing
    }


-- | The prefix to match.
dmfFilterNamePrefix :: Lens' DescribeMetricFilters (Maybe Text)
dmfFilterNamePrefix = lens _dmfFilterNamePrefix (\ s a -> s{_dmfFilterNamePrefix = a})

-- | Undocumented member.
dmfMetricName :: Lens' DescribeMetricFilters (Maybe Text)
dmfMetricName = lens _dmfMetricName (\ s a -> s{_dmfMetricName = a})

-- | The name of the log group.
dmfLogGroupName :: Lens' DescribeMetricFilters (Maybe Text)
dmfLogGroupName = lens _dmfLogGroupName (\ s a -> s{_dmfLogGroupName = a})

-- | The token for the next set of items to return. (You received this token from a previous call.)
dmfNextToken :: Lens' DescribeMetricFilters (Maybe Text)
dmfNextToken = lens _dmfNextToken (\ s a -> s{_dmfNextToken = a})

-- | The namespace of the CloudWatch metric.
dmfMetricNamespace :: Lens' DescribeMetricFilters (Maybe Text)
dmfMetricNamespace = lens _dmfMetricNamespace (\ s a -> s{_dmfMetricNamespace = a})

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
dmfLimit :: Lens' DescribeMetricFilters (Maybe Natural)
dmfLimit = lens _dmfLimit (\ s a -> s{_dmfLimit = a}) . mapping _Nat

instance AWSPager DescribeMetricFilters where
        page rq rs
          | stop (rs ^. dmfrsNextToken) = Nothing
          | stop (rs ^. dmfrsMetricFilters) = Nothing
          | otherwise =
            Just $ rq & dmfNextToken .~ rs ^. dmfrsNextToken

instance AWSRequest DescribeMetricFilters where
        type Rs DescribeMetricFilters =
             DescribeMetricFiltersResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMetricFiltersResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "metricFilters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMetricFilters where

instance NFData DescribeMetricFilters where

instance ToHeaders DescribeMetricFilters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeMetricFilters" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMetricFilters where
        toJSON DescribeMetricFilters'{..}
          = object
              (catMaybes
                 [("filterNamePrefix" .=) <$> _dmfFilterNamePrefix,
                  ("metricName" .=) <$> _dmfMetricName,
                  ("logGroupName" .=) <$> _dmfLogGroupName,
                  ("nextToken" .=) <$> _dmfNextToken,
                  ("metricNamespace" .=) <$> _dmfMetricNamespace,
                  ("limit" .=) <$> _dmfLimit])

instance ToPath DescribeMetricFilters where
        toPath = const "/"

instance ToQuery DescribeMetricFilters where
        toQuery = const mempty

-- | /See:/ 'describeMetricFiltersResponse' smart constructor.
data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse'
  { _dmfrsNextToken      :: !(Maybe Text)
  , _dmfrsMetricFilters  :: !(Maybe [MetricFilter])
  , _dmfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMetricFiltersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmfrsNextToken' - Undocumented member.
--
-- * 'dmfrsMetricFilters' - The metric filters.
--
-- * 'dmfrsResponseStatus' - -- | The response status code.
describeMetricFiltersResponse
    :: Int -- ^ 'dmfrsResponseStatus'
    -> DescribeMetricFiltersResponse
describeMetricFiltersResponse pResponseStatus_ =
  DescribeMetricFiltersResponse'
    { _dmfrsNextToken = Nothing
    , _dmfrsMetricFilters = Nothing
    , _dmfrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
dmfrsNextToken :: Lens' DescribeMetricFiltersResponse (Maybe Text)
dmfrsNextToken = lens _dmfrsNextToken (\ s a -> s{_dmfrsNextToken = a})

-- | The metric filters.
dmfrsMetricFilters :: Lens' DescribeMetricFiltersResponse [MetricFilter]
dmfrsMetricFilters = lens _dmfrsMetricFilters (\ s a -> s{_dmfrsMetricFilters = a}) . _Default . _Coerce

-- | -- | The response status code.
dmfrsResponseStatus :: Lens' DescribeMetricFiltersResponse Int
dmfrsResponseStatus = lens _dmfrsResponseStatus (\ s a -> s{_dmfrsResponseStatus = a})

instance NFData DescribeMetricFiltersResponse where
