{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns all the metrics filters associated with the specified log group.
-- The list returned in the response is ASCII-sorted by filter name.
--
-- By default, this operation returns up to 50 metric filters. If there are
-- more metric filters to list, the response would contain a @nextToken@
-- value in the response body. You can also limit the number of metric
-- filters returned in the response by specifying the @limit@ parameter in
-- the request.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeMetricFilters.html>
module Network.AWS.CloudWatchLogs.DescribeMetricFilters
    (
    -- * Request
      DescribeMetricFilters
    -- ** Request constructor
    , describeMetricFilters
    -- ** Request lenses
    , dmfrqFilterNamePrefix
    , dmfrqNextToken
    , dmfrqLimit
    , dmfrqLogGroupName

    -- * Response
    , DescribeMetricFiltersResponse
    -- ** Response constructor
    , describeMetricFiltersResponse
    -- ** Response lenses
    , dmfrsNextToken
    , dmfrsMetricFilters
    , dmfrsStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMetricFilters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmfrqFilterNamePrefix'
--
-- * 'dmfrqNextToken'
--
-- * 'dmfrqLimit'
--
-- * 'dmfrqLogGroupName'
data DescribeMetricFilters = DescribeMetricFilters'
    { _dmfrqFilterNamePrefix :: !(Maybe Text)
    , _dmfrqNextToken        :: !(Maybe Text)
    , _dmfrqLimit            :: !(Maybe Nat)
    , _dmfrqLogGroupName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMetricFilters' smart constructor.
describeMetricFilters :: Text -> DescribeMetricFilters
describeMetricFilters pLogGroupName_ =
    DescribeMetricFilters'
    { _dmfrqFilterNamePrefix = Nothing
    , _dmfrqNextToken = Nothing
    , _dmfrqLimit = Nothing
    , _dmfrqLogGroupName = pLogGroupName_
    }

-- | Will only return metric filters that match the provided
-- filterNamePrefix. If you don\'t specify a value, no prefix filter is
-- applied.
dmfrqFilterNamePrefix :: Lens' DescribeMetricFilters (Maybe Text)
dmfrqFilterNamePrefix = lens _dmfrqFilterNamePrefix (\ s a -> s{_dmfrqFilterNamePrefix = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- @DescribeMetricFilters@ request.
dmfrqNextToken :: Lens' DescribeMetricFilters (Maybe Text)
dmfrqNextToken = lens _dmfrqNextToken (\ s a -> s{_dmfrqNextToken = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the request would return up to 50 items.
dmfrqLimit :: Lens' DescribeMetricFilters (Maybe Natural)
dmfrqLimit = lens _dmfrqLimit (\ s a -> s{_dmfrqLimit = a}) . mapping _Nat;

-- | The log group name for which metric filters are to be listed.
dmfrqLogGroupName :: Lens' DescribeMetricFilters Text
dmfrqLogGroupName = lens _dmfrqLogGroupName (\ s a -> s{_dmfrqLogGroupName = a});

instance AWSRequest DescribeMetricFilters where
        type Sv DescribeMetricFilters = CloudWatchLogs
        type Rs DescribeMetricFilters =
             DescribeMetricFiltersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMetricFiltersResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "metricFilters" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["filterNamePrefix" .= _dmfrqFilterNamePrefix,
               "nextToken" .= _dmfrqNextToken,
               "limit" .= _dmfrqLimit,
               "logGroupName" .= _dmfrqLogGroupName]

instance ToPath DescribeMetricFilters where
        toPath = const "/"

instance ToQuery DescribeMetricFilters where
        toQuery = const mempty

-- | /See:/ 'describeMetricFiltersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmfrsNextToken'
--
-- * 'dmfrsMetricFilters'
--
-- * 'dmfrsStatus'
data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse'
    { _dmfrsNextToken     :: !(Maybe Text)
    , _dmfrsMetricFilters :: !(Maybe [MetricFilter])
    , _dmfrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMetricFiltersResponse' smart constructor.
describeMetricFiltersResponse :: Int -> DescribeMetricFiltersResponse
describeMetricFiltersResponse pStatus_ =
    DescribeMetricFiltersResponse'
    { _dmfrsNextToken = Nothing
    , _dmfrsMetricFilters = Nothing
    , _dmfrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dmfrsNextToken :: Lens' DescribeMetricFiltersResponse (Maybe Text)
dmfrsNextToken = lens _dmfrsNextToken (\ s a -> s{_dmfrsNextToken = a});

-- | FIXME: Undocumented member.
dmfrsMetricFilters :: Lens' DescribeMetricFiltersResponse [MetricFilter]
dmfrsMetricFilters = lens _dmfrsMetricFilters (\ s a -> s{_dmfrsMetricFilters = a}) . _Default;

-- | FIXME: Undocumented member.
dmfrsStatus :: Lens' DescribeMetricFiltersResponse Int
dmfrsStatus = lens _dmfrsStatus (\ s a -> s{_dmfrsStatus = a});
