{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns all the subscription filters associated with the specified log
-- group. The list returned in the response is ASCII-sorted by filter name.
--
-- By default, this operation returns up to 50 subscription filters. If
-- there are more subscription filters to list, the response would contain
-- a @nextToken@ value in the response body. You can also limit the number
-- of subscription filters returned in the response by specifying the
-- @limit@ parameter in the request.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html>
module Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
    (
    -- * Request
      DescribeSubscriptionFilters
    -- ** Request constructor
    , describeSubscriptionFilters
    -- ** Request lenses
    , dsfrqFilterNamePrefix
    , dsfrqNextToken
    , dsfrqLimit
    , dsfrqLogGroupName

    -- * Response
    , DescribeSubscriptionFiltersResponse
    -- ** Response constructor
    , describeSubscriptionFiltersResponse
    -- ** Response lenses
    , dsfrsSubscriptionFilters
    , dsfrsNextToken
    , dsfrsStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeSubscriptionFilters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrqFilterNamePrefix'
--
-- * 'dsfrqNextToken'
--
-- * 'dsfrqLimit'
--
-- * 'dsfrqLogGroupName'
data DescribeSubscriptionFilters = DescribeSubscriptionFilters'
    { _dsfrqFilterNamePrefix :: !(Maybe Text)
    , _dsfrqNextToken        :: !(Maybe Text)
    , _dsfrqLimit            :: !(Maybe Nat)
    , _dsfrqLogGroupName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSubscriptionFilters' smart constructor.
describeSubscriptionFilters :: Text -> DescribeSubscriptionFilters
describeSubscriptionFilters pLogGroupName_ =
    DescribeSubscriptionFilters'
    { _dsfrqFilterNamePrefix = Nothing
    , _dsfrqNextToken = Nothing
    , _dsfrqLimit = Nothing
    , _dsfrqLogGroupName = pLogGroupName_
    }

-- | Will only return subscription filters that match the provided
-- filterNamePrefix. If you don\'t specify a value, no prefix filter is
-- applied.
dsfrqFilterNamePrefix :: Lens' DescribeSubscriptionFilters (Maybe Text)
dsfrqFilterNamePrefix = lens _dsfrqFilterNamePrefix (\ s a -> s{_dsfrqFilterNamePrefix = a});

-- | FIXME: Undocumented member.
dsfrqNextToken :: Lens' DescribeSubscriptionFilters (Maybe Text)
dsfrqNextToken = lens _dsfrqNextToken (\ s a -> s{_dsfrqNextToken = a});

-- | FIXME: Undocumented member.
dsfrqLimit :: Lens' DescribeSubscriptionFilters (Maybe Natural)
dsfrqLimit = lens _dsfrqLimit (\ s a -> s{_dsfrqLimit = a}) . mapping _Nat;

-- | The log group name for which subscription filters are to be listed.
dsfrqLogGroupName :: Lens' DescribeSubscriptionFilters Text
dsfrqLogGroupName = lens _dsfrqLogGroupName (\ s a -> s{_dsfrqLogGroupName = a});

instance AWSRequest DescribeSubscriptionFilters where
        type Sv DescribeSubscriptionFilters = CloudWatchLogs
        type Rs DescribeSubscriptionFilters =
             DescribeSubscriptionFiltersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSubscriptionFiltersResponse' <$>
                   (x .?> "subscriptionFilters" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeSubscriptionFilters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeSubscriptionFilters" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSubscriptionFilters where
        toJSON DescribeSubscriptionFilters'{..}
          = object
              ["filterNamePrefix" .= _dsfrqFilterNamePrefix,
               "nextToken" .= _dsfrqNextToken,
               "limit" .= _dsfrqLimit,
               "logGroupName" .= _dsfrqLogGroupName]

instance ToPath DescribeSubscriptionFilters where
        toPath = const "/"

instance ToQuery DescribeSubscriptionFilters where
        toQuery = const mempty

-- | /See:/ 'describeSubscriptionFiltersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrsSubscriptionFilters'
--
-- * 'dsfrsNextToken'
--
-- * 'dsfrsStatus'
data DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse'
    { _dsfrsSubscriptionFilters :: !(Maybe [SubscriptionFilter])
    , _dsfrsNextToken           :: !(Maybe Text)
    , _dsfrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSubscriptionFiltersResponse' smart constructor.
describeSubscriptionFiltersResponse :: Int -> DescribeSubscriptionFiltersResponse
describeSubscriptionFiltersResponse pStatus_ =
    DescribeSubscriptionFiltersResponse'
    { _dsfrsSubscriptionFilters = Nothing
    , _dsfrsNextToken = Nothing
    , _dsfrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dsfrsSubscriptionFilters :: Lens' DescribeSubscriptionFiltersResponse [SubscriptionFilter]
dsfrsSubscriptionFilters = lens _dsfrsSubscriptionFilters (\ s a -> s{_dsfrsSubscriptionFilters = a}) . _Default;

-- | FIXME: Undocumented member.
dsfrsNextToken :: Lens' DescribeSubscriptionFiltersResponse (Maybe Text)
dsfrsNextToken = lens _dsfrsNextToken (\ s a -> s{_dsfrsNextToken = a});

-- | FIXME: Undocumented member.
dsfrsStatus :: Lens' DescribeSubscriptionFiltersResponse Int
dsfrsStatus = lens _dsfrsStatus (\ s a -> s{_dsfrsStatus = a});
