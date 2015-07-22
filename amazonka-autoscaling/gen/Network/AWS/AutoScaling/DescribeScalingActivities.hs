{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more scaling activities for the specified Auto Scaling
-- group. If you omit the @ActivityIds@, the call returns all activities
-- from the past six weeks. Activities are sorted by the start time.
-- Activities still in progress appear first on the list.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingActivities.html>
module Network.AWS.AutoScaling.DescribeScalingActivities
    (
    -- * Request
      DescribeScalingActivities
    -- ** Request constructor
    , describeScalingActivities
    -- ** Request lenses
    , dsasNextToken
    , dsasMaxRecords
    , dsasAutoScalingGroupName
    , dsasActivityIds

    -- * Response
    , DescribeScalingActivitiesResponse
    -- ** Response constructor
    , describeScalingActivitiesResponse
    -- ** Response lenses
    , dNextToken
    , dStatus
    , dActivities
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScalingActivities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsasNextToken'
--
-- * 'dsasMaxRecords'
--
-- * 'dsasAutoScalingGroupName'
--
-- * 'dsasActivityIds'
data DescribeScalingActivities = DescribeScalingActivities'
    { _dsasNextToken            :: !(Maybe Text)
    , _dsasMaxRecords           :: !(Maybe Int)
    , _dsasAutoScalingGroupName :: !(Maybe Text)
    , _dsasActivityIds          :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingActivities' smart constructor.
describeScalingActivities :: DescribeScalingActivities
describeScalingActivities =
    DescribeScalingActivities'
    { _dsasNextToken = Nothing
    , _dsasMaxRecords = Nothing
    , _dsasAutoScalingGroupName = Nothing
    , _dsasActivityIds = Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dsasNextToken :: Lens' DescribeScalingActivities (Maybe Text)
dsasNextToken = lens _dsasNextToken (\ s a -> s{_dsasNextToken = a});

-- | The maximum number of items to return with this call.
dsasMaxRecords :: Lens' DescribeScalingActivities (Maybe Int)
dsasMaxRecords = lens _dsasMaxRecords (\ s a -> s{_dsasMaxRecords = a});

-- | The name of the group.
dsasAutoScalingGroupName :: Lens' DescribeScalingActivities (Maybe Text)
dsasAutoScalingGroupName = lens _dsasAutoScalingGroupName (\ s a -> s{_dsasAutoScalingGroupName = a});

-- | The activity IDs of the desired scaling activities. If this list is
-- omitted, all activities are described. If the @AutoScalingGroupName@
-- parameter is provided, the results are limited to that group. The list
-- of requested activities cannot contain more than 50 items. If unknown
-- activities are requested, they are ignored with no error.
dsasActivityIds :: Lens' DescribeScalingActivities [Text]
dsasActivityIds = lens _dsasActivityIds (\ s a -> s{_dsasActivityIds = a}) . _Default;

instance AWSPager DescribeScalingActivities where
        page rq rs
          | stop (rs ^. dNextToken) = Nothing
          | stop (rs ^. dActivities) = Nothing
          | otherwise =
            Just $ rq & dsasNextToken .~ rs ^. dNextToken

instance AWSRequest DescribeScalingActivities where
        type Sv DescribeScalingActivities = AutoScaling
        type Rs DescribeScalingActivities =
             DescribeScalingActivitiesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeScalingActivitiesResult"
              (\ s h x ->
                 DescribeScalingActivitiesResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "Activities" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders DescribeScalingActivities where
        toHeaders = const mempty

instance ToPath DescribeScalingActivities where
        toPath = const "/"

instance ToQuery DescribeScalingActivities where
        toQuery DescribeScalingActivities'{..}
          = mconcat
              ["Action" =:
                 ("DescribeScalingActivities" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "NextToken" =: _dsasNextToken,
               "MaxRecords" =: _dsasMaxRecords,
               "AutoScalingGroupName" =: _dsasAutoScalingGroupName,
               "ActivityIds" =:
                 toQuery (toQueryList "member" <$> _dsasActivityIds)]

-- | /See:/ 'describeScalingActivitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dNextToken'
--
-- * 'dStatus'
--
-- * 'dActivities'
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
    { _dNextToken  :: !(Maybe Text)
    , _dStatus     :: !Int
    , _dActivities :: ![Activity]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingActivitiesResponse' smart constructor.
describeScalingActivitiesResponse :: Int -> DescribeScalingActivitiesResponse
describeScalingActivitiesResponse pStatus =
    DescribeScalingActivitiesResponse'
    { _dNextToken = Nothing
    , _dStatus = pStatus
    , _dActivities = mempty
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | FIXME: Undocumented member.
dStatus :: Lens' DescribeScalingActivitiesResponse Int
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The scaling activities.
dActivities :: Lens' DescribeScalingActivitiesResponse [Activity]
dActivities = lens _dActivities (\ s a -> s{_dActivities = a});
