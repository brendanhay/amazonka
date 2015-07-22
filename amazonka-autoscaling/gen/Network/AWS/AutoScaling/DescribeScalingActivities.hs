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
    , desrqNextToken
    , desrqMaxRecords
    , desrqAutoScalingGroupName
    , desrqActivityIds

    -- * Response
    , DescribeScalingActivitiesResponse
    -- ** Response constructor
    , describeScalingActivitiesResponse
    -- ** Response lenses
    , desrsNextToken
    , desrsStatus
    , desrsActivities
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
-- * 'desrqNextToken'
--
-- * 'desrqMaxRecords'
--
-- * 'desrqAutoScalingGroupName'
--
-- * 'desrqActivityIds'
data DescribeScalingActivities = DescribeScalingActivities'
    { _desrqNextToken            :: !(Maybe Text)
    , _desrqMaxRecords           :: !(Maybe Int)
    , _desrqAutoScalingGroupName :: !(Maybe Text)
    , _desrqActivityIds          :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingActivities' smart constructor.
describeScalingActivities :: DescribeScalingActivities
describeScalingActivities =
    DescribeScalingActivities'
    { _desrqNextToken = Nothing
    , _desrqMaxRecords = Nothing
    , _desrqAutoScalingGroupName = Nothing
    , _desrqActivityIds = Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
desrqNextToken :: Lens' DescribeScalingActivities (Maybe Text)
desrqNextToken = lens _desrqNextToken (\ s a -> s{_desrqNextToken = a});

-- | The maximum number of items to return with this call.
desrqMaxRecords :: Lens' DescribeScalingActivities (Maybe Int)
desrqMaxRecords = lens _desrqMaxRecords (\ s a -> s{_desrqMaxRecords = a});

-- | The name of the group.
desrqAutoScalingGroupName :: Lens' DescribeScalingActivities (Maybe Text)
desrqAutoScalingGroupName = lens _desrqAutoScalingGroupName (\ s a -> s{_desrqAutoScalingGroupName = a});

-- | The activity IDs of the desired scaling activities. If this list is
-- omitted, all activities are described. If the @AutoScalingGroupName@
-- parameter is provided, the results are limited to that group. The list
-- of requested activities cannot contain more than 50 items. If unknown
-- activities are requested, they are ignored with no error.
desrqActivityIds :: Lens' DescribeScalingActivities [Text]
desrqActivityIds = lens _desrqActivityIds (\ s a -> s{_desrqActivityIds = a}) . _Default;

instance AWSPager DescribeScalingActivities where
        page rq rs
          | stop (rs ^. desrsNextToken) = Nothing
          | stop (rs ^. desrsActivities) = Nothing
          | otherwise =
            Just $ rq & desrqNextToken .~ rs ^. desrsNextToken

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
               "NextToken" =: _desrqNextToken,
               "MaxRecords" =: _desrqMaxRecords,
               "AutoScalingGroupName" =: _desrqAutoScalingGroupName,
               "ActivityIds" =:
                 toQuery (toQueryList "member" <$> _desrqActivityIds)]

-- | /See:/ 'describeScalingActivitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrsNextToken'
--
-- * 'desrsStatus'
--
-- * 'desrsActivities'
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
    { _desrsNextToken  :: !(Maybe Text)
    , _desrsStatus     :: !Int
    , _desrsActivities :: ![Activity]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingActivitiesResponse' smart constructor.
describeScalingActivitiesResponse :: Int -> DescribeScalingActivitiesResponse
describeScalingActivitiesResponse pStatus =
    DescribeScalingActivitiesResponse'
    { _desrsNextToken = Nothing
    , _desrsStatus = pStatus
    , _desrsActivities = mempty
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
desrsNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
desrsNextToken = lens _desrsNextToken (\ s a -> s{_desrsNextToken = a});

-- | FIXME: Undocumented member.
desrsStatus :: Lens' DescribeScalingActivitiesResponse Int
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});

-- | The scaling activities.
desrsActivities :: Lens' DescribeScalingActivitiesResponse [Activity]
desrsActivities = lens _desrsActivities (\ s a -> s{_desrsActivities = a});
