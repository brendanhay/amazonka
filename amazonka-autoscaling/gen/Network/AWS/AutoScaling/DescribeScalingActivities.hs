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
    , desNextToken
    , desMaxRecords
    , desAutoScalingGroupName
    , desActivityIds

    -- * Response
    , DescribeScalingActivitiesResponse
    -- ** Response constructor
    , describeScalingActivitiesResponse
    -- ** Response lenses
    , dsasrsNextToken
    , dsasrsStatus
    , dsasrsActivities
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
-- * 'desNextToken'
--
-- * 'desMaxRecords'
--
-- * 'desAutoScalingGroupName'
--
-- * 'desActivityIds'
data DescribeScalingActivities = DescribeScalingActivities'
    { _desNextToken            :: !(Maybe Text)
    , _desMaxRecords           :: !(Maybe Int)
    , _desAutoScalingGroupName :: !(Maybe Text)
    , _desActivityIds          :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingActivities' smart constructor.
describeScalingActivities :: DescribeScalingActivities
describeScalingActivities =
    DescribeScalingActivities'
    { _desNextToken = Nothing
    , _desMaxRecords = Nothing
    , _desAutoScalingGroupName = Nothing
    , _desActivityIds = Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
desNextToken :: Lens' DescribeScalingActivities (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

-- | The maximum number of items to return with this call.
desMaxRecords :: Lens' DescribeScalingActivities (Maybe Int)
desMaxRecords = lens _desMaxRecords (\ s a -> s{_desMaxRecords = a});

-- | The name of the group.
desAutoScalingGroupName :: Lens' DescribeScalingActivities (Maybe Text)
desAutoScalingGroupName = lens _desAutoScalingGroupName (\ s a -> s{_desAutoScalingGroupName = a});

-- | The activity IDs of the desired scaling activities. If this list is
-- omitted, all activities are described. If the @AutoScalingGroupName@
-- parameter is provided, the results are limited to that group. The list
-- of requested activities cannot contain more than 50 items. If unknown
-- activities are requested, they are ignored with no error.
desActivityIds :: Lens' DescribeScalingActivities [Text]
desActivityIds = lens _desActivityIds (\ s a -> s{_desActivityIds = a}) . _Default . _Coerce;

instance AWSPager DescribeScalingActivities where
        page rq rs
          | stop (rs ^. dsasrsNextToken) = Nothing
          | stop (rs ^. dsasrsActivities) = Nothing
          | otherwise =
            Just $ rq & desNextToken .~ rs ^. dsasrsNextToken

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
               "NextToken" =: _desNextToken,
               "MaxRecords" =: _desMaxRecords,
               "AutoScalingGroupName" =: _desAutoScalingGroupName,
               "ActivityIds" =:
                 toQuery (toQueryList "member" <$> _desActivityIds)]

-- | /See:/ 'describeScalingActivitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsasrsNextToken'
--
-- * 'dsasrsStatus'
--
-- * 'dsasrsActivities'
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
    { _dsasrsNextToken  :: !(Maybe Text)
    , _dsasrsStatus     :: !Int
    , _dsasrsActivities :: ![Activity]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingActivitiesResponse' smart constructor.
describeScalingActivitiesResponse :: Int -> DescribeScalingActivitiesResponse
describeScalingActivitiesResponse pStatus_ =
    DescribeScalingActivitiesResponse'
    { _dsasrsNextToken = Nothing
    , _dsasrsStatus = pStatus_
    , _dsasrsActivities = mempty
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dsasrsNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dsasrsNextToken = lens _dsasrsNextToken (\ s a -> s{_dsasrsNextToken = a});

-- | FIXME: Undocumented member.
dsasrsStatus :: Lens' DescribeScalingActivitiesResponse Int
dsasrsStatus = lens _dsasrsStatus (\ s a -> s{_dsasrsStatus = a});

-- | The scaling activities.
dsasrsActivities :: Lens' DescribeScalingActivitiesResponse [Activity]
dsasrsActivities = lens _dsasrsActivities (\ s a -> s{_dsasrsActivities = a}) . _Coerce;
