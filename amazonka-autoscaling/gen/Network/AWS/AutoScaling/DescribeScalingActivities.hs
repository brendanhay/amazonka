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
-- Module      : Network.AWS.AutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more scaling activities for the specified Auto Scaling
-- group. If you omit the 'ActivityIds', the call returns all activities
-- from the past six weeks. Activities are sorted by the start time.
-- Activities still in progress appear first on the list.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingActivities.html AWS API Reference> for DescribeScalingActivities.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScalingActivities
    (
    -- * Creating a Request
      describeScalingActivities
    , DescribeScalingActivities
    -- * Request Lenses
    , desNextToken
    , desMaxRecords
    , desAutoScalingGroupName
    , desActivityIds

    -- * Destructuring the Response
    , describeScalingActivitiesResponse
    , DescribeScalingActivitiesResponse
    -- * Response Lenses
    , dsasrsNextToken
    , dsasrsStatus
    , dsasrsActivities
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
    { _desNextToken            :: !(Maybe Text)
    , _desMaxRecords           :: !(Maybe Int)
    , _desAutoScalingGroupName :: !(Maybe Text)
    , _desActivityIds          :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingActivities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desNextToken'
--
-- * 'desMaxRecords'
--
-- * 'desAutoScalingGroupName'
--
-- * 'desActivityIds'
describeScalingActivities
    :: DescribeScalingActivities
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
-- omitted, all activities are described. If the 'AutoScalingGroupName'
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
        type Rs DescribeScalingActivities =
             DescribeScalingActivitiesResponse
        request = postQuery autoScaling
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
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
    { _dsasrsNextToken  :: !(Maybe Text)
    , _dsasrsStatus     :: !Int
    , _dsasrsActivities :: ![Activity]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasrsNextToken'
--
-- * 'dsasrsStatus'
--
-- * 'dsasrsActivities'
describeScalingActivitiesResponse
    :: Int -- ^ 'dsasrsStatus'
    -> DescribeScalingActivitiesResponse
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

-- | The response status code.
dsasrsStatus :: Lens' DescribeScalingActivitiesResponse Int
dsasrsStatus = lens _dsasrsStatus (\ s a -> s{_dsasrsStatus = a});

-- | The scaling activities.
dsasrsActivities :: Lens' DescribeScalingActivitiesResponse [Activity]
dsasrsActivities = lens _dsasrsActivities (\ s a -> s{_dsasrsActivities = a}) . _Coerce;
