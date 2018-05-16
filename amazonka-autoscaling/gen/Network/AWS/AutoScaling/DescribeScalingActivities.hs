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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more scaling activities for the specified Auto Scaling group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScalingActivities
    (
    -- * Creating a Request
      describeScalingActivities
    , DescribeScalingActivities
    -- * Request Lenses
    , desNextToken
    , desAutoScalingGroupName
    , desMaxRecords
    , desActivityIds

    -- * Destructuring the Response
    , describeScalingActivitiesResponse
    , DescribeScalingActivitiesResponse
    -- * Response Lenses
    , dsasrsNextToken
    , dsasrsResponseStatus
    , dsasrsActivities
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
  { _desNextToken            :: !(Maybe Text)
  , _desAutoScalingGroupName :: !(Maybe Text)
  , _desMaxRecords           :: !(Maybe Int)
  , _desActivityIds          :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingActivities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'desAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'desMaxRecords' - The maximum number of items to return with this call. The default value is 100 and the maximum value is 100.
--
-- * 'desActivityIds' - The activity IDs of the desired scaling activities. If you omit this parameter, all activities for the past six weeks are described. If you specify an Auto Scaling group, the results are limited to that group. The list of requested activities cannot contain more than 50 items. If unknown activities are requested, they are ignored with no error.
describeScalingActivities
    :: DescribeScalingActivities
describeScalingActivities =
  DescribeScalingActivities'
    { _desNextToken = Nothing
    , _desAutoScalingGroupName = Nothing
    , _desMaxRecords = Nothing
    , _desActivityIds = Nothing
    }


-- | The token for the next set of items to return. (You received this token from a previous call.)
desNextToken :: Lens' DescribeScalingActivities (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a})

-- | The name of the Auto Scaling group.
desAutoScalingGroupName :: Lens' DescribeScalingActivities (Maybe Text)
desAutoScalingGroupName = lens _desAutoScalingGroupName (\ s a -> s{_desAutoScalingGroupName = a})

-- | The maximum number of items to return with this call. The default value is 100 and the maximum value is 100.
desMaxRecords :: Lens' DescribeScalingActivities (Maybe Int)
desMaxRecords = lens _desMaxRecords (\ s a -> s{_desMaxRecords = a})

-- | The activity IDs of the desired scaling activities. If you omit this parameter, all activities for the past six weeks are described. If you specify an Auto Scaling group, the results are limited to that group. The list of requested activities cannot contain more than 50 items. If unknown activities are requested, they are ignored with no error.
desActivityIds :: Lens' DescribeScalingActivities [Text]
desActivityIds = lens _desActivityIds (\ s a -> s{_desActivityIds = a}) . _Default . _Coerce

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

instance Hashable DescribeScalingActivities where

instance NFData DescribeScalingActivities where

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
               "AutoScalingGroupName" =: _desAutoScalingGroupName,
               "MaxRecords" =: _desMaxRecords,
               "ActivityIds" =:
                 toQuery (toQueryList "member" <$> _desActivityIds)]

-- | /See:/ 'describeScalingActivitiesResponse' smart constructor.
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
  { _dsasrsNextToken      :: !(Maybe Text)
  , _dsasrsResponseStatus :: !Int
  , _dsasrsActivities     :: ![Activity]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScalingActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dsasrsResponseStatus' - -- | The response status code.
--
-- * 'dsasrsActivities' - The scaling activities. Activities are sorted by start time. Activities still in progress are described first.
describeScalingActivitiesResponse
    :: Int -- ^ 'dsasrsResponseStatus'
    -> DescribeScalingActivitiesResponse
describeScalingActivitiesResponse pResponseStatus_ =
  DescribeScalingActivitiesResponse'
    { _dsasrsNextToken = Nothing
    , _dsasrsResponseStatus = pResponseStatus_
    , _dsasrsActivities = mempty
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dsasrsNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dsasrsNextToken = lens _dsasrsNextToken (\ s a -> s{_dsasrsNextToken = a})

-- | -- | The response status code.
dsasrsResponseStatus :: Lens' DescribeScalingActivitiesResponse Int
dsasrsResponseStatus = lens _dsasrsResponseStatus (\ s a -> s{_dsasrsResponseStatus = a})

-- | The scaling activities. Activities are sorted by start time. Activities still in progress are described first.
dsasrsActivities :: Lens' DescribeScalingActivitiesResponse [Activity]
dsasrsActivities = lens _dsasrsActivities (\ s a -> s{_dsasrsActivities = a}) . _Coerce

instance NFData DescribeScalingActivitiesResponse
         where
