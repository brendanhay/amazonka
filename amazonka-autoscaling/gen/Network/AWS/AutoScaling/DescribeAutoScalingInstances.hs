{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more Auto Scaling instances. If a list is not provided,
-- the call describes all instances.
--
-- You can describe up to a maximum of 50 instances with a single call. By
-- default, a call returns up to 20 instances. If there are more items to
-- return, the call returns a token. To get the next set of items, repeat
-- the call with the returned token in the @NextToken@ parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingInstances.html>
module Network.AWS.AutoScaling.DescribeAutoScalingInstances
    (
    -- * Request
      DescribeAutoScalingInstances
    -- ** Request constructor
    , describeAutoScalingInstances
    -- ** Request lenses
    , dasiNextToken
    , dasiInstanceIds
    , dasiMaxRecords

    -- * Response
    , DescribeAutoScalingInstancesResponse
    -- ** Response constructor
    , describeAutoScalingInstancesResponse
    -- ** Response lenses
    , dasirNextToken
    , dasirAutoScalingInstances
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'describeAutoScalingInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasiNextToken'
--
-- * 'dasiInstanceIds'
--
-- * 'dasiMaxRecords'
data DescribeAutoScalingInstances = DescribeAutoScalingInstances'{_dasiNextToken :: Maybe Text, _dasiInstanceIds :: Maybe [Text], _dasiMaxRecords :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DescribeAutoScalingInstances' smart constructor.
describeAutoScalingInstances :: DescribeAutoScalingInstances
describeAutoScalingInstances = DescribeAutoScalingInstances'{_dasiNextToken = Nothing, _dasiInstanceIds = Nothing, _dasiMaxRecords = Nothing};

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dasiNextToken :: Lens' DescribeAutoScalingInstances (Maybe Text)
dasiNextToken = lens _dasiNextToken (\ s a -> s{_dasiNextToken = a});

-- | One or more Auto Scaling instances to describe, up to 50 instances. If
-- you omit this parameter, all Auto Scaling instances are described. If
-- you specify an ID that does not exist, it is ignored with no error.
dasiInstanceIds :: Lens' DescribeAutoScalingInstances [Text]
dasiInstanceIds = lens _dasiInstanceIds (\ s a -> s{_dasiInstanceIds = a}) . _Default;

-- | The maximum number of items to return with this call.
dasiMaxRecords :: Lens' DescribeAutoScalingInstances (Maybe Int)
dasiMaxRecords = lens _dasiMaxRecords (\ s a -> s{_dasiMaxRecords = a});

instance AWSRequest DescribeAutoScalingInstances
         where
        type Sv DescribeAutoScalingInstances = AutoScaling
        type Rs DescribeAutoScalingInstances =
             DescribeAutoScalingInstancesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeAutoScalingInstancesResult"
              (\ s h x ->
                 DescribeAutoScalingInstancesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "AutoScalingInstances" .!@ mempty >>=
                        may (parseXMLList "member")))

instance ToHeaders DescribeAutoScalingInstances where
        toHeaders = const mempty

instance ToPath DescribeAutoScalingInstances where
        toPath = const "/"

instance ToQuery DescribeAutoScalingInstances where
        toQuery DescribeAutoScalingInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAutoScalingInstances" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "NextToken" =: _dasiNextToken,
               "InstanceIds" =:
                 toQuery (toQueryList "member" <$> _dasiInstanceIds),
               "MaxRecords" =: _dasiMaxRecords]

-- | /See:/ 'describeAutoScalingInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasirNextToken'
--
-- * 'dasirAutoScalingInstances'
data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse'{_dasirNextToken :: Maybe Text, _dasirAutoScalingInstances :: Maybe [AutoScalingInstanceDetails]} deriving (Eq, Read, Show)

-- | 'DescribeAutoScalingInstancesResponse' smart constructor.
describeAutoScalingInstancesResponse :: DescribeAutoScalingInstancesResponse
describeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse'{_dasirNextToken = Nothing, _dasirAutoScalingInstances = Nothing};

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dasirNextToken :: Lens' DescribeAutoScalingInstancesResponse (Maybe Text)
dasirNextToken = lens _dasirNextToken (\ s a -> s{_dasirNextToken = a});

-- | The instances.
dasirAutoScalingInstances :: Lens' DescribeAutoScalingInstancesResponse [AutoScalingInstanceDetails]
dasirAutoScalingInstances = lens _dasirAutoScalingInstances (\ s a -> s{_dasirAutoScalingInstances = a}) . _Default;
