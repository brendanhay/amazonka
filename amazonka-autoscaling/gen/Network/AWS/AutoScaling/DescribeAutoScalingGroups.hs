{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
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

-- | Describes one or more Auto Scaling groups. If a list of names is not
-- provided, the call describes all Auto Scaling groups.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingGroups.html>
module Network.AWS.AutoScaling.DescribeAutoScalingGroups
    (
    -- * Request
      DescribeAutoScalingGroups
    -- ** Request constructor
    , describeAutoScalingGroups
    -- ** Request lenses
    , dasgAutoScalingGroupNames
    , dasgNextToken
    , dasgMaxRecords

    -- * Response
    , DescribeAutoScalingGroupsResponse
    -- ** Response constructor
    , describeAutoScalingGroupsResponse
    -- ** Response lenses
    , dasgrNextToken
    , dasgrAutoScalingGroups
    , dasgrStatusCode
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAutoScalingGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasgAutoScalingGroupNames'
--
-- * 'dasgNextToken'
--
-- * 'dasgMaxRecords'
data DescribeAutoScalingGroups = DescribeAutoScalingGroups'{_dasgAutoScalingGroupNames :: Maybe [Text], _dasgNextToken :: Maybe Text, _dasgMaxRecords :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DescribeAutoScalingGroups' smart constructor.
describeAutoScalingGroups :: DescribeAutoScalingGroups
describeAutoScalingGroups = DescribeAutoScalingGroups'{_dasgAutoScalingGroupNames = Nothing, _dasgNextToken = Nothing, _dasgMaxRecords = Nothing};

-- | The group names.
dasgAutoScalingGroupNames :: Lens' DescribeAutoScalingGroups [Text]
dasgAutoScalingGroupNames = lens _dasgAutoScalingGroupNames (\ s a -> s{_dasgAutoScalingGroupNames = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dasgNextToken :: Lens' DescribeAutoScalingGroups (Maybe Text)
dasgNextToken = lens _dasgNextToken (\ s a -> s{_dasgNextToken = a});

-- | The maximum number of items to return with this call.
dasgMaxRecords :: Lens' DescribeAutoScalingGroups (Maybe Int)
dasgMaxRecords = lens _dasgMaxRecords (\ s a -> s{_dasgMaxRecords = a});

instance AWSPager DescribeAutoScalingGroups where
        page rq rs
          | stop (rs ^. dasgrNextToken) = Nothing
          | stop (rs ^. dasgrAutoScalingGroups) = Nothing
          | otherwise =
            Just $ rq & dasgNextToken .~ rs ^. dasgrNextToken

instance AWSRequest DescribeAutoScalingGroups where
        type Sv DescribeAutoScalingGroups = AutoScaling
        type Rs DescribeAutoScalingGroups =
             DescribeAutoScalingGroupsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAutoScalingGroupsResult"
              (\ s h x ->
                 DescribeAutoScalingGroupsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "AutoScalingGroups" .!@ mempty >>=
                        parseXMLList "member")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAutoScalingGroups where
        toHeaders = const mempty

instance ToPath DescribeAutoScalingGroups where
        toPath = const "/"

instance ToQuery DescribeAutoScalingGroups where
        toQuery DescribeAutoScalingGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAutoScalingGroups" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupNames" =:
                 toQuery
                   (toQueryList "member" <$>
                      _dasgAutoScalingGroupNames),
               "NextToken" =: _dasgNextToken,
               "MaxRecords" =: _dasgMaxRecords]

-- | /See:/ 'describeAutoScalingGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasgrNextToken'
--
-- * 'dasgrAutoScalingGroups'
--
-- * 'dasgrStatusCode'
data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse'{_dasgrNextToken :: Maybe Text, _dasgrAutoScalingGroups :: [AutoScalingGroup], _dasgrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeAutoScalingGroupsResponse' smart constructor.
describeAutoScalingGroupsResponse :: Int -> DescribeAutoScalingGroupsResponse
describeAutoScalingGroupsResponse pStatusCode = DescribeAutoScalingGroupsResponse'{_dasgrNextToken = Nothing, _dasgrAutoScalingGroups = mempty, _dasgrStatusCode = pStatusCode};

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dasgrNextToken :: Lens' DescribeAutoScalingGroupsResponse (Maybe Text)
dasgrNextToken = lens _dasgrNextToken (\ s a -> s{_dasgrNextToken = a});

-- | The groups.
dasgrAutoScalingGroups :: Lens' DescribeAutoScalingGroupsResponse [AutoScalingGroup]
dasgrAutoScalingGroups = lens _dasgrAutoScalingGroups (\ s a -> s{_dasgrAutoScalingGroups = a});

-- | FIXME: Undocumented member.
dasgrStatusCode :: Lens' DescribeAutoScalingGroupsResponse Int
dasgrStatusCode = lens _dasgrStatusCode (\ s a -> s{_dasgrStatusCode = a});
