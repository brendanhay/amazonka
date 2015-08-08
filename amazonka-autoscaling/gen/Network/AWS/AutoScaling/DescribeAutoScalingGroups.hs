{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Auto Scaling groups. If a list of names is not
-- provided, the call describes all Auto Scaling groups.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingGroups.html AWS API Reference> for DescribeAutoScalingGroups.
module Network.AWS.AutoScaling.DescribeAutoScalingGroups
    (
    -- * Creating a Request
      DescribeAutoScalingGroups
    , describeAutoScalingGroups
    -- * Request Lenses
    , dasgAutoScalingGroupNames
    , dasgNextToken
    , dasgMaxRecords

    -- * Destructuring the Response
    , DescribeAutoScalingGroupsResponse
    , describeAutoScalingGroupsResponse
    -- * Response Lenses
    , dasgrsNextToken
    , dasgrsStatus
    , dasgrsAutoScalingGroups
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAutoScalingGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasgAutoScalingGroupNames'
--
-- * 'dasgNextToken'
--
-- * 'dasgMaxRecords'
data DescribeAutoScalingGroups = DescribeAutoScalingGroups'
    { _dasgAutoScalingGroupNames :: !(Maybe [Text])
    , _dasgNextToken             :: !(Maybe Text)
    , _dasgMaxRecords            :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAutoScalingGroups' smart constructor.
describeAutoScalingGroups :: DescribeAutoScalingGroups
describeAutoScalingGroups =
    DescribeAutoScalingGroups'
    { _dasgAutoScalingGroupNames = Nothing
    , _dasgNextToken = Nothing
    , _dasgMaxRecords = Nothing
    }

-- | The group names.
dasgAutoScalingGroupNames :: Lens' DescribeAutoScalingGroups [Text]
dasgAutoScalingGroupNames = lens _dasgAutoScalingGroupNames (\ s a -> s{_dasgAutoScalingGroupNames = a}) . _Default . _Coerce;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dasgNextToken :: Lens' DescribeAutoScalingGroups (Maybe Text)
dasgNextToken = lens _dasgNextToken (\ s a -> s{_dasgNextToken = a});

-- | The maximum number of items to return with this call.
dasgMaxRecords :: Lens' DescribeAutoScalingGroups (Maybe Int)
dasgMaxRecords = lens _dasgMaxRecords (\ s a -> s{_dasgMaxRecords = a});

instance AWSPager DescribeAutoScalingGroups where
        page rq rs
          | stop (rs ^. dasgrsNextToken) = Nothing
          | stop (rs ^. dasgrsAutoScalingGroups) = Nothing
          | otherwise =
            Just $ rq & dasgNextToken .~ rs ^. dasgrsNextToken

instance AWSRequest DescribeAutoScalingGroups where
        type Sv DescribeAutoScalingGroups = AutoScaling
        type Rs DescribeAutoScalingGroups =
             DescribeAutoScalingGroupsResponse
        request = postQuery
        response
          = receiveXMLWrapper "DescribeAutoScalingGroupsResult"
              (\ s h x ->
                 DescribeAutoScalingGroupsResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "AutoScalingGroups" .!@ mempty >>=
                        parseXMLList "member"))

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
-- * 'dasgrsNextToken'
--
-- * 'dasgrsStatus'
--
-- * 'dasgrsAutoScalingGroups'
data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse'
    { _dasgrsNextToken         :: !(Maybe Text)
    , _dasgrsStatus            :: !Int
    , _dasgrsAutoScalingGroups :: ![AutoScalingGroup]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAutoScalingGroupsResponse' smart constructor.
describeAutoScalingGroupsResponse :: Int -> DescribeAutoScalingGroupsResponse
describeAutoScalingGroupsResponse pStatus_ =
    DescribeAutoScalingGroupsResponse'
    { _dasgrsNextToken = Nothing
    , _dasgrsStatus = pStatus_
    , _dasgrsAutoScalingGroups = mempty
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dasgrsNextToken :: Lens' DescribeAutoScalingGroupsResponse (Maybe Text)
dasgrsNextToken = lens _dasgrsNextToken (\ s a -> s{_dasgrsNextToken = a});

-- | Undocumented member.
dasgrsStatus :: Lens' DescribeAutoScalingGroupsResponse Int
dasgrsStatus = lens _dasgrsStatus (\ s a -> s{_dasgrsStatus = a});

-- | The groups.
dasgrsAutoScalingGroups :: Lens' DescribeAutoScalingGroupsResponse [AutoScalingGroup]
dasgrsAutoScalingGroups = lens _dasgrsAutoScalingGroups (\ s a -> s{_dasgrsAutoScalingGroups = a}) . _Coerce;
