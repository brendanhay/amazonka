{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the load balancers for the specified Auto Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLoadBalancers.html>
module Network.AWS.AutoScaling.DescribeLoadBalancers
    (
    -- * Request
      DescribeLoadBalancers
    -- ** Request constructor
    , describeLoadBalancers
    -- ** Request lenses
    , dlbrqNextToken
    , dlbrqMaxRecords
    , dlbrqAutoScalingGroupName

    -- * Response
    , DescribeLoadBalancersResponse
    -- ** Response constructor
    , describeLoadBalancersResponse
    -- ** Response lenses
    , dlbrsLoadBalancers
    , dlbrsNextToken
    , dlbrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbrqNextToken'
--
-- * 'dlbrqMaxRecords'
--
-- * 'dlbrqAutoScalingGroupName'
data DescribeLoadBalancers = DescribeLoadBalancers'
    { _dlbrqNextToken            :: !(Maybe Text)
    , _dlbrqMaxRecords           :: !(Maybe Int)
    , _dlbrqAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancers' smart constructor.
describeLoadBalancers :: Text -> DescribeLoadBalancers
describeLoadBalancers pAutoScalingGroupName_ =
    DescribeLoadBalancers'
    { _dlbrqNextToken = Nothing
    , _dlbrqMaxRecords = Nothing
    , _dlbrqAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dlbrqNextToken :: Lens' DescribeLoadBalancers (Maybe Text)
dlbrqNextToken = lens _dlbrqNextToken (\ s a -> s{_dlbrqNextToken = a});

-- | The maximum number of items to return with this call.
dlbrqMaxRecords :: Lens' DescribeLoadBalancers (Maybe Int)
dlbrqMaxRecords = lens _dlbrqMaxRecords (\ s a -> s{_dlbrqMaxRecords = a});

-- | The name of the group.
dlbrqAutoScalingGroupName :: Lens' DescribeLoadBalancers Text
dlbrqAutoScalingGroupName = lens _dlbrqAutoScalingGroupName (\ s a -> s{_dlbrqAutoScalingGroupName = a});

instance AWSRequest DescribeLoadBalancers where
        type Sv DescribeLoadBalancers = AutoScaling
        type Rs DescribeLoadBalancers =
             DescribeLoadBalancersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeLoadBalancersResult"
              (\ s h x ->
                 DescribeLoadBalancersResponse' <$>
                   (x .@? "LoadBalancers" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeLoadBalancers where
        toHeaders = const mempty

instance ToPath DescribeLoadBalancers where
        toPath = const "/"

instance ToQuery DescribeLoadBalancers where
        toQuery DescribeLoadBalancers'{..}
          = mconcat
              ["Action" =: ("DescribeLoadBalancers" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "NextToken" =: _dlbrqNextToken,
               "MaxRecords" =: _dlbrqMaxRecords,
               "AutoScalingGroupName" =: _dlbrqAutoScalingGroupName]

-- | /See:/ 'describeLoadBalancersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbrsLoadBalancers'
--
-- * 'dlbrsNextToken'
--
-- * 'dlbrsStatus'
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
    { _dlbrsLoadBalancers :: !(Maybe [LoadBalancerState])
    , _dlbrsNextToken     :: !(Maybe Text)
    , _dlbrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancersResponse' smart constructor.
describeLoadBalancersResponse :: Int -> DescribeLoadBalancersResponse
describeLoadBalancersResponse pStatus_ =
    DescribeLoadBalancersResponse'
    { _dlbrsLoadBalancers = Nothing
    , _dlbrsNextToken = Nothing
    , _dlbrsStatus = pStatus_
    }

-- | The load balancers.
dlbrsLoadBalancers :: Lens' DescribeLoadBalancersResponse [LoadBalancerState]
dlbrsLoadBalancers = lens _dlbrsLoadBalancers (\ s a -> s{_dlbrsLoadBalancers = a}) . _Default;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dlbrsNextToken :: Lens' DescribeLoadBalancersResponse (Maybe Text)
dlbrsNextToken = lens _dlbrsNextToken (\ s a -> s{_dlbrsNextToken = a});

-- | FIXME: Undocumented member.
dlbrsStatus :: Lens' DescribeLoadBalancersResponse Int
dlbrsStatus = lens _dlbrsStatus (\ s a -> s{_dlbrsStatus = a});
