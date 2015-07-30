{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Auto Scaling instances. If a list is not provided,
-- the call describes all instances.
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
    , dasirsNextToken
    , dasirsAutoScalingInstances
    , dasirsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAutoScalingInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasiNextToken'
--
-- * 'dasiInstanceIds'
--
-- * 'dasiMaxRecords'
data DescribeAutoScalingInstances = DescribeAutoScalingInstances'
    { _dasiNextToken   :: !(Maybe Text)
    , _dasiInstanceIds :: !(Maybe [Text])
    , _dasiMaxRecords  :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAutoScalingInstances' smart constructor.
describeAutoScalingInstances :: DescribeAutoScalingInstances
describeAutoScalingInstances =
    DescribeAutoScalingInstances'
    { _dasiNextToken = Nothing
    , _dasiInstanceIds = Nothing
    , _dasiMaxRecords = Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dasiNextToken :: Lens' DescribeAutoScalingInstances (Maybe Text)
dasiNextToken = lens _dasiNextToken (\ s a -> s{_dasiNextToken = a});

-- | One or more Auto Scaling instances to describe, up to 50 instances. If
-- you omit this parameter, all Auto Scaling instances are described. If
-- you specify an ID that does not exist, it is ignored with no error.
dasiInstanceIds :: Lens' DescribeAutoScalingInstances [Text]
dasiInstanceIds = lens _dasiInstanceIds (\ s a -> s{_dasiInstanceIds = a}) . _Default . _Coerce;

-- | The maximum number of items to return with this call.
dasiMaxRecords :: Lens' DescribeAutoScalingInstances (Maybe Int)
dasiMaxRecords = lens _dasiMaxRecords (\ s a -> s{_dasiMaxRecords = a});

instance AWSPager DescribeAutoScalingInstances where
        page rq rs
          | stop (rs ^. dasirsNextToken) = Nothing
          | stop (rs ^. dasirsAutoScalingInstances) = Nothing
          | otherwise =
            Just $ rq & dasiNextToken .~ rs ^. dasirsNextToken

instance AWSRequest DescribeAutoScalingInstances
         where
        type Sv DescribeAutoScalingInstances = AutoScaling
        type Rs DescribeAutoScalingInstances =
             DescribeAutoScalingInstancesResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeAutoScalingInstancesResult"
              (\ s h x ->
                 DescribeAutoScalingInstancesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "AutoScalingInstances" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAutoScalingInstances where
        toHeaders = const mempty

instance ToPath DescribeAutoScalingInstances where
        toPath = const mempty

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
-- * 'dasirsNextToken'
--
-- * 'dasirsAutoScalingInstances'
--
-- * 'dasirsStatus'
data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse'
    { _dasirsNextToken            :: !(Maybe Text)
    , _dasirsAutoScalingInstances :: !(Maybe [AutoScalingInstanceDetails])
    , _dasirsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAutoScalingInstancesResponse' smart constructor.
describeAutoScalingInstancesResponse :: Int -> DescribeAutoScalingInstancesResponse
describeAutoScalingInstancesResponse pStatus_ =
    DescribeAutoScalingInstancesResponse'
    { _dasirsNextToken = Nothing
    , _dasirsAutoScalingInstances = Nothing
    , _dasirsStatus = pStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dasirsNextToken :: Lens' DescribeAutoScalingInstancesResponse (Maybe Text)
dasirsNextToken = lens _dasirsNextToken (\ s a -> s{_dasirsNextToken = a});

-- | The instances.
dasirsAutoScalingInstances :: Lens' DescribeAutoScalingInstancesResponse [AutoScalingInstanceDetails]
dasirsAutoScalingInstances = lens _dasirsAutoScalingInstances (\ s a -> s{_dasirsAutoScalingInstances = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
dasirsStatus :: Lens' DescribeAutoScalingInstancesResponse Int
dasirsStatus = lens _dasirsStatus (\ s a -> s{_dasirsStatus = a});
