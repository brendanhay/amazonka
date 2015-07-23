{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancerAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerAttributes.html>
module Network.AWS.ELB.DescribeLoadBalancerAttributes
    (
    -- * Request
      DescribeLoadBalancerAttributes
    -- ** Request constructor
    , describeLoadBalancerAttributes
    -- ** Request lenses
    , dlbarqLoadBalancerName

    -- * Response
    , DescribeLoadBalancerAttributesResponse
    -- ** Response constructor
    , describeLoadBalancerAttributesResponse
    -- ** Response lenses
    , dlbarsLoadBalancerAttributes
    , dlbarsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancerAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbarqLoadBalancerName'
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
    { _dlbarqLoadBalancerName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancerAttributes' smart constructor.
describeLoadBalancerAttributes :: Text -> DescribeLoadBalancerAttributes
describeLoadBalancerAttributes pLoadBalancerName_ =
    DescribeLoadBalancerAttributes'
    { _dlbarqLoadBalancerName = pLoadBalancerName_
    }

-- | The name of the load balancer.
dlbarqLoadBalancerName :: Lens' DescribeLoadBalancerAttributes Text
dlbarqLoadBalancerName = lens _dlbarqLoadBalancerName (\ s a -> s{_dlbarqLoadBalancerName = a});

instance AWSRequest DescribeLoadBalancerAttributes
         where
        type Sv DescribeLoadBalancerAttributes = ELB
        type Rs DescribeLoadBalancerAttributes =
             DescribeLoadBalancerAttributesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeLoadBalancerAttributesResult"
              (\ s h x ->
                 DescribeLoadBalancerAttributesResponse' <$>
                   (x .@? "LoadBalancerAttributes") <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeLoadBalancerAttributes
         where
        toHeaders = const mempty

instance ToPath DescribeLoadBalancerAttributes where
        toPath = const "/"

instance ToQuery DescribeLoadBalancerAttributes where
        toQuery DescribeLoadBalancerAttributes'{..}
          = mconcat
              ["Action" =:
                 ("DescribeLoadBalancerAttributes" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerName" =: _dlbarqLoadBalancerName]

-- | /See:/ 'describeLoadBalancerAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbarsLoadBalancerAttributes'
--
-- * 'dlbarsStatus'
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
    { _dlbarsLoadBalancerAttributes :: !(Maybe LoadBalancerAttributes)
    , _dlbarsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBalancerAttributesResponse' smart constructor.
describeLoadBalancerAttributesResponse :: Int -> DescribeLoadBalancerAttributesResponse
describeLoadBalancerAttributesResponse pStatus_ =
    DescribeLoadBalancerAttributesResponse'
    { _dlbarsLoadBalancerAttributes = Nothing
    , _dlbarsStatus = pStatus_
    }

-- | Information about the load balancer attributes.
dlbarsLoadBalancerAttributes :: Lens' DescribeLoadBalancerAttributesResponse (Maybe LoadBalancerAttributes)
dlbarsLoadBalancerAttributes = lens _dlbarsLoadBalancerAttributes (\ s a -> s{_dlbarsLoadBalancerAttributes = a});

-- | FIXME: Undocumented member.
dlbarsStatus :: Lens' DescribeLoadBalancerAttributesResponse Int
dlbarsStatus = lens _dlbarsStatus (\ s a -> s{_dlbarsStatus = a});
