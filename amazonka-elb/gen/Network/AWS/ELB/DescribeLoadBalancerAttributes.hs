{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.DescribeLoadBalancerAttributes
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

-- | Describes the attributes for the specified load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerAttributes.html>
module Network.AWS.ELB.DescribeLoadBalancerAttributes
    (
    -- * Request
      DescribeLoadBalancerAttributes
    -- ** Request constructor
    , describeLoadBalancerAttributes
    -- ** Request lenses
    , dlbaLoadBalancerName

    -- * Response
    , DescribeLoadBalancerAttributesResponse
    -- ** Response constructor
    , describeLoadBalancerAttributesResponse
    -- ** Response lenses
    , dlbarLoadBalancerAttributes
    , dlbarStatusCode
    ) where

import Network.AWS.ELB.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLoadBalancerAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbaLoadBalancerName'
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'{_dlbaLoadBalancerName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeLoadBalancerAttributes' smart constructor.
describeLoadBalancerAttributes :: Text -> DescribeLoadBalancerAttributes
describeLoadBalancerAttributes pLoadBalancerName = DescribeLoadBalancerAttributes'{_dlbaLoadBalancerName = pLoadBalancerName};

-- | The name of the load balancer.
dlbaLoadBalancerName :: Lens' DescribeLoadBalancerAttributes Text
dlbaLoadBalancerName = lens _dlbaLoadBalancerName (\ s a -> s{_dlbaLoadBalancerName = a});

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
               "LoadBalancerName" =: _dlbaLoadBalancerName]

-- | /See:/ 'describeLoadBalancerAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbarLoadBalancerAttributes'
--
-- * 'dlbarStatusCode'
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'{_dlbarLoadBalancerAttributes :: Maybe LoadBalancerAttributes, _dlbarStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeLoadBalancerAttributesResponse' smart constructor.
describeLoadBalancerAttributesResponse :: Int -> DescribeLoadBalancerAttributesResponse
describeLoadBalancerAttributesResponse pStatusCode = DescribeLoadBalancerAttributesResponse'{_dlbarLoadBalancerAttributes = Nothing, _dlbarStatusCode = pStatusCode};

-- | Information about the load balancer attributes.
dlbarLoadBalancerAttributes :: Lens' DescribeLoadBalancerAttributesResponse (Maybe LoadBalancerAttributes)
dlbarLoadBalancerAttributes = lens _dlbarLoadBalancerAttributes (\ s a -> s{_dlbarLoadBalancerAttributes = a});

-- | FIXME: Undocumented member.
dlbarStatusCode :: Lens' DescribeLoadBalancerAttributesResponse Int
dlbarStatusCode = lens _dlbarStatusCode (\ s a -> s{_dlbarStatusCode = a});
