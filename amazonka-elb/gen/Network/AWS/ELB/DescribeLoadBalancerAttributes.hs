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
-- Module      : Network.AWS.ELB.DescribeLoadBalancerAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified load balancer.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerAttributes.html AWS API Reference> for DescribeLoadBalancerAttributes.
module Network.AWS.ELB.DescribeLoadBalancerAttributes
    (
    -- * Creating a Request
      describeLoadBalancerAttributes
    , DescribeLoadBalancerAttributes
    -- * Request Lenses
    , dlbaLoadBalancerName

    -- * Destructuring the Response
    , describeLoadBalancerAttributesResponse
    , DescribeLoadBalancerAttributesResponse
    -- * Response Lenses
    , dlbarsLoadBalancerAttributes
    , dlbarsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBalancerAttributes' smart constructor.
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
    { _dlbaLoadBalancerName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLoadBalancerAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbaLoadBalancerName'
describeLoadBalancerAttributes
    :: Text -- ^ 'dlbaLoadBalancerName'
    -> DescribeLoadBalancerAttributes
describeLoadBalancerAttributes pLoadBalancerName_ =
    DescribeLoadBalancerAttributes'
    { _dlbaLoadBalancerName = pLoadBalancerName_
    }

-- | The name of the load balancer.
dlbaLoadBalancerName :: Lens' DescribeLoadBalancerAttributes Text
dlbaLoadBalancerName = lens _dlbaLoadBalancerName (\ s a -> s{_dlbaLoadBalancerName = a});

instance AWSRequest DescribeLoadBalancerAttributes
         where
        type Rs DescribeLoadBalancerAttributes =
             DescribeLoadBalancerAttributesResponse
        request = postQuery eLB
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
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
    { _dlbarsLoadBalancerAttributes :: !(Maybe LoadBalancerAttributes)
    , _dlbarsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeLoadBalancerAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbarsLoadBalancerAttributes'
--
-- * 'dlbarsStatus'
describeLoadBalancerAttributesResponse
    :: Int -- ^ 'dlbarsStatus'
    -> DescribeLoadBalancerAttributesResponse
describeLoadBalancerAttributesResponse pStatus_ =
    DescribeLoadBalancerAttributesResponse'
    { _dlbarsLoadBalancerAttributes = Nothing
    , _dlbarsStatus = pStatus_
    }

-- | Information about the load balancer attributes.
dlbarsLoadBalancerAttributes :: Lens' DescribeLoadBalancerAttributesResponse (Maybe LoadBalancerAttributes)
dlbarsLoadBalancerAttributes = lens _dlbarsLoadBalancerAttributes (\ s a -> s{_dlbarsLoadBalancerAttributes = a});

-- | The response status code.
dlbarsStatus :: Lens' DescribeLoadBalancerAttributesResponse Int
dlbarsStatus = lens _dlbarsStatus (\ s a -> s{_dlbarsStatus = a});
