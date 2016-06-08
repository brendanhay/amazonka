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
-- Module      : Network.AWS.EC2.DescribeNatGateways
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the your NAT gateways.
module Network.AWS.EC2.DescribeNatGateways
    (
    -- * Creating a Request
      describeNatGateways
    , DescribeNatGateways
    -- * Request Lenses
    , dngNatGatewayIds
    , dngNextToken
    , dngFilter
    , dngMaxResults

    -- * Destructuring the Response
    , describeNatGatewaysResponse
    , DescribeNatGatewaysResponse
    -- * Response Lenses
    , dngrsNatGateways
    , dngrsNextToken
    , dngrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeNatGateways.
--
-- /See:/ 'describeNatGateways' smart constructor.
data DescribeNatGateways = DescribeNatGateways'
    { _dngNatGatewayIds :: !(Maybe [Text])
    , _dngNextToken     :: !(Maybe Text)
    , _dngFilter        :: !(Maybe [Filter])
    , _dngMaxResults    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeNatGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngNatGatewayIds'
--
-- * 'dngNextToken'
--
-- * 'dngFilter'
--
-- * 'dngMaxResults'
describeNatGateways
    :: DescribeNatGateways
describeNatGateways =
    DescribeNatGateways'
    { _dngNatGatewayIds = Nothing
    , _dngNextToken = Nothing
    , _dngFilter = Nothing
    , _dngMaxResults = Nothing
    }

-- | One or more NAT gateway IDs.
dngNatGatewayIds :: Lens' DescribeNatGateways [Text]
dngNatGatewayIds = lens _dngNatGatewayIds (\ s a -> s{_dngNatGatewayIds = a}) . _Default . _Coerce;

-- | The token to retrieve the next page of results.
dngNextToken :: Lens' DescribeNatGateways (Maybe Text)
dngNextToken = lens _dngNextToken (\ s a -> s{_dngNextToken = a});

-- | One or more filters.
--
-- -   'nat-gateway-id' - The ID of the NAT gateway.
--
-- -   'state' - The state of the NAT gateway ('pending' | 'failed' | 'available' | 'deleting' | 'deleted').
--
-- -   'subnet-id' - The ID of the subnet in which the NAT gateway resides.
--
-- -   'vpc-id' - The ID of the VPC in which the NAT gateway resides.
--
dngFilter :: Lens' DescribeNatGateways [Filter]
dngFilter = lens _dngFilter (\ s a -> s{_dngFilter = a}) . _Default . _Coerce;

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value specified is greater than 1000, we return only 1000 items.
dngMaxResults :: Lens' DescribeNatGateways (Maybe Int)
dngMaxResults = lens _dngMaxResults (\ s a -> s{_dngMaxResults = a});

instance AWSRequest DescribeNatGateways where
        type Rs DescribeNatGateways =
             DescribeNatGatewaysResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeNatGatewaysResponse' <$>
                   (x .@? "natGatewaySet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeNatGateways

instance NFData DescribeNatGateways

instance ToHeaders DescribeNatGateways where
        toHeaders = const mempty

instance ToPath DescribeNatGateways where
        toPath = const "/"

instance ToQuery DescribeNatGateways where
        toQuery DescribeNatGateways'{..}
          = mconcat
              ["Action" =: ("DescribeNatGateways" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               toQuery
                 (toQueryList "NatGatewayId" <$> _dngNatGatewayIds),
               "NextToken" =: _dngNextToken,
               toQuery (toQueryList "Filter" <$> _dngFilter),
               "MaxResults" =: _dngMaxResults]

-- | Contains the output of DescribeNatGateways.
--
-- /See:/ 'describeNatGatewaysResponse' smart constructor.
data DescribeNatGatewaysResponse = DescribeNatGatewaysResponse'
    { _dngrsNatGateways    :: !(Maybe [NatGateway])
    , _dngrsNextToken      :: !(Maybe Text)
    , _dngrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeNatGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngrsNatGateways'
--
-- * 'dngrsNextToken'
--
-- * 'dngrsResponseStatus'
describeNatGatewaysResponse
    :: Int -- ^ 'dngrsResponseStatus'
    -> DescribeNatGatewaysResponse
describeNatGatewaysResponse pResponseStatus_ =
    DescribeNatGatewaysResponse'
    { _dngrsNatGateways = Nothing
    , _dngrsNextToken = Nothing
    , _dngrsResponseStatus = pResponseStatus_
    }

-- | Information about the NAT gateways.
dngrsNatGateways :: Lens' DescribeNatGatewaysResponse [NatGateway]
dngrsNatGateways = lens _dngrsNatGateways (\ s a -> s{_dngrsNatGateways = a}) . _Default . _Coerce;

-- | The token to use to retrieve the next page of results. This value is 'null' when there are no more results to return.
dngrsNextToken :: Lens' DescribeNatGatewaysResponse (Maybe Text)
dngrsNextToken = lens _dngrsNextToken (\ s a -> s{_dngrsNextToken = a});

-- | The response status code.
dngrsResponseStatus :: Lens' DescribeNatGatewaysResponse Int
dngrsResponseStatus = lens _dngrsResponseStatus (\ s a -> s{_dngrsResponseStatus = a});

instance NFData DescribeNatGatewaysResponse
