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
-- Module      : Network.AWS.EC2.DescribeEgressOnlyInternetGateways
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your egress-only Internet gateways.
--
--
module Network.AWS.EC2.DescribeEgressOnlyInternetGateways
    (
    -- * Creating a Request
      describeEgressOnlyInternetGateways
    , DescribeEgressOnlyInternetGateways
    -- * Request Lenses
    , deoigEgressOnlyInternetGatewayIds
    , deoigNextToken
    , deoigDryRun
    , deoigMaxResults

    -- * Destructuring the Response
    , describeEgressOnlyInternetGatewaysResponse
    , DescribeEgressOnlyInternetGatewaysResponse
    -- * Response Lenses
    , deoigrsEgressOnlyInternetGateways
    , deoigrsNextToken
    , deoigrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEgressOnlyInternetGateways' smart constructor.
data DescribeEgressOnlyInternetGateways = DescribeEgressOnlyInternetGateways'
  { _deoigEgressOnlyInternetGatewayIds :: !(Maybe [Text])
  , _deoigNextToken                    :: !(Maybe Text)
  , _deoigDryRun                       :: !(Maybe Bool)
  , _deoigMaxResults                   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEgressOnlyInternetGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deoigEgressOnlyInternetGatewayIds' - One or more egress-only Internet gateway IDs.
--
-- * 'deoigNextToken' - The token to retrieve the next page of results.
--
-- * 'deoigDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'deoigMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned.
describeEgressOnlyInternetGateways
    :: DescribeEgressOnlyInternetGateways
describeEgressOnlyInternetGateways =
  DescribeEgressOnlyInternetGateways'
    { _deoigEgressOnlyInternetGatewayIds = Nothing
    , _deoigNextToken = Nothing
    , _deoigDryRun = Nothing
    , _deoigMaxResults = Nothing
    }


-- | One or more egress-only Internet gateway IDs.
deoigEgressOnlyInternetGatewayIds :: Lens' DescribeEgressOnlyInternetGateways [Text]
deoigEgressOnlyInternetGatewayIds = lens _deoigEgressOnlyInternetGatewayIds (\ s a -> s{_deoigEgressOnlyInternetGatewayIds = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
deoigNextToken :: Lens' DescribeEgressOnlyInternetGateways (Maybe Text)
deoigNextToken = lens _deoigNextToken (\ s a -> s{_deoigNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deoigDryRun :: Lens' DescribeEgressOnlyInternetGateways (Maybe Bool)
deoigDryRun = lens _deoigDryRun (\ s a -> s{_deoigDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned.
deoigMaxResults :: Lens' DescribeEgressOnlyInternetGateways (Maybe Int)
deoigMaxResults = lens _deoigMaxResults (\ s a -> s{_deoigMaxResults = a})

instance AWSRequest
           DescribeEgressOnlyInternetGateways
         where
        type Rs DescribeEgressOnlyInternetGateways =
             DescribeEgressOnlyInternetGatewaysResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeEgressOnlyInternetGatewaysResponse' <$>
                   (x .@? "egressOnlyInternetGatewaySet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEgressOnlyInternetGateways
         where

instance NFData DescribeEgressOnlyInternetGateways
         where

instance ToHeaders DescribeEgressOnlyInternetGateways
         where
        toHeaders = const mempty

instance ToPath DescribeEgressOnlyInternetGateways
         where
        toPath = const "/"

instance ToQuery DescribeEgressOnlyInternetGateways
         where
        toQuery DescribeEgressOnlyInternetGateways'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEgressOnlyInternetGateways" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "EgressOnlyInternetGatewayId" <$>
                    _deoigEgressOnlyInternetGatewayIds),
               "NextToken" =: _deoigNextToken,
               "DryRun" =: _deoigDryRun,
               "MaxResults" =: _deoigMaxResults]

-- | /See:/ 'describeEgressOnlyInternetGatewaysResponse' smart constructor.
data DescribeEgressOnlyInternetGatewaysResponse = DescribeEgressOnlyInternetGatewaysResponse'
  { _deoigrsEgressOnlyInternetGateways :: !(Maybe [EgressOnlyInternetGateway])
  , _deoigrsNextToken                  :: !(Maybe Text)
  , _deoigrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEgressOnlyInternetGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deoigrsEgressOnlyInternetGateways' - Information about the egress-only Internet gateways.
--
-- * 'deoigrsNextToken' - The token to use to retrieve the next page of results.
--
-- * 'deoigrsResponseStatus' - -- | The response status code.
describeEgressOnlyInternetGatewaysResponse
    :: Int -- ^ 'deoigrsResponseStatus'
    -> DescribeEgressOnlyInternetGatewaysResponse
describeEgressOnlyInternetGatewaysResponse pResponseStatus_ =
  DescribeEgressOnlyInternetGatewaysResponse'
    { _deoigrsEgressOnlyInternetGateways = Nothing
    , _deoigrsNextToken = Nothing
    , _deoigrsResponseStatus = pResponseStatus_
    }


-- | Information about the egress-only Internet gateways.
deoigrsEgressOnlyInternetGateways :: Lens' DescribeEgressOnlyInternetGatewaysResponse [EgressOnlyInternetGateway]
deoigrsEgressOnlyInternetGateways = lens _deoigrsEgressOnlyInternetGateways (\ s a -> s{_deoigrsEgressOnlyInternetGateways = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results.
deoigrsNextToken :: Lens' DescribeEgressOnlyInternetGatewaysResponse (Maybe Text)
deoigrsNextToken = lens _deoigrsNextToken (\ s a -> s{_deoigrsNextToken = a})

-- | -- | The response status code.
deoigrsResponseStatus :: Lens' DescribeEgressOnlyInternetGatewaysResponse Int
deoigrsResponseStatus = lens _deoigrsResponseStatus (\ s a -> s{_deoigrsResponseStatus = a})

instance NFData
           DescribeEgressOnlyInternetGatewaysResponse
         where
