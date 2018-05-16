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
-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Internet gateways.
--
--
module Network.AWS.EC2.DescribeInternetGateways
    (
    -- * Creating a Request
      describeInternetGateways
    , DescribeInternetGateways
    -- * Request Lenses
    , dFilters
    , dInternetGatewayIds
    , dDryRun

    -- * Destructuring the Response
    , describeInternetGatewaysResponse
    , DescribeInternetGatewaysResponse
    -- * Response Lenses
    , digrsInternetGateways
    , digrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeInternetGateways.
--
--
--
-- /See:/ 'describeInternetGateways' smart constructor.
data DescribeInternetGateways = DescribeInternetGateways'
  { _dFilters            :: !(Maybe [Filter])
  , _dInternetGatewayIds :: !(Maybe [Text])
  , _dDryRun             :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInternetGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dFilters' - One or more filters.     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.     * @attachment.vpc-id@ - The ID of an attached VPC.     * @internet-gateway-id@ - The ID of the Internet gateway.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.
--
-- * 'dInternetGatewayIds' - One or more Internet gateway IDs. Default: Describes all your Internet gateways.
--
-- * 'dDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeInternetGateways
    :: DescribeInternetGateways
describeInternetGateways =
  DescribeInternetGateways'
    {_dFilters = Nothing, _dInternetGatewayIds = Nothing, _dDryRun = Nothing}


-- | One or more filters.     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.     * @attachment.vpc-id@ - The ID of an attached VPC.     * @internet-gateway-id@ - The ID of the Internet gateway.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of what the tag's key is). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.     * @tag-value@ - The value of a tag assigned to the resource. This filter is independent of the @tag-key@ filter.
dFilters :: Lens' DescribeInternetGateways [Filter]
dFilters = lens _dFilters (\ s a -> s{_dFilters = a}) . _Default . _Coerce

-- | One or more Internet gateway IDs. Default: Describes all your Internet gateways.
dInternetGatewayIds :: Lens' DescribeInternetGateways [Text]
dInternetGatewayIds = lens _dInternetGatewayIds (\ s a -> s{_dInternetGatewayIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dDryRun :: Lens' DescribeInternetGateways (Maybe Bool)
dDryRun = lens _dDryRun (\ s a -> s{_dDryRun = a})

instance AWSRequest DescribeInternetGateways where
        type Rs DescribeInternetGateways =
             DescribeInternetGatewaysResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeInternetGatewaysResponse' <$>
                   (x .@? "internetGatewaySet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInternetGateways where

instance NFData DescribeInternetGateways where

instance ToHeaders DescribeInternetGateways where
        toHeaders = const mempty

instance ToPath DescribeInternetGateways where
        toPath = const "/"

instance ToQuery DescribeInternetGateways where
        toQuery DescribeInternetGateways'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInternetGateways" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dFilters),
               toQuery
                 (toQueryList "InternetGatewayId" <$>
                    _dInternetGatewayIds),
               "DryRun" =: _dDryRun]

-- | Contains the output of DescribeInternetGateways.
--
--
--
-- /See:/ 'describeInternetGatewaysResponse' smart constructor.
data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse'
  { _digrsInternetGateways :: !(Maybe [InternetGateway])
  , _digrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInternetGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digrsInternetGateways' - Information about one or more Internet gateways.
--
-- * 'digrsResponseStatus' - -- | The response status code.
describeInternetGatewaysResponse
    :: Int -- ^ 'digrsResponseStatus'
    -> DescribeInternetGatewaysResponse
describeInternetGatewaysResponse pResponseStatus_ =
  DescribeInternetGatewaysResponse'
    {_digrsInternetGateways = Nothing, _digrsResponseStatus = pResponseStatus_}


-- | Information about one or more Internet gateways.
digrsInternetGateways :: Lens' DescribeInternetGatewaysResponse [InternetGateway]
digrsInternetGateways = lens _digrsInternetGateways (\ s a -> s{_digrsInternetGateways = a}) . _Default . _Coerce

-- | -- | The response status code.
digrsResponseStatus :: Lens' DescribeInternetGatewaysResponse Int
digrsResponseStatus = lens _digrsResponseStatus (\ s a -> s{_digrsResponseStatus = a})

instance NFData DescribeInternetGatewaysResponse
         where
