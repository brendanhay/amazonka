{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.DescribeVirtualGateways
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

-- | Returns a list of virtual private gateways owned by the AWS account.
--
-- You can create one or more AWS Direct Connect private virtual interfaces
-- linking to a virtual private gateway. A virtual private gateway can be
-- managed via Amazon Virtual Private Cloud (VPC) console or the
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html EC2 CreateVpnGateway>
-- action.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeVirtualGateways.html>
module Network.AWS.DirectConnect.DescribeVirtualGateways
    (
    -- * Request
      DescribeVirtualGateways
    -- ** Request constructor
    , describeVirtualGateways

    -- * Response
    , DescribeVirtualGatewaysResponse
    -- ** Response constructor
    , describeVirtualGatewaysResponse
    -- ** Response lenses
    , dvgrVirtualGateways
    , dvgrStatusCode
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVirtualGateways' smart constructor.
data DescribeVirtualGateways = DescribeVirtualGateways' deriving (Eq, Read, Show)

-- | 'DescribeVirtualGateways' smart constructor.
describeVirtualGateways :: DescribeVirtualGateways
describeVirtualGateways = DescribeVirtualGateways';

instance AWSRequest DescribeVirtualGateways where
        type Sv DescribeVirtualGateways = DirectConnect
        type Rs DescribeVirtualGateways =
             DescribeVirtualGatewaysResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVirtualGatewaysResponse' <$>
                   (x .?> "virtualGateways" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeVirtualGateways where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeVirtualGateways" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeVirtualGateways where
        toJSON = const (Object mempty)

instance ToPath DescribeVirtualGateways where
        toPath = const "/"

instance ToQuery DescribeVirtualGateways where
        toQuery = const mempty

-- | A structure containing a list of virtual private gateways.
--
-- /See:/ 'describeVirtualGatewaysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvgrVirtualGateways'
--
-- * 'dvgrStatusCode'
data DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse'{_dvgrVirtualGateways :: Maybe [VirtualGateway], _dvgrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeVirtualGatewaysResponse' smart constructor.
describeVirtualGatewaysResponse :: Int -> DescribeVirtualGatewaysResponse
describeVirtualGatewaysResponse pStatusCode = DescribeVirtualGatewaysResponse'{_dvgrVirtualGateways = Nothing, _dvgrStatusCode = pStatusCode};

-- | A list of virtual private gateways.
dvgrVirtualGateways :: Lens' DescribeVirtualGatewaysResponse [VirtualGateway]
dvgrVirtualGateways = lens _dvgrVirtualGateways (\ s a -> s{_dvgrVirtualGateways = a}) . _Default;

-- | FIXME: Undocumented member.
dvgrStatusCode :: Lens' DescribeVirtualGatewaysResponse Int
dvgrStatusCode = lens _dvgrStatusCode (\ s a -> s{_dvgrStatusCode = a});
