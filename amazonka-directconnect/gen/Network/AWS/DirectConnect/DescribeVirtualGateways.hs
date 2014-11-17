{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeVirtualGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of virtual private gateways owned by the AWS account. You
-- can create one or more AWS Direct Connect private virtual interfaces
-- linking to a virtual private gateway. A virtual private gateway can be
-- managed via Amazon Virtual Private Cloud (VPC) console or the EC2
-- CreateVpnGateway action.
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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data DescribeVirtualGateways = DescribeVirtualGateways
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeVirtualGateways' constructor.
describeVirtualGateways :: DescribeVirtualGateways
describeVirtualGateways = DescribeVirtualGateways

newtype DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse
    { _dvgrVirtualGateways :: [VirtualGateway]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeVirtualGatewaysResponse where
    type Item DescribeVirtualGatewaysResponse = VirtualGateway

    fromList = DescribeVirtualGatewaysResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dvgrVirtualGateways

-- | 'DescribeVirtualGatewaysResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvgrVirtualGateways' @::@ ['VirtualGateway']
--
describeVirtualGatewaysResponse :: DescribeVirtualGatewaysResponse
describeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse
    { _dvgrVirtualGateways = mempty
    }

-- | A list of virtual private gateways.
dvgrVirtualGateways :: Lens' DescribeVirtualGatewaysResponse [VirtualGateway]
dvgrVirtualGateways =
    lens _dvgrVirtualGateways (\s a -> s { _dvgrVirtualGateways = a })

instance AWSRequest DescribeVirtualGateways where
    type Sv DescribeVirtualGateways = DirectConnect
    type Rs DescribeVirtualGateways = DescribeVirtualGatewaysResponse

    request  = post
    response = jsonResponse

instance FromJSON DescribeVirtualGatewaysResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath DescribeVirtualGateways where
    toPath = const "/"

instance ToHeaders DescribeVirtualGateways

instance ToQuery DescribeVirtualGateways
