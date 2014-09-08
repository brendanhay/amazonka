{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeInternetGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your Internet gateways. Example This example
-- describes all your Internet gateways.
-- https://ec2.amazonaws.com/?Action=DescribeInternetGateways &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE igw-eaad4883EXAMPLE vpc-11ad4878
-- available.
module Network.AWS.EC2.V2014_06_15.DescribeInternetGateways
    (
    -- * Request
      DescribeInternetGateways
    -- ** Request constructor
    , mkDescribeInternetGateways
    -- ** Request lenses
    , dig1InternetGatewayIds
    , dig1Filters

    -- * Response
    , DescribeInternetGatewaysResponse
    -- ** Response constructor
    , mkDescribeInternetGatewaysResponse
    -- ** Response lenses
    , digrInternetGateways
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeInternetGateways = DescribeInternetGateways
    { _dig1InternetGatewayIds :: [Text]
    , _dig1Filters :: [Filter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInternetGateways' request.
mkDescribeInternetGateways :: DescribeInternetGateways
mkDescribeInternetGateways = DescribeInternetGateways
    { _dig1InternetGatewayIds = mempty
    , _dig1Filters = mempty
    }

-- | One or more Internet gateway IDs. Default: Describes all your Internet
-- gateways.
dig1InternetGatewayIds :: Lens' DescribeInternetGateways [Text]
dig1InternetGatewayIds =
    lens _dig1InternetGatewayIds (\s a -> s { _dig1InternetGatewayIds = a })

-- | One or more filters. attachment.state - The current state of the attachment
-- between the gateway and the VPC. Present only if a VPC is attached.
-- attachment.vpc-id - The ID of an attached VPC. internet-gateway-id - The ID
-- of the Internet gateway. tag:key=value - The key/value combination of a tag
-- assigned to the resource. tag-key - The key of a tag assigned to the
-- resource. This filter is independent of the tag-value filter. For example,
-- if you use both the filter "tag-key=Purpose" and the filter "tag-value=X",
-- you get any resources assigned both the tag key Purpose (regardless of what
-- the tag's value is), and the tag value X (regardless of what the tag's key
-- is). If you want to list only resources where Purpose is X, see the
-- tag:key=value filter. tag-value - The value of a tag assigned to the
-- resource. This filter is independent of the tag-key filter.
dig1Filters :: Lens' DescribeInternetGateways [Filter]
dig1Filters = lens _dig1Filters (\s a -> s { _dig1Filters = a })

instance ToQuery DescribeInternetGateways where
    toQuery = genericQuery def

-- | 
newtype DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { _digrInternetGateways :: [InternetGateway]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInternetGatewaysResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeInternetGatewaysResponse :: DescribeInternetGatewaysResponse
mkDescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { _digrInternetGateways = mempty
    }

-- | Information about one or more Internet gateways.
digrInternetGateways :: Lens' DescribeInternetGatewaysResponse [InternetGateway]
digrInternetGateways =
    lens _digrInternetGateways (\s a -> s { _digrInternetGateways = a })

instance FromXML DescribeInternetGatewaysResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeInternetGateways where
    type Sv DescribeInternetGateways = EC2
    type Rs DescribeInternetGateways = DescribeInternetGatewaysResponse

    request = post "DescribeInternetGateways"
    response _ = xmlResponse
