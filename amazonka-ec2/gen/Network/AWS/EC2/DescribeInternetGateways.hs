{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your Internet gateways.
module Network.AWS.EC2.DescribeInternetGateways
    (
    -- * Request
      DescribeInternetGateways
    -- ** Request constructor
    , describeInternetGateways
    -- ** Request lenses
    , digDryRun
    , digFilters
    , digInternetGatewayIds

    -- * Response
    , DescribeInternetGatewaysResult
    -- ** Response constructor
    , describeInternetGatewaysResult
    -- ** Response lenses
    , digrInternetGateways
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeInternetGateways = DescribeInternetGateways
    { _digDryRun             :: Maybe Bool
    , _digFilters            :: [Filter]
    , _digInternetGatewayIds :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeInternetGateways' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'digDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'digFilters' @::@ ['Filter']
--
-- * 'digInternetGatewayIds' @::@ ['Text']
--
describeInternetGateways :: DescribeInternetGateways
describeInternetGateways = DescribeInternetGateways
    { _digDryRun             = Nothing
    , _digInternetGatewayIds = mempty
    , _digFilters            = mempty
    }

digDryRun :: Lens' DescribeInternetGateways (Maybe Bool)
digDryRun = lens _digDryRun (\s a -> s { _digDryRun = a })

-- | One or more filters. attachment.state - The current state of the
-- attachment between the gateway and the VPC (available). Present only if a
-- VPC is attached. attachment.vpc-id - The ID of an attached VPC.
-- internet-gateway-id - The ID of the Internet gateway. tag:key=value - The
-- key/value combination of a tag assigned to the resource. tag-key - The
-- key of a tag assigned to the resource. This filter is independent of the
-- tag-value filter. For example, if you use both the filter
-- "tag-key=Purpose" and the filter "tag-value=X", you get any resources
-- assigned both the tag key Purpose (regardless of what the tag's value
-- is), and the tag value X (regardless of what the tag's key is). If you
-- want to list only resources where Purpose is X, see the tag:key=value
-- filter. tag-value - The value of a tag assigned to the resource. This
-- filter is independent of the tag-key filter.
digFilters :: Lens' DescribeInternetGateways [Filter]
digFilters = lens _digFilters (\s a -> s { _digFilters = a })

-- | One or more Internet gateway IDs. Default: Describes all your Internet
-- gateways.
digInternetGatewayIds :: Lens' DescribeInternetGateways [Text]
digInternetGatewayIds =
    lens _digInternetGatewayIds (\s a -> s { _digInternetGatewayIds = a })

instance ToQuery DescribeInternetGateways

instance ToPath DescribeInternetGateways where
    toPath = const "/"

newtype DescribeInternetGatewaysResult = DescribeInternetGatewaysResult
    { _digrInternetGateways :: [InternetGateway]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeInternetGatewaysResult
    type Item DescribeInternetGatewaysResult = InternetGateway

    fromList = DescribeInternetGatewaysResult . fromList
    toList   = toList . _digrInternetGateways

-- | 'DescribeInternetGatewaysResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'digrInternetGateways' @::@ ['InternetGateway']
--
describeInternetGatewaysResult :: DescribeInternetGatewaysResult
describeInternetGatewaysResult = DescribeInternetGatewaysResult
    { _digrInternetGateways = mempty
    }

-- | Information about one or more Internet gateways.
digrInternetGateways :: Lens' DescribeInternetGatewaysResult [InternetGateway]
digrInternetGateways =
    lens _digrInternetGateways (\s a -> s { _digrInternetGateways = a })

instance FromXML DescribeInternetGatewaysResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeInternetGatewaysResult"

instance AWSRequest DescribeInternetGateways where
    type Sv DescribeInternetGateways = EC2
    type Rs DescribeInternetGateways = DescribeInternetGatewaysResult

    request  = post "DescribeInternetGateways"
    response = xmlResponse $ \h x -> DescribeInternetGatewaysResult
        <$> x %| "internetGatewaySet"
