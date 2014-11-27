{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your Internet gateways.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInternetGateways.html>
module Network.AWS.EC2.DescribeInternetGateways
    (
    -- * Request
      DescribeInternetGateways
    -- ** Request constructor
    , describeInternetGateways
    -- ** Request lenses
    , dig1DryRun
    , dig1Filters
    , dig1InternetGatewayIds

    -- * Response
    , DescribeInternetGatewaysResponse
    -- ** Response constructor
    , describeInternetGatewaysResponse
    -- ** Response lenses
    , digrInternetGateways
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeInternetGateways = DescribeInternetGateways
    { _dig1DryRun             :: Maybe Bool
    , _dig1Filters            :: List "Filter" Filter
    , _dig1InternetGatewayIds :: List "item" Text
    } deriving (Eq, Show)

-- | 'DescribeInternetGateways' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dig1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dig1Filters' @::@ ['Filter']
--
-- * 'dig1InternetGatewayIds' @::@ ['Text']
--
describeInternetGateways :: DescribeInternetGateways
describeInternetGateways = DescribeInternetGateways
    { _dig1DryRun             = Nothing
    , _dig1InternetGatewayIds = mempty
    , _dig1Filters            = mempty
    }

dig1DryRun :: Lens' DescribeInternetGateways (Maybe Bool)
dig1DryRun = lens _dig1DryRun (\s a -> s { _dig1DryRun = a })

-- | One or more filters.
--
-- 'attachment.state' - The current state of the attachment between the gateway
-- and the VPC ('available'). Present only if a VPC is attached.
--
-- 'attachment.vpc-id' - The ID of an attached VPC.
--
-- 'internet-gateway-id' - The ID of the Internet gateway.
--
-- 'tag':/key/=/value/ - The key/value combination of a tag assigned to the
-- resource.
--
-- 'tag-key' - The key of a tag assigned to the resource. This filter is
-- independent of the 'tag-value' filter. For example, if you use both the filter
-- "tag-key=Purpose" and the filter "tag-value=X", you get any resources
-- assigned both the tag key Purpose (regardless of what the tag's value is),
-- and the tag value X (regardless of what the tag's key is). If you want to
-- list only resources where Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- 'tag-value' - The value of a tag assigned to the resource. This filter is
-- independent of the 'tag-key' filter.
--
--
dig1Filters :: Lens' DescribeInternetGateways [Filter]
dig1Filters = lens _dig1Filters (\s a -> s { _dig1Filters = a }) . _List

-- | One or more Internet gateway IDs.
--
-- Default: Describes all your Internet gateways.
dig1InternetGatewayIds :: Lens' DescribeInternetGateways [Text]
dig1InternetGatewayIds =
    lens _dig1InternetGatewayIds (\s a -> s { _dig1InternetGatewayIds = a })
        . _List

newtype DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { _digrInternetGateways :: List "item" InternetGateway
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeInternetGatewaysResponse where
    type Item DescribeInternetGatewaysResponse = InternetGateway

    fromList = DescribeInternetGatewaysResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _digrInternetGateways

-- | 'DescribeInternetGatewaysResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'digrInternetGateways' @::@ ['InternetGateway']
--
describeInternetGatewaysResponse :: DescribeInternetGatewaysResponse
describeInternetGatewaysResponse = DescribeInternetGatewaysResponse
    { _digrInternetGateways = mempty
    }

-- | Information about one or more Internet gateways.
digrInternetGateways :: Lens' DescribeInternetGatewaysResponse [InternetGateway]
digrInternetGateways =
    lens _digrInternetGateways (\s a -> s { _digrInternetGateways = a })
        . _List

instance ToPath DescribeInternetGateways where
    toPath = const "/"

instance ToQuery DescribeInternetGateways where
    toQuery DescribeInternetGateways{..} = mconcat
        [ "dryRun"            =? _dig1DryRun
        , "Filter"            =? _dig1Filters
        , "internetGatewayId" =? _dig1InternetGatewayIds
        ]

instance ToHeaders DescribeInternetGateways

instance AWSRequest DescribeInternetGateways where
    type Sv DescribeInternetGateways = EC2
    type Rs DescribeInternetGateways = DescribeInternetGatewaysResponse

    request  = post "DescribeInternetGateways"
    response = xmlResponse

instance FromXML DescribeInternetGatewaysResponse where
    parseXML x = DescribeInternetGatewaysResponse
        <$> x .@  "internetGatewaySet"
