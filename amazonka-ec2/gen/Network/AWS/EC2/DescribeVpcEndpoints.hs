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

-- Module      : Network.AWS.EC2.DescribeVpcEndpoints
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

-- | Describes one or more of your VPC endpoints.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcEndpoints.html>
module Network.AWS.EC2.DescribeVpcEndpoints
    (
    -- * Request
      DescribeVpcEndpoints
    -- ** Request constructor
    , describeVpcEndpoints
    -- ** Request lenses
    , dve1DryRun
    , dve1Filters
    , dve1MaxResults
    , dve1NextToken
    , dve1VpcEndpointIds

    -- * Response
    , DescribeVpcEndpointsResponse
    -- ** Response constructor
    , describeVpcEndpointsResponse
    -- ** Response lenses
    , dverNextToken
    , dverVpcEndpoints
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVpcEndpoints = DescribeVpcEndpoints
    { _dve1DryRun         :: Maybe Bool
    , _dve1Filters        :: List "Filter" Filter
    , _dve1MaxResults     :: Maybe Int
    , _dve1NextToken      :: Maybe Text
    , _dve1VpcEndpointIds :: List "item" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeVpcEndpoints' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dve1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dve1Filters' @::@ ['Filter']
--
-- * 'dve1MaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dve1NextToken' @::@ 'Maybe' 'Text'
--
-- * 'dve1VpcEndpointIds' @::@ ['Text']
--
describeVpcEndpoints :: DescribeVpcEndpoints
describeVpcEndpoints = DescribeVpcEndpoints
    { _dve1DryRun         = Nothing
    , _dve1VpcEndpointIds = mempty
    , _dve1Filters        = mempty
    , _dve1MaxResults     = Nothing
    , _dve1NextToken      = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dve1DryRun :: Lens' DescribeVpcEndpoints (Maybe Bool)
dve1DryRun = lens _dve1DryRun (\s a -> s { _dve1DryRun = a })

-- | One or more filters.
--
-- 'service-name': The name of the AWS service.
--
-- 'vpc-id': The ID of the VPC in which the endpoint resides.
--
-- 'vpc-endpoint-id': The ID of the endpoint.
--
-- 'vpc-endpoint-state': The state of the endpoint. ('pending' | 'available' | 'deleting' | 'deleted')
--
--
dve1Filters :: Lens' DescribeVpcEndpoints [Filter]
dve1Filters = lens _dve1Filters (\s a -> s { _dve1Filters = a }) . _List

-- | The maximum number of items to return for this request. The request returns a
-- token that you can specify in a subsequent call to get the next set of
-- results.
--
-- Constraint: If the value is greater than 1000, we return only 1000 items.
dve1MaxResults :: Lens' DescribeVpcEndpoints (Maybe Int)
dve1MaxResults = lens _dve1MaxResults (\s a -> s { _dve1MaxResults = a })

-- | The token for the next set of items to return. (You received this token from
-- a prior call.)
dve1NextToken :: Lens' DescribeVpcEndpoints (Maybe Text)
dve1NextToken = lens _dve1NextToken (\s a -> s { _dve1NextToken = a })

-- | One or more endpoint IDs.
dve1VpcEndpointIds :: Lens' DescribeVpcEndpoints [Text]
dve1VpcEndpointIds =
    lens _dve1VpcEndpointIds (\s a -> s { _dve1VpcEndpointIds = a })
        . _List

data DescribeVpcEndpointsResponse = DescribeVpcEndpointsResponse
    { _dverNextToken    :: Maybe Text
    , _dverVpcEndpoints :: List "item" VpcEndpoint
    } deriving (Eq, Read, Show)

-- | 'DescribeVpcEndpointsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dverNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dverVpcEndpoints' @::@ ['VpcEndpoint']
--
describeVpcEndpointsResponse :: DescribeVpcEndpointsResponse
describeVpcEndpointsResponse = DescribeVpcEndpointsResponse
    { _dverVpcEndpoints = mempty
    , _dverNextToken    = Nothing
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dverNextToken :: Lens' DescribeVpcEndpointsResponse (Maybe Text)
dverNextToken = lens _dverNextToken (\s a -> s { _dverNextToken = a })

-- | Information about the endpoints.
dverVpcEndpoints :: Lens' DescribeVpcEndpointsResponse [VpcEndpoint]
dverVpcEndpoints = lens _dverVpcEndpoints (\s a -> s { _dverVpcEndpoints = a }) . _List

instance ToPath DescribeVpcEndpoints where
    toPath = const "/"

instance ToQuery DescribeVpcEndpoints where
    toQuery DescribeVpcEndpoints{..} = mconcat
        [ "DryRun"        =? _dve1DryRun
        , "Filter"        `toQueryList` _dve1Filters
        , "MaxResults"    =? _dve1MaxResults
        , "NextToken"     =? _dve1NextToken
        , "VpcEndpointId" `toQueryList` _dve1VpcEndpointIds
        ]

instance ToHeaders DescribeVpcEndpoints

instance AWSRequest DescribeVpcEndpoints where
    type Sv DescribeVpcEndpoints = EC2
    type Rs DescribeVpcEndpoints = DescribeVpcEndpointsResponse

    request  = post "DescribeVpcEndpoints"
    response = xmlResponse

instance FromXML DescribeVpcEndpointsResponse where
    parseXML x = DescribeVpcEndpointsResponse
        <$> x .@? "nextToken"
        <*> x .@? "vpcEndpointSet" .!@ mempty
