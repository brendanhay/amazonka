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

-- Module      : Network.AWS.EC2.DescribeVpcEndpointServices
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

-- | Describes all supported AWS services that can be specified when creating a
-- VPC endpoint.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpcEndpointServices.html>
module Network.AWS.EC2.DescribeVpcEndpointServices
    (
    -- * Request
      DescribeVpcEndpointServices
    -- ** Request constructor
    , describeVpcEndpointServices
    -- ** Request lenses
    , dvesDryRun
    , dvesMaxResults
    , dvesNextToken

    -- * Response
    , DescribeVpcEndpointServicesResponse
    -- ** Response constructor
    , describeVpcEndpointServicesResponse
    -- ** Response lenses
    , dvesrNextToken
    , dvesrServiceNames
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVpcEndpointServices = DescribeVpcEndpointServices
    { _dvesDryRun     :: Maybe Bool
    , _dvesMaxResults :: Maybe Int
    , _dvesNextToken  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeVpcEndpointServices' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvesDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvesMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dvesNextToken' @::@ 'Maybe' 'Text'
--
describeVpcEndpointServices :: DescribeVpcEndpointServices
describeVpcEndpointServices = DescribeVpcEndpointServices
    { _dvesDryRun     = Nothing
    , _dvesMaxResults = Nothing
    , _dvesNextToken  = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dvesDryRun :: Lens' DescribeVpcEndpointServices (Maybe Bool)
dvesDryRun = lens _dvesDryRun (\s a -> s { _dvesDryRun = a })

-- | The maximum number of items to return for this request. The request returns a
-- token that you can specify in a subsequent call to get the next set of
-- results.
--
-- Constraint: If the value is greater than 1000, we return only 1000 items.
dvesMaxResults :: Lens' DescribeVpcEndpointServices (Maybe Int)
dvesMaxResults = lens _dvesMaxResults (\s a -> s { _dvesMaxResults = a })

-- | The token for the next set of items to return. (You received this token from
-- a prior call.)
dvesNextToken :: Lens' DescribeVpcEndpointServices (Maybe Text)
dvesNextToken = lens _dvesNextToken (\s a -> s { _dvesNextToken = a })

data DescribeVpcEndpointServicesResponse = DescribeVpcEndpointServicesResponse
    { _dvesrNextToken    :: Maybe Text
    , _dvesrServiceNames :: List "item" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeVpcEndpointServicesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvesrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dvesrServiceNames' @::@ ['Text']
--
describeVpcEndpointServicesResponse :: DescribeVpcEndpointServicesResponse
describeVpcEndpointServicesResponse = DescribeVpcEndpointServicesResponse
    { _dvesrServiceNames = mempty
    , _dvesrNextToken    = Nothing
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dvesrNextToken :: Lens' DescribeVpcEndpointServicesResponse (Maybe Text)
dvesrNextToken = lens _dvesrNextToken (\s a -> s { _dvesrNextToken = a })

-- | A list of supported AWS services.
dvesrServiceNames :: Lens' DescribeVpcEndpointServicesResponse [Text]
dvesrServiceNames =
    lens _dvesrServiceNames (\s a -> s { _dvesrServiceNames = a })
        . _List

instance ToPath DescribeVpcEndpointServices where
    toPath = const "/"

instance ToQuery DescribeVpcEndpointServices where
    toQuery DescribeVpcEndpointServices{..} = mconcat
        [ "DryRun"     =? _dvesDryRun
        , "MaxResults" =? _dvesMaxResults
        , "NextToken"  =? _dvesNextToken
        ]

instance ToHeaders DescribeVpcEndpointServices

instance AWSRequest DescribeVpcEndpointServices where
    type Sv DescribeVpcEndpointServices = EC2
    type Rs DescribeVpcEndpointServices = DescribeVpcEndpointServicesResponse

    request  = post "DescribeVpcEndpointServices"
    response = xmlResponse

instance FromXML DescribeVpcEndpointServicesResponse where
    parseXML x = DescribeVpcEndpointServicesResponse
        <$> x .@? "nextToken"
        <*> x .@? "serviceNameSet" .!@ mempty
