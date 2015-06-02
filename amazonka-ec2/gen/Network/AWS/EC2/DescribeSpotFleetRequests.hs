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

-- Module      : Network.AWS.EC2.DescribeSpotFleetRequests
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

-- | Describes your Spot fleet requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotFleetRequests.html>
module Network.AWS.EC2.DescribeSpotFleetRequests
    (
    -- * Request
      DescribeSpotFleetRequests
    -- ** Request constructor
    , describeSpotFleetRequests
    -- ** Request lenses
    , dsfrDryRun
    , dsfrMaxResults
    , dsfrNextToken
    , dsfrSpotFleetRequestIds

    -- * Response
    , DescribeSpotFleetRequestsResponse
    -- ** Response constructor
    , describeSpotFleetRequestsResponse
    -- ** Response lenses
    , dsfrrNextToken
    , dsfrrSpotFleetRequestConfigs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeSpotFleetRequests = DescribeSpotFleetRequests
    { _dsfrDryRun              :: Maybe Bool
    , _dsfrMaxResults          :: Maybe Int
    , _dsfrNextToken           :: Maybe Text
    , _dsfrSpotFleetRequestIds :: List "item" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeSpotFleetRequests' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dsfrMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dsfrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsfrSpotFleetRequestIds' @::@ ['Text']
--
describeSpotFleetRequests :: DescribeSpotFleetRequests
describeSpotFleetRequests = DescribeSpotFleetRequests
    { _dsfrDryRun              = Nothing
    , _dsfrSpotFleetRequestIds = mempty
    , _dsfrNextToken           = Nothing
    , _dsfrMaxResults          = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dsfrDryRun :: Lens' DescribeSpotFleetRequests (Maybe Bool)
dsfrDryRun = lens _dsfrDryRun (\s a -> s { _dsfrDryRun = a })

-- | The maximum number of results to return in a single call. Specify a value
-- between 1 and 1000. The default value is 1000. To retrieve the remaining
-- results, make another call with the returned 'NextToken' value.
dsfrMaxResults :: Lens' DescribeSpotFleetRequests (Maybe Int)
dsfrMaxResults = lens _dsfrMaxResults (\s a -> s { _dsfrMaxResults = a })

-- | The token for the next set of results.
dsfrNextToken :: Lens' DescribeSpotFleetRequests (Maybe Text)
dsfrNextToken = lens _dsfrNextToken (\s a -> s { _dsfrNextToken = a })

-- | The IDs of the Spot fleet requests.
dsfrSpotFleetRequestIds :: Lens' DescribeSpotFleetRequests [Text]
dsfrSpotFleetRequestIds =
    lens _dsfrSpotFleetRequestIds (\s a -> s { _dsfrSpotFleetRequestIds = a })
        . _List

data DescribeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse
    { _dsfrrNextToken               :: Maybe Text
    , _dsfrrSpotFleetRequestConfigs :: List "item" SpotFleetRequestConfig
    } deriving (Eq, Read, Show)

-- | 'DescribeSpotFleetRequestsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsfrrSpotFleetRequestConfigs' @::@ ['SpotFleetRequestConfig']
--
describeSpotFleetRequestsResponse :: DescribeSpotFleetRequestsResponse
describeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse
    { _dsfrrSpotFleetRequestConfigs = mempty
    , _dsfrrNextToken               = Nothing
    }

-- | The token required to retrieve the next set of results. This value is 'null'
-- when there are no more results to return.
dsfrrNextToken :: Lens' DescribeSpotFleetRequestsResponse (Maybe Text)
dsfrrNextToken = lens _dsfrrNextToken (\s a -> s { _dsfrrNextToken = a })

-- | Information about the configuration of your Spot fleet.
dsfrrSpotFleetRequestConfigs :: Lens' DescribeSpotFleetRequestsResponse [SpotFleetRequestConfig]
dsfrrSpotFleetRequestConfigs =
    lens _dsfrrSpotFleetRequestConfigs
        (\s a -> s { _dsfrrSpotFleetRequestConfigs = a })
            . _List

instance ToPath DescribeSpotFleetRequests where
    toPath = const "/"

instance ToQuery DescribeSpotFleetRequests where
    toQuery DescribeSpotFleetRequests{..} = mconcat
        [ "DryRun"             =? _dsfrDryRun
        , "MaxResults"         =? _dsfrMaxResults
        , "NextToken"          =? _dsfrNextToken
        , "SpotFleetRequestId" `toQueryList` _dsfrSpotFleetRequestIds
        ]

instance ToHeaders DescribeSpotFleetRequests

instance AWSRequest DescribeSpotFleetRequests where
    type Sv DescribeSpotFleetRequests = EC2
    type Rs DescribeSpotFleetRequests = DescribeSpotFleetRequestsResponse

    request  = post "DescribeSpotFleetRequests"
    response = xmlResponse

instance FromXML DescribeSpotFleetRequestsResponse where
    parseXML x = DescribeSpotFleetRequestsResponse
        <$> x .@? "nextToken"
        <*> x .@? "spotFleetRequestConfigSet" .!@ mempty
