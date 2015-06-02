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

-- Module      : Network.AWS.EC2.DescribeSpotFleetInstances
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

-- | Describes the running instances for the specified Spot fleet.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotFleetInstances.html>
module Network.AWS.EC2.DescribeSpotFleetInstances
    (
    -- * Request
      DescribeSpotFleetInstances
    -- ** Request constructor
    , describeSpotFleetInstances
    -- ** Request lenses
    , dsfiDryRun
    , dsfiMaxResults
    , dsfiNextToken
    , dsfiSpotFleetRequestId

    -- * Response
    , DescribeSpotFleetInstancesResponse
    -- ** Response constructor
    , describeSpotFleetInstancesResponse
    -- ** Response lenses
    , dsfirActiveInstances
    , dsfirNextToken
    , dsfirSpotFleetRequestId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeSpotFleetInstances = DescribeSpotFleetInstances
    { _dsfiDryRun             :: Maybe Bool
    , _dsfiMaxResults         :: Maybe Int
    , _dsfiNextToken          :: Maybe Text
    , _dsfiSpotFleetRequestId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeSpotFleetInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfiDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dsfiMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dsfiNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsfiSpotFleetRequestId' @::@ 'Text'
--
describeSpotFleetInstances :: Text -- ^ 'dsfiSpotFleetRequestId'
                           -> DescribeSpotFleetInstances
describeSpotFleetInstances p1 = DescribeSpotFleetInstances
    { _dsfiSpotFleetRequestId = p1
    , _dsfiDryRun             = Nothing
    , _dsfiNextToken          = Nothing
    , _dsfiMaxResults         = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dsfiDryRun :: Lens' DescribeSpotFleetInstances (Maybe Bool)
dsfiDryRun = lens _dsfiDryRun (\s a -> s { _dsfiDryRun = a })

-- | The maximum number of results to return in a single call. Specify a value
-- between 1 and 1000. The default value is 1000. To retrieve the remaining
-- results, make another call with the returned 'NextToken' value.
dsfiMaxResults :: Lens' DescribeSpotFleetInstances (Maybe Int)
dsfiMaxResults = lens _dsfiMaxResults (\s a -> s { _dsfiMaxResults = a })

-- | The token for the next set of results.
dsfiNextToken :: Lens' DescribeSpotFleetInstances (Maybe Text)
dsfiNextToken = lens _dsfiNextToken (\s a -> s { _dsfiNextToken = a })

-- | The ID of the Spot fleet request.
dsfiSpotFleetRequestId :: Lens' DescribeSpotFleetInstances Text
dsfiSpotFleetRequestId =
    lens _dsfiSpotFleetRequestId (\s a -> s { _dsfiSpotFleetRequestId = a })

data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse
    { _dsfirActiveInstances    :: List "item" ActiveInstance
    , _dsfirNextToken          :: Maybe Text
    , _dsfirSpotFleetRequestId :: Text
    } deriving (Eq, Read, Show)

-- | 'DescribeSpotFleetInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfirActiveInstances' @::@ ['ActiveInstance']
--
-- * 'dsfirNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsfirSpotFleetRequestId' @::@ 'Text'
--
describeSpotFleetInstancesResponse :: Text -- ^ 'dsfirSpotFleetRequestId'
                                   -> DescribeSpotFleetInstancesResponse
describeSpotFleetInstancesResponse p1 = DescribeSpotFleetInstancesResponse
    { _dsfirSpotFleetRequestId = p1
    , _dsfirActiveInstances    = mempty
    , _dsfirNextToken          = Nothing
    }

-- | The running instances. Note that this list is refreshed periodically and
-- might be out of date.
dsfirActiveInstances :: Lens' DescribeSpotFleetInstancesResponse [ActiveInstance]
dsfirActiveInstances =
    lens _dsfirActiveInstances (\s a -> s { _dsfirActiveInstances = a })
        . _List

-- | The token required to retrieve the next set of results. This value is 'null'
-- when there are no more results to return.
dsfirNextToken :: Lens' DescribeSpotFleetInstancesResponse (Maybe Text)
dsfirNextToken = lens _dsfirNextToken (\s a -> s { _dsfirNextToken = a })

-- | The ID of the Spot fleet request.
dsfirSpotFleetRequestId :: Lens' DescribeSpotFleetInstancesResponse Text
dsfirSpotFleetRequestId =
    lens _dsfirSpotFleetRequestId (\s a -> s { _dsfirSpotFleetRequestId = a })

instance ToPath DescribeSpotFleetInstances where
    toPath = const "/"

instance ToQuery DescribeSpotFleetInstances where
    toQuery DescribeSpotFleetInstances{..} = mconcat
        [ "DryRun"             =? _dsfiDryRun
        , "MaxResults"         =? _dsfiMaxResults
        , "NextToken"          =? _dsfiNextToken
        , "SpotFleetRequestId" =? _dsfiSpotFleetRequestId
        ]

instance ToHeaders DescribeSpotFleetInstances

instance AWSRequest DescribeSpotFleetInstances where
    type Sv DescribeSpotFleetInstances = EC2
    type Rs DescribeSpotFleetInstances = DescribeSpotFleetInstancesResponse

    request  = post "DescribeSpotFleetInstances"
    response = xmlResponse

instance FromXML DescribeSpotFleetInstancesResponse where
    parseXML x = DescribeSpotFleetInstancesResponse
        <$> x .@? "activeInstanceSet" .!@ mempty
        <*> x .@? "nextToken"
        <*> x .@  "spotFleetRequestId"
