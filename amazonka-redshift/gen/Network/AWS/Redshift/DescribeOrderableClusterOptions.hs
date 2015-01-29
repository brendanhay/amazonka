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

-- Module      : Network.AWS.Redshift.DescribeOrderableClusterOptions
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

-- | Returns a list of orderable cluster options. Before you create a new cluster
-- you can use this operation to find what options are available, such as the
-- EC2 Availability Zones (AZ) in the specific AWS region that you can specify,
-- and the node types you can request. The node types differ by available
-- storage, memory, CPU and price. With the cost involved you might want to
-- obtain a list of cluster options in the specific region and specify values
-- when creating a cluster. For more information about managing clusters, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeOrderableClusterOptions.html>
module Network.AWS.Redshift.DescribeOrderableClusterOptions
    (
    -- * Request
      DescribeOrderableClusterOptions
    -- ** Request constructor
    , describeOrderableClusterOptions
    -- ** Request lenses
    , docoClusterVersion
    , docoMarker
    , docoMaxRecords
    , docoNodeType

    -- * Response
    , DescribeOrderableClusterOptionsResponse
    -- ** Response constructor
    , describeOrderableClusterOptionsResponse
    -- ** Response lenses
    , docorMarker
    , docorOrderableClusterOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions
    { _docoClusterVersion :: Maybe Text
    , _docoMarker         :: Maybe Text
    , _docoMaxRecords     :: Maybe Int
    , _docoNodeType       :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeOrderableClusterOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'docoClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'docoMarker' @::@ 'Maybe' 'Text'
--
-- * 'docoMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'docoNodeType' @::@ 'Maybe' 'Text'
--
describeOrderableClusterOptions :: DescribeOrderableClusterOptions
describeOrderableClusterOptions = DescribeOrderableClusterOptions
    { _docoClusterVersion = Nothing
    , _docoNodeType       = Nothing
    , _docoMaxRecords     = Nothing
    , _docoMarker         = Nothing
    }

-- | The version filter value. Specify this parameter to show only the available
-- offerings matching the specified version.
--
-- Default: All versions.
--
-- Constraints: Must be one of the version returned from 'DescribeClusterVersions'
-- .
docoClusterVersion :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoClusterVersion =
    lens _docoClusterVersion (\s a -> s { _docoClusterVersion = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a 'DescribeOrderableClusterOptions'
-- request exceed the value specified in 'MaxRecords', AWS returns a value in the 'Marker' field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the 'Marker' parameter and retrying the
-- request.
docoMarker :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoMarker = lens _docoMarker (\s a -> s { _docoMarker = a })

-- | The maximum number of response records to return in each call. If the number
-- of remaining response records exceeds the specified 'MaxRecords' value, a value
-- is returned in a 'marker' field of the response. You can retrieve the next set
-- of records by retrying the command with the returned marker value.
--
-- Default: '100'
--
-- Constraints: minimum 20, maximum 100.
docoMaxRecords :: Lens' DescribeOrderableClusterOptions (Maybe Int)
docoMaxRecords = lens _docoMaxRecords (\s a -> s { _docoMaxRecords = a })

-- | The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
docoNodeType :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoNodeType = lens _docoNodeType (\s a -> s { _docoNodeType = a })

data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse
    { _docorMarker                  :: Maybe Text
    , _docorOrderableClusterOptions :: List "member" OrderableClusterOption
    } deriving (Eq, Read, Show)

-- | 'DescribeOrderableClusterOptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'docorMarker' @::@ 'Maybe' 'Text'
--
-- * 'docorOrderableClusterOptions' @::@ ['OrderableClusterOption']
--
describeOrderableClusterOptionsResponse :: DescribeOrderableClusterOptionsResponse
describeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse
    { _docorOrderableClusterOptions = mempty
    , _docorMarker                  = Nothing
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker value
-- in the 'Marker' parameter and retrying the command. If the 'Marker' field is
-- empty, all response records have been retrieved for the request.
docorMarker :: Lens' DescribeOrderableClusterOptionsResponse (Maybe Text)
docorMarker = lens _docorMarker (\s a -> s { _docorMarker = a })

-- | An 'OrderableClusterOption' structure containing information about orderable
-- options for the Cluster.
docorOrderableClusterOptions :: Lens' DescribeOrderableClusterOptionsResponse [OrderableClusterOption]
docorOrderableClusterOptions =
    lens _docorOrderableClusterOptions
        (\s a -> s { _docorOrderableClusterOptions = a })
            . _List

instance ToPath DescribeOrderableClusterOptions where
    toPath = const "/"

instance ToQuery DescribeOrderableClusterOptions where
    toQuery DescribeOrderableClusterOptions{..} = mconcat
        [ "ClusterVersion" =? _docoClusterVersion
        , "Marker"         =? _docoMarker
        , "MaxRecords"     =? _docoMaxRecords
        , "NodeType"       =? _docoNodeType
        ]

instance ToHeaders DescribeOrderableClusterOptions

instance AWSRequest DescribeOrderableClusterOptions where
    type Sv DescribeOrderableClusterOptions = Redshift
    type Rs DescribeOrderableClusterOptions = DescribeOrderableClusterOptionsResponse

    request  = post "DescribeOrderableClusterOptions"
    response = xmlResponse

instance FromXML DescribeOrderableClusterOptionsResponse where
    parseXML = withElement "DescribeOrderableClusterOptionsResult" $ \x -> DescribeOrderableClusterOptionsResponse
        <$> x .@? "Marker"
        <*> x .@? "OrderableClusterOptions" .!@ mempty

instance AWSPager DescribeOrderableClusterOptions where
    page rq rs
        | stop (rs ^. docorMarker) = Nothing
        | otherwise = (\x -> rq & docoMarker ?~ x)
            <$> (rs ^. docorMarker)
