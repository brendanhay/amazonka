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

-- Module      : Network.AWS.Redshift.DescribeClusterVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions of the available Amazon Redshift cluster versions. You
-- can call this operation even before creating any clusters to learn more
-- about the Amazon Redshift versions. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide.
module Network.AWS.Redshift.DescribeClusterVersions
    (
    -- * Request
      DescribeClusterVersionsMessage
    -- ** Request constructor
    , describeClusterVersionsMessage
    -- ** Request lenses
    , dcvmClusterParameterGroupFamily
    , dcvmClusterVersion
    , dcvmMarker
    , dcvmMaxRecords

    -- * Response
    , ClusterVersionsMessage
    -- ** Response constructor
    , clusterVersionsMessage
    -- ** Response lenses
    , cvmClusterVersions
    , cvmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeClusterVersionsMessage = DescribeClusterVersionsMessage
    { _dcvmClusterParameterGroupFamily :: Maybe Text
    , _dcvmClusterVersion              :: Maybe Text
    , _dcvmMarker                      :: Maybe Text
    , _dcvmMaxRecords                  :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusterVersionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcvmClusterParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'dcvmClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'dcvmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcvmMaxRecords' @::@ 'Maybe' 'Int'
--
describeClusterVersionsMessage :: DescribeClusterVersionsMessage
describeClusterVersionsMessage = DescribeClusterVersionsMessage
    { _dcvmClusterVersion              = Nothing
    , _dcvmClusterParameterGroupFamily = Nothing
    , _dcvmMaxRecords                  = Nothing
    , _dcvmMarker                      = Nothing
    }

-- | The name of a specific cluster parameter group family to return details
-- for. Constraints: Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
dcvmClusterParameterGroupFamily :: Lens' DescribeClusterVersionsMessage (Maybe Text)
dcvmClusterParameterGroupFamily =
    lens _dcvmClusterParameterGroupFamily
        (\s a -> s { _dcvmClusterParameterGroupFamily = a })

-- | The specific cluster version to return. Example: 1.0.
dcvmClusterVersion :: Lens' DescribeClusterVersionsMessage (Maybe Text)
dcvmClusterVersion =
    lens _dcvmClusterVersion (\s a -> s { _dcvmClusterVersion = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterVersions
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dcvmMarker :: Lens' DescribeClusterVersionsMessage (Maybe Text)
dcvmMarker = lens _dcvmMarker (\s a -> s { _dcvmMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcvmMaxRecords :: Lens' DescribeClusterVersionsMessage (Maybe Int)
dcvmMaxRecords = lens _dcvmMaxRecords (\s a -> s { _dcvmMaxRecords = a })
instance ToQuery DescribeClusterVersionsMessage

instance ToPath DescribeClusterVersionsMessage where
    toPath = const "/"

data ClusterVersionsMessage = ClusterVersionsMessage
    { _cvmClusterVersions :: [ClusterVersion]
    , _cvmMarker          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ClusterVersionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvmClusterVersions' @::@ ['ClusterVersion']
--
-- * 'cvmMarker' @::@ 'Maybe' 'Text'
--
clusterVersionsMessage :: ClusterVersionsMessage
clusterVersionsMessage = ClusterVersionsMessage
    { _cvmMarker          = Nothing
    , _cvmClusterVersions = mempty
    }

-- | A list of Version elements.
cvmClusterVersions :: Lens' ClusterVersionsMessage [ClusterVersion]
cvmClusterVersions =
    lens _cvmClusterVersions (\s a -> s { _cvmClusterVersions = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
cvmMarker :: Lens' ClusterVersionsMessage (Maybe Text)
cvmMarker = lens _cvmMarker (\s a -> s { _cvmMarker = a })
instance FromXML ClusterVersionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ClusterVersionsMessage"

instance AWSRequest DescribeClusterVersionsMessage where
    type Sv DescribeClusterVersionsMessage = Redshift
    type Rs DescribeClusterVersionsMessage = ClusterVersionsMessage

    request  = post "DescribeClusterVersions"
    response = xmlResponse $ \h x -> ClusterVersionsMessage
        <$> x %| "ClusterVersions"
        <*> x %| "Marker"
