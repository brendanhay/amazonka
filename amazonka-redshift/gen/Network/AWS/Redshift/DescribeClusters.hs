{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.Redshift.DescribeClusters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns properties of provisioned clusters including general cluster
-- properties, cluster database properties, maintenance and backup properties,
-- and security and access properties. This operation supports pagination. For
-- more information about managing clusters, go to Amazon Redshift Clusters in
-- the Amazon Redshift Management Guide .
module Network.AWS.Redshift.DescribeClusters
    (
    -- * Request
      DescribeClusters
    -- ** Request constructor
    , describeClusters
    -- ** Request lenses
    , dcClusterIdentifier
    , dcMarker
    , dcMaxRecords

    -- * Response
    , DescribeClustersResponse
    -- ** Response constructor
    , describeClustersResponse
    -- ** Response lenses
    , dcrClusters
    , dcrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeClusters = DescribeClusters
    { _dcClusterIdentifier :: Maybe Text
    , _dcMarker            :: Maybe Text
    , _dcMaxRecords        :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcMaxRecords' @::@ 'Maybe' 'Int'
--
describeClusters :: DescribeClusters
describeClusters = DescribeClusters
    { _dcClusterIdentifier = Nothing
    , _dcMaxRecords        = Nothing
    , _dcMarker            = Nothing
    }

-- | The unique identifier of a cluster whose properties you are requesting.
-- This parameter is case sensitive. The default is that all clusters
-- defined for an account are returned.
dcClusterIdentifier :: Lens' DescribeClusters (Maybe Text)
dcClusterIdentifier =
    lens _dcClusterIdentifier (\s a -> s { _dcClusterIdentifier = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusters request
-- exceed the value specified in MaxRecords, AWS returns a value in the
-- Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter
-- and retrying the request. Constraints: You can specify either the
-- ClusterIdentifier parameter or the Marker parameter, but not both.
dcMarker :: Lens' DescribeClusters (Maybe Text)
dcMarker = lens _dcMarker (\s a -> s { _dcMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcMaxRecords :: Lens' DescribeClusters (Maybe Int)
dcMaxRecords = lens _dcMaxRecords (\s a -> s { _dcMaxRecords = a })

instance ToQuery DescribeClusters

instance ToPath DescribeClusters where
    toPath = const "/"

data DescribeClustersResponse = DescribeClustersResponse
    { _dcrClusters :: [Cluster]
    , _dcrMarker   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeClustersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrClusters' @::@ ['Cluster']
--
-- * 'dcrMarker' @::@ 'Maybe' 'Text'
--
describeClustersResponse :: DescribeClustersResponse
describeClustersResponse = DescribeClustersResponse
    { _dcrMarker   = Nothing
    , _dcrClusters = mempty
    }

-- | A list of Cluster objects, where each object describes one cluster.
dcrClusters :: Lens' DescribeClustersResponse [Cluster]
dcrClusters = lens _dcrClusters (\s a -> s { _dcrClusters = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
dcrMarker :: Lens' DescribeClustersResponse (Maybe Text)
dcrMarker = lens _dcrMarker (\s a -> s { _dcrMarker = a })

instance AWSRequest DescribeClusters where
    type Sv DescribeClusters = Redshift
    type Rs DescribeClusters = DescribeClustersResponse

    request  = post "DescribeClusters"
    response = xmlResponse $ \h x -> DescribeClustersResponse
        <$> x %| "Clusters"
        <*> x %| "Marker"

instance AWSPager DescribeClusters where
    next rq rs = (\x -> rq & dcMarker ?~ x)
        <$> (rs ^. dcrMarker)
