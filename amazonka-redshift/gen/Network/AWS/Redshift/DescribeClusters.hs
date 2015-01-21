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

-- Module      : Network.AWS.Redshift.DescribeClusters
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

-- | Returns properties of provisioned clusters including general cluster
-- properties, cluster database properties, maintenance and backup properties,
-- and security and access properties. This operation supports pagination. For
-- more information about managing clusters, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in
-- the /Amazon Redshift Cluster Management Guide/ .
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all clusters that match any combination of the specified
-- keys and values. For example, if you have 'owner' and 'environment' for tag keys,
-- and 'admin' and 'test' for tag values, all clusters that have any combination of
-- those values are returned.
--
-- If both tag keys and values are omitted from the request, clusters are
-- returned regardless of whether they have tag keys or values associated with
-- them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusters.html>
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
    , dcTagKeys
    , dcTagValues

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
    , _dcTagKeys           :: List "member" Text
    , _dcTagValues         :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

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
-- * 'dcTagKeys' @::@ ['Text']
--
-- * 'dcTagValues' @::@ ['Text']
--
describeClusters :: DescribeClusters
describeClusters = DescribeClusters
    { _dcClusterIdentifier = Nothing
    , _dcMaxRecords        = Nothing
    , _dcMarker            = Nothing
    , _dcTagKeys           = mempty
    , _dcTagValues         = mempty
    }

-- | The unique identifier of a cluster whose properties you are requesting. This
-- parameter is case sensitive.
--
-- The default is that all clusters defined for an account are returned.
dcClusterIdentifier :: Lens' DescribeClusters (Maybe Text)
dcClusterIdentifier =
    lens _dcClusterIdentifier (\s a -> s { _dcClusterIdentifier = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a 'DescribeClusters' request exceed the
-- value specified in 'MaxRecords', AWS returns a value in the 'Marker' field of the
-- response. You can retrieve the next set of response records by providing the
-- returned marker value in the 'Marker' parameter and retrying the request.
--
-- Constraints: You can specify either the ClusterIdentifier parameter or the Marker
-- parameter, but not both.
dcMarker :: Lens' DescribeClusters (Maybe Text)
dcMarker = lens _dcMarker (\s a -> s { _dcMarker = a })

-- | The maximum number of response records to return in each call. If the number
-- of remaining response records exceeds the specified 'MaxRecords' value, a value
-- is returned in a 'marker' field of the response. You can retrieve the next set
-- of records by retrying the command with the returned marker value.
--
-- Default: '100'
--
-- Constraints: minimum 20, maximum 100.
dcMaxRecords :: Lens' DescribeClusters (Maybe Int)
dcMaxRecords = lens _dcMaxRecords (\s a -> s { _dcMaxRecords = a })

-- | A tag key or keys for which you want to return all matching clusters that are
-- associated with the specified key or keys. For example, suppose that you have
-- clusters that are tagged with keys called 'owner' and 'environment'. If you
-- specify both of these tag keys in the request, Amazon Redshift returns a
-- response with the clusters that have either or both of these tag keys
-- associated with them.
dcTagKeys :: Lens' DescribeClusters [Text]
dcTagKeys = lens _dcTagKeys (\s a -> s { _dcTagKeys = a }) . _List

-- | A tag value or values for which you want to return all matching clusters that
-- are associated with the specified tag value or values. For example, suppose
-- that you have clusters that are tagged with values called 'admin' and 'test'. If
-- you specify both of these tag values in the request, Amazon Redshift returns
-- a response with the clusters that have either or both of these tag values
-- associated with them.
dcTagValues :: Lens' DescribeClusters [Text]
dcTagValues = lens _dcTagValues (\s a -> s { _dcTagValues = a }) . _List

data DescribeClustersResponse = DescribeClustersResponse
    { _dcrClusters :: List "member" Cluster
    , _dcrMarker   :: Maybe Text
    } deriving (Eq, Read, Show)

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

-- | A list of 'Cluster' objects, where each object describes one cluster.
dcrClusters :: Lens' DescribeClustersResponse [Cluster]
dcrClusters = lens _dcrClusters (\s a -> s { _dcrClusters = a }) . _List

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker value
-- in the 'Marker' parameter and retrying the command. If the 'Marker' field is
-- empty, all response records have been retrieved for the request.
dcrMarker :: Lens' DescribeClustersResponse (Maybe Text)
dcrMarker = lens _dcrMarker (\s a -> s { _dcrMarker = a })

instance ToPath DescribeClusters where
    toPath = const "/"

instance ToQuery DescribeClusters where
    toQuery DescribeClusters{..} = mconcat
        [ "ClusterIdentifier" =? _dcClusterIdentifier
        , "Marker"            =? _dcMarker
        , "MaxRecords"        =? _dcMaxRecords
        , "TagKeys"           =? _dcTagKeys
        , "TagValues"         =? _dcTagValues
        ]

instance ToHeaders DescribeClusters

instance AWSRequest DescribeClusters where
    type Sv DescribeClusters = Redshift
    type Rs DescribeClusters = DescribeClustersResponse

    request  = post "DescribeClusters"
    response = xmlResponse

instance FromXML DescribeClustersResponse where
    parseXML = withElement "DescribeClustersResult" $ \x -> DescribeClustersResponse
        <$> x .@? "Clusters" .!@ mempty
        <*> x .@? "Marker"

instance AWSPager DescribeClusters where
    page rq rs
        | stop (rq ^. dcMarker) = Nothing
        | otherwise = (\x -> rq & dcMarker ?~ x)
            <$> (rs ^. dcrMarker)
