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
      DescribeClustersMessage
    -- ** Request constructor
    , describeClusters
    -- ** Request lenses
    , dcm1ClusterIdentifier
    , dcm1Marker
    , dcm1MaxRecords

    -- * Response
    , ClustersMessage
    -- ** Response constructor
    , describeClustersResponse
    -- ** Response lenses
    , cmClusters
    , cmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeClustersMessage = DescribeClustersMessage
    { _dcm1ClusterIdentifier :: Maybe Text
    , _dcm1Marker            :: Maybe Text
    , _dcm1MaxRecords        :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClustersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcm1ClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcm1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcm1MaxRecords' @::@ 'Maybe' 'Int'
--
describeClusters :: DescribeClustersMessage
describeClusters = DescribeClustersMessage
    { _dcm1ClusterIdentifier = Nothing
    , _dcm1MaxRecords        = Nothing
    , _dcm1Marker            = Nothing
    }

-- | The unique identifier of a cluster whose properties you are requesting.
-- This parameter is case sensitive. The default is that all clusters
-- defined for an account are returned.
dcm1ClusterIdentifier :: Lens' DescribeClustersMessage (Maybe Text)
dcm1ClusterIdentifier =
    lens _dcm1ClusterIdentifier (\s a -> s { _dcm1ClusterIdentifier = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusters request
-- exceed the value specified in MaxRecords, AWS returns a value in the
-- Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter
-- and retrying the request. Constraints: You can specify either the
-- ClusterIdentifier parameter or the Marker parameter, but not both.
dcm1Marker :: Lens' DescribeClustersMessage (Maybe Text)
dcm1Marker = lens _dcm1Marker (\s a -> s { _dcm1Marker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcm1MaxRecords :: Lens' DescribeClustersMessage (Maybe Int)
dcm1MaxRecords = lens _dcm1MaxRecords (\s a -> s { _dcm1MaxRecords = a })

instance ToPath DescribeClustersMessage where
    toPath = const "/"

instance ToQuery DescribeClustersMessage

data ClustersMessage = ClustersMessage
    { _cmClusters :: [Cluster]
    , _cmMarker   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ClustersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmClusters' @::@ ['Cluster']
--
-- * 'cmMarker' @::@ 'Maybe' 'Text'
--
describeClustersResponse :: ClustersMessage
describeClustersResponse = ClustersMessage
    { _cmMarker   = Nothing
    , _cmClusters = mempty
    }

-- | A list of Cluster objects, where each object describes one cluster.
cmClusters :: Lens' ClustersMessage [Cluster]
cmClusters = lens _cmClusters (\s a -> s { _cmClusters = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
cmMarker :: Lens' ClustersMessage (Maybe Text)
cmMarker = lens _cmMarker (\s a -> s { _cmMarker = a })

instance AWSRequest DescribeClustersMessage where
    type Sv DescribeClustersMessage = Redshift
    type Rs DescribeClustersMessage = ClustersMessage

    request  = post "DescribeClusters"
    response = xmlResponse $ \h x -> ClustersMessage
        <$> x %| "Clusters"
        <*> x %| "Marker"
