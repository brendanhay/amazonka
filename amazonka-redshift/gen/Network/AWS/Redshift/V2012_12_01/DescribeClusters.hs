{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeClusters
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
-- the Amazon Redshift Management Guide . Describing All Clusters The
-- following example shows a request that describes all clusters.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeClusters
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T000452Z
-- &x-amz-signedheaders=content-type;host;x-amz-date **** 1.0 creating 2 1
-- true false dev sun:10:30-sun:11:00 in-sync default.redshift-1.0 active
-- default us-east-1a dw1.xlarge examplecluster true masteruser
-- 837d45d6-64f0-11e2-b07c-f7fbdd006c67.
module Network.AWS.Redshift.V2012_12_01.DescribeClusters
    (
    -- * Request
      DescribeClusters
    -- ** Request constructor
    , mkDescribeClustersMessage
    -- ** Request lenses
    , dcnClusterIdentifier
    , dcnMaxRecords
    , dcnMarker

    -- * Response
    , DescribeClustersResponse
    -- ** Response lenses
    , cmMarker
    , cmClusters
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusters' request.
mkDescribeClustersMessage :: DescribeClusters
mkDescribeClustersMessage = DescribeClusters
    { _dcnClusterIdentifier = Nothing
    , _dcnMaxRecords = Nothing
    , _dcnMarker = Nothing
    }
{-# INLINE mkDescribeClustersMessage #-}

data DescribeClusters = DescribeClusters
    { _dcnClusterIdentifier :: Maybe Text
      -- ^ The unique identifier of a cluster whose properties you are
      -- requesting. This parameter is case sensitive. The default is that
      -- all clusters defined for an account are returned.
    , _dcnMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dcnMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a DescribeClusters
      -- request exceed the value specified in MaxRecords, AWS returns a
      -- value in the Marker field of the response. You can retrieve the
      -- next set of response records by providing the returned marker
      -- value in the Marker parameter and retrying the request.
      -- Constraints: You can specify either the ClusterIdentifier
      -- parameter or the Marker parameter, but not both.
    } deriving (Show, Generic)

-- | The unique identifier of a cluster whose properties you are requesting.
-- This parameter is case sensitive. The default is that all clusters defined
-- for an account are returned.
dcnClusterIdentifier :: Lens' DescribeClusters (Maybe Text)
dcnClusterIdentifier = lens _dcnClusterIdentifier (\s a -> s { _dcnClusterIdentifier = a })
{-# INLINE dcnClusterIdentifier #-}

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcnMaxRecords :: Lens' DescribeClusters (Maybe Integer)
dcnMaxRecords = lens _dcnMaxRecords (\s a -> s { _dcnMaxRecords = a })
{-# INLINE dcnMaxRecords #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusters request exceed the
-- value specified in MaxRecords, AWS returns a value in the Marker field of
-- the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request. Constraints: You can specify either the ClusterIdentifier
-- parameter or the Marker parameter, but not both.
dcnMarker :: Lens' DescribeClusters (Maybe Text)
dcnMarker = lens _dcnMarker (\s a -> s { _dcnMarker = a })
{-# INLINE dcnMarker #-}

instance ToQuery DescribeClusters where
    toQuery = genericQuery def

data DescribeClustersResponse = DescribeClustersResponse
    { _cmMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    , _cmClusters :: [Cluster]
      -- ^ A list of Cluster objects, where each object describes one
      -- cluster.
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
cmMarker :: Lens' DescribeClustersResponse (Maybe Text)
cmMarker = lens _cmMarker (\s a -> s { _cmMarker = a })
{-# INLINE cmMarker #-}

-- | A list of Cluster objects, where each object describes one cluster.
cmClusters :: Lens' DescribeClustersResponse ([Cluster])
cmClusters = lens _cmClusters (\s a -> s { _cmClusters = a })
{-# INLINE cmClusters #-}

instance FromXML DescribeClustersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusters where
    type Sv DescribeClusters = Redshift
    type Rs DescribeClusters = DescribeClustersResponse

    request = post "DescribeClusters"
    response _ = xmlResponse

instance AWSPager DescribeClusters where
    next rq rs = (\x -> rq { _dcnMarker = Just x })
        <$> (_cmMarker rs)
