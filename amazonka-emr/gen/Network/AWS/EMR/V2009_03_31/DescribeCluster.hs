{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.DescribeCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides cluster-level details including status, hardware and software
-- configuration, VPC settings, and so on. For information about the cluster
-- steps, see ListSteps.
module Network.AWS.EMR.V2009_03_31.DescribeCluster
    (
    -- * Request
      DescribeCluster
    -- ** Request constructor
    , describeCluster
    -- ** Request lenses
    , dciClusterId

    -- * Response
    , DescribeClusterResponse
    -- ** Response lenses
    , dcoCluster
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeCluster' request.
describeCluster :: Text -- ^ 'dciClusterId'
                -> DescribeCluster
describeCluster p1 = DescribeCluster
    { _dciClusterId = p1
    }

data DescribeCluster = DescribeCluster
    { _dciClusterId :: Text
      -- ^ The identifier of the cluster to describe.
    } deriving (Show, Generic)

-- | The identifier of the cluster to describe.
dciClusterId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeCluster
    -> f DescribeCluster
dciClusterId f x =
    (\y -> x { _dciClusterId = y })
       <$> f (_dciClusterId x)
{-# INLINE dciClusterId #-}

instance ToPath DescribeCluster

instance ToQuery DescribeCluster

instance ToHeaders DescribeCluster

instance ToJSON DescribeCluster

data DescribeClusterResponse = DescribeClusterResponse
    { _dcoCluster :: Maybe Cluster
      -- ^ This output contains the details for the requested cluster.
    } deriving (Show, Generic)

-- | This output contains the details for the requested cluster.
dcoCluster
    :: Functor f
    => (Maybe Cluster
    -> f (Maybe Cluster))
    -> DescribeClusterResponse
    -> f DescribeClusterResponse
dcoCluster f x =
    (\y -> x { _dcoCluster = y })
       <$> f (_dcoCluster x)
{-# INLINE dcoCluster #-}

instance FromJSON DescribeClusterResponse

instance AWSRequest DescribeCluster where
    type Sv DescribeCluster = EMR
    type Rs DescribeCluster = DescribeClusterResponse

    request = get
    response _ = jsonResponse
