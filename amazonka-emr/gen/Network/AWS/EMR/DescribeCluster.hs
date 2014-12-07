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

-- Module      : Network.AWS.EMR.DescribeCluster
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

-- | Provides cluster-level details including status, hardware and software
-- configuration, VPC settings, and so on. For information about the cluster
-- steps, see 'ListSteps'.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_DescribeCluster.html>
module Network.AWS.EMR.DescribeCluster
    (
    -- * Request
      DescribeCluster
    -- ** Request constructor
    , describeCluster
    -- ** Request lenses
    , dcClusterId

    -- * Response
    , DescribeClusterResponse
    -- ** Response constructor
    , describeClusterResponse
    -- ** Response lenses
    , dcrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

newtype DescribeCluster = DescribeCluster
    { _dcClusterId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DescribeCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcClusterId' @::@ 'Text'
--
describeCluster :: Text -- ^ 'dcClusterId'
                -> DescribeCluster
describeCluster p1 = DescribeCluster
    { _dcClusterId = p1
    }

-- | The identifier of the cluster to describe.
dcClusterId :: Lens' DescribeCluster Text
dcClusterId = lens _dcClusterId (\s a -> s { _dcClusterId = a })

newtype DescribeClusterResponse = DescribeClusterResponse
    { _dcrCluster :: Cluster
    } deriving (Eq, Show)

-- | 'DescribeClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrCluster' @::@ 'Cluster'
--
describeClusterResponse :: Cluster -- ^ 'dcrCluster'
                        -> DescribeClusterResponse
describeClusterResponse p1 = DescribeClusterResponse
    { _dcrCluster = p1
    }

-- | This output contains the details for the requested cluster.
dcrCluster :: Lens' DescribeClusterResponse Cluster
dcrCluster = lens _dcrCluster (\s a -> s { _dcrCluster = a })

instance ToPath DescribeCluster where
    toPath = const "/"

instance ToQuery DescribeCluster where
    toQuery = const mempty

instance ToHeaders DescribeCluster

instance ToJSON DescribeCluster where
    toJSON DescribeCluster{..} = object
        [ "ClusterId" .= _dcClusterId
        ]

instance AWSRequest DescribeCluster where
    type Sv DescribeCluster = EMR
    type Rs DescribeCluster = DescribeClusterResponse

    request  = post "DescribeCluster"
    response = jsonResponse

instance FromJSON DescribeClusterResponse where
    parseJSON = withObject "DescribeClusterResponse" $ \o -> DescribeClusterResponse
        <$> o .:  "Cluster"
