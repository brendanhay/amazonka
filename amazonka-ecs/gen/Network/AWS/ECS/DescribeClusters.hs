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

-- Module      : Network.AWS.ECS.DescribeClusters
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

-- | Describes one or more of your clusters.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeClusters.html>
module Network.AWS.ECS.DescribeClusters
    (
    -- * Request
      DescribeClusters
    -- ** Request constructor
    , describeClusters
    -- ** Request lenses
    , dcClusters

    -- * Response
    , DescribeClustersResponse
    -- ** Response constructor
    , describeClustersResponse
    -- ** Response lenses
    , dcrClusters
    , dcrFailures
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

newtype DescribeClusters = DescribeClusters
    { _dcClusters :: List "clusters" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeClusters where
    type Item DescribeClusters = Text

    fromList = DescribeClusters . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dcClusters

-- | 'DescribeClusters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcClusters' @::@ ['Text']
--
describeClusters :: DescribeClusters
describeClusters = DescribeClusters
    { _dcClusters = mempty
    }

-- | A space-separated list of cluster names or full cluster Amazon Resource Name
-- (ARN) entries. If you do not specify a cluster, the default cluster is
-- assumed.
dcClusters :: Lens' DescribeClusters [Text]
dcClusters = lens _dcClusters (\s a -> s { _dcClusters = a }) . _List

data DescribeClustersResponse = DescribeClustersResponse
    { _dcrClusters :: List "clusters" Cluster
    , _dcrFailures :: List "failures" Failure
    } deriving (Eq, Read, Show)

-- | 'DescribeClustersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrClusters' @::@ ['Cluster']
--
-- * 'dcrFailures' @::@ ['Failure']
--
describeClustersResponse :: DescribeClustersResponse
describeClustersResponse = DescribeClustersResponse
    { _dcrClusters = mempty
    , _dcrFailures = mempty
    }

-- | The list of clusters.
dcrClusters :: Lens' DescribeClustersResponse [Cluster]
dcrClusters = lens _dcrClusters (\s a -> s { _dcrClusters = a }) . _List

dcrFailures :: Lens' DescribeClustersResponse [Failure]
dcrFailures = lens _dcrFailures (\s a -> s { _dcrFailures = a }) . _List

instance ToPath DescribeClusters where
    toPath = const "/"

instance ToQuery DescribeClusters where
    toQuery = const mempty

instance ToHeaders DescribeClusters

instance ToJSON DescribeClusters where
    toJSON DescribeClusters{..} = object
        [ "clusters" .= _dcClusters
        ]

instance AWSRequest DescribeClusters where
    type Sv DescribeClusters = ECS
    type Rs DescribeClusters = DescribeClustersResponse

    request  = post "DescribeClusters"
    response = jsonResponse

instance FromJSON DescribeClustersResponse where
    parseJSON = withObject "DescribeClustersResponse" $ \o -> DescribeClustersResponse
        <$> o .:? "clusters" .!= mempty
        <*> o .:? "failures" .!= mempty
