{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.DescribeClusters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , dcrFailures
    , dcrClusters
    ) where

import Network.AWS.ECS.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClusters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcClusters'
newtype DescribeClusters = DescribeClusters'{_dcClusters :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeClusters' smart constructor.
describeClusters :: DescribeClusters
describeClusters = DescribeClusters'{_dcClusters = Nothing};

-- | A space-separated list of cluster names or full cluster Amazon Resource
-- Name (ARN) entries. If you do not specify a cluster, the default cluster
-- is assumed.
dcClusters :: Lens' DescribeClusters [Text]
dcClusters = lens _dcClusters (\ s a -> s{_dcClusters = a}) . _Default;

instance AWSRequest DescribeClusters where
        type Sv DescribeClusters = ECS
        type Rs DescribeClusters = DescribeClustersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeClustersResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "clusters" .!@ mempty))

instance ToHeaders DescribeClusters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DescribeClusters"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeClusters where
        toJSON DescribeClusters'{..}
          = object ["clusters" .= _dcClusters]

instance ToPath DescribeClusters where
        toPath = const "/"

instance ToQuery DescribeClusters where
        toQuery = const mempty

-- | /See:/ 'describeClustersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrFailures'
--
-- * 'dcrClusters'
data DescribeClustersResponse = DescribeClustersResponse'{_dcrFailures :: Maybe [Failure], _dcrClusters :: Maybe [Cluster]} deriving (Eq, Read, Show)

-- | 'DescribeClustersResponse' smart constructor.
describeClustersResponse :: DescribeClustersResponse
describeClustersResponse = DescribeClustersResponse'{_dcrFailures = Nothing, _dcrClusters = Nothing};

-- | FIXME: Undocumented member.
dcrFailures :: Lens' DescribeClustersResponse [Failure]
dcrFailures = lens _dcrFailures (\ s a -> s{_dcrFailures = a}) . _Default;

-- | The list of clusters.
dcrClusters :: Lens' DescribeClustersResponse [Cluster]
dcrClusters = lens _dcrClusters (\ s a -> s{_dcrClusters = a}) . _Default;
