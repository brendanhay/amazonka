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

-- Module      : Network.AWS.Redshift.RebootCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Reboots a cluster. This action is taken as soon as possible. It results in
-- a momentary outage to the cluster, during which the cluster status is set
-- to rebooting. A cluster event is created when the reboot is completed. Any
-- pending cluster modifications (see ModifyCluster) are applied at this
-- reboot. For more information about managing clusters, go to Amazon Redshift
-- Clusters in the Amazon Redshift Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_RebootCluster.html>
module Network.AWS.Redshift.RebootCluster
    (
    -- * Request
      RebootCluster
    -- ** Request constructor
    , rebootCluster
    -- ** Request lenses
    , rcClusterIdentifier

    -- * Response
    , RebootClusterResponse
    -- ** Response constructor
    , rebootClusterResponse
    -- ** Response lenses
    , rcrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

newtype RebootCluster = RebootCluster
    { _rcClusterIdentifier :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'RebootCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcClusterIdentifier' @::@ 'Text'
--
rebootCluster :: Text -- ^ 'rcClusterIdentifier'
              -> RebootCluster
rebootCluster p1 = RebootCluster
    { _rcClusterIdentifier = p1
    }

-- | The cluster identifier.
rcClusterIdentifier :: Lens' RebootCluster Text
rcClusterIdentifier =
    lens _rcClusterIdentifier (\s a -> s { _rcClusterIdentifier = a })

newtype RebootClusterResponse = RebootClusterResponse
    { _rcrCluster :: Maybe Cluster
    } deriving (Eq, Show)

-- | 'RebootClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcrCluster' @::@ 'Maybe' 'Cluster'
--
rebootClusterResponse :: RebootClusterResponse
rebootClusterResponse = RebootClusterResponse
    { _rcrCluster = Nothing
    }

rcrCluster :: Lens' RebootClusterResponse (Maybe Cluster)
rcrCluster = lens _rcrCluster (\s a -> s { _rcrCluster = a })

instance ToPath RebootCluster where
    toPath = const "/"

instance ToQuery RebootCluster where
    toQuery RebootCluster{..} = mconcat
        [ "ClusterIdentifier" =? _rcClusterIdentifier
        ]

instance ToHeaders RebootCluster

instance AWSRequest RebootCluster where
    type Sv RebootCluster = Redshift
    type Rs RebootCluster = RebootClusterResponse

    request  = post "RebootCluster"
    response = xmlResponse

instance FromXML RebootClusterResponse where
    parseXML = withElement "RebootClusterResult" $ \x -> RebootClusterResponse
        <$> x .@? "Cluster"
