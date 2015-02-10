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

-- Module      : Network.AWS.ECS.CreateCluster
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

-- | Creates a new Amazon ECS cluster. By default, your account will receive a 'default' cluster when you launch your first container instance. However, you can
-- create your own cluster with a unique name with the 'CreateCluster' action.
--
-- During the preview, each account is limited to two clusters.
--
--
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateCluster.html>
module Network.AWS.ECS.CreateCluster
    (
    -- * Request
      CreateCluster
    -- ** Request constructor
    , createCluster
    -- ** Request lenses
    , ccClusterName

    -- * Response
    , CreateClusterResponse
    -- ** Response constructor
    , createClusterResponse
    -- ** Response lenses
    , ccrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ECS.Types
import qualified GHC.Exts

newtype CreateCluster = CreateCluster
    { _ccClusterName :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccClusterName' @::@ 'Maybe' 'Text'
--
createCluster :: CreateCluster
createCluster = CreateCluster
    { _ccClusterName = Nothing
    }

-- | The name of your cluster. If you do not specify a name for your cluster, you
-- will create a cluster named 'default'.
ccClusterName :: Lens' CreateCluster (Maybe Text)
ccClusterName = lens _ccClusterName (\s a -> s { _ccClusterName = a })

newtype CreateClusterResponse = CreateClusterResponse
    { _ccrCluster :: Maybe Cluster
    } deriving (Eq, Read, Show)

-- | 'CreateClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrCluster' @::@ 'Maybe' 'Cluster'
--
createClusterResponse :: CreateClusterResponse
createClusterResponse = CreateClusterResponse
    { _ccrCluster = Nothing
    }

-- | The full description of your new cluster.
ccrCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrCluster = lens _ccrCluster (\s a -> s { _ccrCluster = a })

instance ToPath CreateCluster where
    toPath = const "/"

instance ToQuery CreateCluster where
    toQuery CreateCluster{..} = mconcat
        [ "clusterName" =? _ccClusterName
        ]

instance ToHeaders CreateCluster

instance AWSRequest CreateCluster where
    type Sv CreateCluster = ECS
    type Rs CreateCluster = CreateClusterResponse

    request  = post "CreateCluster"
    response = xmlResponse

instance FromXML CreateClusterResponse where
    parseXML = withElement "CreateClusterResult" $ \x -> CreateClusterResponse
        <$> x .@? "cluster"
