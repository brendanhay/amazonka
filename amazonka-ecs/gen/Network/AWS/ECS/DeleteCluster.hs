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

-- Module      : Network.AWS.ECS.DeleteCluster
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

-- | Deletes the specified cluster. You must deregister all container instances
-- from this cluster before you may delete it. You can list the container
-- instances in a cluster with 'ListContainerInstances' and deregister them with 'DeregisterContainerInstance'.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeleteCluster.html>
module Network.AWS.ECS.DeleteCluster
    (
    -- * Request
      DeleteCluster
    -- ** Request constructor
    , deleteCluster
    -- ** Request lenses
    , dcCluster

    -- * Response
    , DeleteClusterResponse
    -- ** Response constructor
    , deleteClusterResponse
    -- ** Response lenses
    , dcrCluster
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

newtype DeleteCluster = DeleteCluster
    { _dcCluster :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcCluster' @::@ 'Text'
--
deleteCluster :: Text -- ^ 'dcCluster'
              -> DeleteCluster
deleteCluster p1 = DeleteCluster
    { _dcCluster = p1
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that you
-- want to delete.
dcCluster :: Lens' DeleteCluster Text
dcCluster = lens _dcCluster (\s a -> s { _dcCluster = a })

newtype DeleteClusterResponse = DeleteClusterResponse
    { _dcrCluster :: Maybe Cluster
    } deriving (Eq, Read, Show)

-- | 'DeleteClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrCluster' @::@ 'Maybe' 'Cluster'
--
deleteClusterResponse :: DeleteClusterResponse
deleteClusterResponse = DeleteClusterResponse
    { _dcrCluster = Nothing
    }

-- | The full description of the deleted cluster.
dcrCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
dcrCluster = lens _dcrCluster (\s a -> s { _dcrCluster = a })

instance ToPath DeleteCluster where
    toPath = const "/"

instance ToQuery DeleteCluster where
    toQuery = const mempty

instance ToHeaders DeleteCluster

instance ToJSON DeleteCluster where
    toJSON DeleteCluster{..} = object
        [ "cluster" .= _dcCluster
        ]

instance AWSRequest DeleteCluster where
    type Sv DeleteCluster = ECS
    type Rs DeleteCluster = DeleteClusterResponse

    request  = post "DeleteCluster"
    response = jsonResponse

instance FromJSON DeleteClusterResponse where
    parseJSON = withObject "DeleteClusterResponse" $ \o -> DeleteClusterResponse
        <$> o .:? "cluster"
