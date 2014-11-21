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

-- Module      : Network.AWS.Redshift.DeleteCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a previously provisioned cluster. A successful response from the
-- web service indicates that the request was received correctly. Use
-- DescribeClusters to monitor the status of the deletion. The delete
-- operation cannot be canceled or reverted once submitted. For more
-- information about managing clusters, go to Amazon Redshift Clusters in the
-- Amazon Redshift Cluster Management Guide . If you want to shut down the
-- cluster and retain it for future use, set SkipFinalClusterSnapshot to false
-- and specify a name for FinalClusterSnapshotIdentifier. You can later
-- restore this snapshot to resume using the cluster. If a final cluster
-- snapshot is requested, the status of the cluster will be "final-snapshot"
-- while the snapshot is being taken, then it's "deleting" once Amazon
-- Redshift begins deleting the cluster. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Cluster
-- Management Guide .
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteCluster.html>
module Network.AWS.Redshift.DeleteCluster
    (
    -- * Request
      DeleteCluster
    -- ** Request constructor
    , deleteCluster
    -- ** Request lenses
    , dc1ClusterIdentifier
    , dc1FinalClusterSnapshotIdentifier
    , dc1SkipFinalClusterSnapshot

    -- * Response
    , DeleteClusterResponse
    -- ** Response constructor
    , deleteClusterResponse
    -- ** Response lenses
    , dcrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DeleteCluster = DeleteCluster
    { _dc1ClusterIdentifier              :: Text
    , _dc1FinalClusterSnapshotIdentifier :: Maybe Text
    , _dc1SkipFinalClusterSnapshot       :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'DeleteCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dc1ClusterIdentifier' @::@ 'Text'
--
-- * 'dc1FinalClusterSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dc1SkipFinalClusterSnapshot' @::@ 'Maybe' 'Bool'
--
deleteCluster :: Text -- ^ 'dc1ClusterIdentifier'
              -> DeleteCluster
deleteCluster p1 = DeleteCluster
    { _dc1ClusterIdentifier              = p1
    , _dc1SkipFinalClusterSnapshot       = Nothing
    , _dc1FinalClusterSnapshotIdentifier = Nothing
    }

-- | The identifier of the cluster to be deleted. Constraints: Must contain
-- lowercase characters. Must contain from 1 to 63 alphanumeric characters
-- or hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens.
dc1ClusterIdentifier :: Lens' DeleteCluster Text
dc1ClusterIdentifier =
    lens _dc1ClusterIdentifier (\s a -> s { _dc1ClusterIdentifier = a })

-- | The identifier of the final snapshot that is to be created immediately
-- before deleting the cluster. If this parameter is provided,
-- SkipFinalClusterSnapshot must be false. Constraints: Must be 1 to 255
-- alphanumeric characters. First character must be a letter. Cannot end
-- with a hyphen or contain two consecutive hyphens.
dc1FinalClusterSnapshotIdentifier :: Lens' DeleteCluster (Maybe Text)
dc1FinalClusterSnapshotIdentifier =
    lens _dc1FinalClusterSnapshotIdentifier
        (\s a -> s { _dc1FinalClusterSnapshotIdentifier = a })

-- | Determines whether a final snapshot of the cluster is created before
-- Amazon Redshift deletes the cluster. If true, a final cluster snapshot is
-- not created. If false, a final cluster snapshot is created before the
-- cluster is deleted. The FinalClusterSnapshotIdentifier parameter must be
-- specified if SkipFinalClusterSnapshot is false. Default: false.
dc1SkipFinalClusterSnapshot :: Lens' DeleteCluster (Maybe Bool)
dc1SkipFinalClusterSnapshot =
    lens _dc1SkipFinalClusterSnapshot
        (\s a -> s { _dc1SkipFinalClusterSnapshot = a })

newtype DeleteClusterResponse = DeleteClusterResponse
    { _dcrCluster :: Maybe Cluster
    } deriving (Eq, Show)

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

dcrCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
dcrCluster = lens _dcrCluster (\s a -> s { _dcrCluster = a })

instance ToPath DeleteCluster where
    toPath = const "/"

instance ToQuery DeleteCluster where
    toQuery DeleteCluster{..} = mconcat
        [ "ClusterIdentifier"              =? _dc1ClusterIdentifier
        , "FinalClusterSnapshotIdentifier" =? _dc1FinalClusterSnapshotIdentifier
        , "SkipFinalClusterSnapshot"       =? _dc1SkipFinalClusterSnapshot
        ]

instance ToHeaders DeleteCluster

instance AWSRequest DeleteCluster where
    type Sv DeleteCluster = Redshift
    type Rs DeleteCluster = DeleteClusterResponse

    request  = post "DeleteCluster"
    response = xmlResponse

instance FromXML DeleteClusterResponse where
    parseXML = withElement "DeleteClusterResult" $ \x -> DeleteClusterResponse
        <$> x .@? "Cluster"
