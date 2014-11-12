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
-- Amazon Redshift Management Guide . If you want to shut down the cluster and
-- retain it for future use, set SkipFinalClusterSnapshot to false and specify
-- a name for FinalClusterSnapshotIdentifier. You can later restore this
-- snapshot to resume using the cluster. If a final cluster snapshot is
-- requested, the status of the cluster will be "final-snapshot" while the
-- snapshot is being taken, then it's "deleting" once Amazon Redshift begins
-- deleting the cluster. For more information about managing clusters, go to
-- Amazon Redshift Clusters in the Amazon Redshift Management Guide .
module Network.AWS.Redshift.DeleteCluster
    (
    -- * Request
      DeleteClusterMessage
    -- ** Request constructor
    , deleteCluster
    -- ** Request lenses
    , dcmClusterIdentifier
    , dcmFinalClusterSnapshotIdentifier
    , dcmSkipFinalClusterSnapshot

    -- * Response
    , DeleteClusterResult
    -- ** Response constructor
    , deleteClusterResponse
    -- ** Response lenses
    , dcrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DeleteClusterMessage = DeleteClusterMessage
    { _dcmClusterIdentifier              :: Text
    , _dcmFinalClusterSnapshotIdentifier :: Maybe Text
    , _dcmSkipFinalClusterSnapshot       :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteClusterMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcmClusterIdentifier' @::@ 'Text'
--
-- * 'dcmFinalClusterSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcmSkipFinalClusterSnapshot' @::@ 'Maybe' 'Bool'
--
deleteCluster :: Text -- ^ 'dcmClusterIdentifier'
              -> DeleteClusterMessage
deleteCluster p1 = DeleteClusterMessage
    { _dcmClusterIdentifier              = p1
    , _dcmSkipFinalClusterSnapshot       = Nothing
    , _dcmFinalClusterSnapshotIdentifier = Nothing
    }

-- | The identifier of the cluster to be deleted. Constraints: Must contain
-- lowercase characters. Must contain from 1 to 63 alphanumeric characters
-- or hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens.
dcmClusterIdentifier :: Lens' DeleteClusterMessage Text
dcmClusterIdentifier =
    lens _dcmClusterIdentifier (\s a -> s { _dcmClusterIdentifier = a })

-- | The identifier of the final snapshot that is to be created immediately
-- before deleting the cluster. If this parameter is provided,
-- SkipFinalClusterSnapshot must be false. Constraints: Must be 1 to 255
-- alphanumeric characters. First character must be a letter. Cannot end
-- with a hyphen or contain two consecutive hyphens.
dcmFinalClusterSnapshotIdentifier :: Lens' DeleteClusterMessage (Maybe Text)
dcmFinalClusterSnapshotIdentifier =
    lens _dcmFinalClusterSnapshotIdentifier
        (\s a -> s { _dcmFinalClusterSnapshotIdentifier = a })

-- | Determines whether a final snapshot of the cluster is created before
-- Amazon Redshift deletes the cluster. If true, a final cluster snapshot is
-- not created. If false, a final cluster snapshot is created before the
-- cluster is deleted. Default: false.
dcmSkipFinalClusterSnapshot :: Lens' DeleteClusterMessage (Maybe Bool)
dcmSkipFinalClusterSnapshot =
    lens _dcmSkipFinalClusterSnapshot
        (\s a -> s { _dcmSkipFinalClusterSnapshot = a })

instance ToQuery DeleteClusterMessage

instance ToPath DeleteClusterMessage where
    toPath = const "/"

newtype DeleteClusterResult = DeleteClusterResult
    { _dcrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'DeleteClusterResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrCluster' @::@ 'Maybe' 'Cluster'
--
deleteClusterResponse :: DeleteClusterResult
deleteClusterResponse = DeleteClusterResult
    { _dcrCluster = Nothing
    }

dcrCluster :: Lens' DeleteClusterResult (Maybe Cluster)
dcrCluster = lens _dcrCluster (\s a -> s { _dcrCluster = a })

instance FromXML DeleteClusterResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteClusterResult"

instance AWSRequest DeleteClusterMessage where
    type Sv DeleteClusterMessage = Redshift
    type Rs DeleteClusterMessage = DeleteClusterResult

    request  = post "DeleteCluster"
    response = xmlResponse $ \h x -> DeleteClusterResult
        <$> x %| "Cluster"
