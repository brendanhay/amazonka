{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned cluster. A successful response from the
-- web service indicates that the request was received correctly. Use
-- DescribeClusters to monitor the status of the deletion. The delete
-- operation cannot be canceled or reverted once submitted. For more
-- information about managing clusters, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/ .
--
-- If you want to shut down the cluster and retain it for future use, set
-- /SkipFinalClusterSnapshot/ to @false@ and specify a name for
-- /FinalClusterSnapshotIdentifier/. You can later restore this snapshot to
-- resume using the cluster. If a final cluster snapshot is requested, the
-- status of the cluster will be \"final-snapshot\" while the snapshot is
-- being taken, then it\'s \"deleting\" once Amazon Redshift begins
-- deleting the cluster.
--
-- For more information about managing clusters, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/ .
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteCluster.html>
module Network.AWS.Redshift.DeleteCluster
    (
    -- * Request
      DeleteCluster
    -- ** Request constructor
    , deleteCluster
    -- ** Request lenses
    , drqSkipFinalClusterSnapshot
    , drqFinalClusterSnapshotIdentifier
    , drqClusterIdentifier

    -- * Response
    , DeleteClusterResponse
    -- ** Response constructor
    , deleteClusterResponse
    -- ** Response lenses
    , drsCluster
    , drsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqSkipFinalClusterSnapshot'
--
-- * 'drqFinalClusterSnapshotIdentifier'
--
-- * 'drqClusterIdentifier'
data DeleteCluster = DeleteCluster'
    { _drqSkipFinalClusterSnapshot       :: !(Maybe Bool)
    , _drqFinalClusterSnapshotIdentifier :: !(Maybe Text)
    , _drqClusterIdentifier              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCluster' smart constructor.
deleteCluster :: Text -> DeleteCluster
deleteCluster pClusterIdentifier_ =
    DeleteCluster'
    { _drqSkipFinalClusterSnapshot = Nothing
    , _drqFinalClusterSnapshotIdentifier = Nothing
    , _drqClusterIdentifier = pClusterIdentifier_
    }

-- | Determines whether a final snapshot of the cluster is created before
-- Amazon Redshift deletes the cluster. If @true@, a final cluster snapshot
-- is not created. If @false@, a final cluster snapshot is created before
-- the cluster is deleted.
--
-- The /FinalClusterSnapshotIdentifier/ parameter must be specified if
-- /SkipFinalClusterSnapshot/ is @false@.
--
-- Default: @false@
drqSkipFinalClusterSnapshot :: Lens' DeleteCluster (Maybe Bool)
drqSkipFinalClusterSnapshot = lens _drqSkipFinalClusterSnapshot (\ s a -> s{_drqSkipFinalClusterSnapshot = a});

-- | The identifier of the final snapshot that is to be created immediately
-- before deleting the cluster. If this parameter is provided,
-- /SkipFinalClusterSnapshot/ must be @false@.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
drqFinalClusterSnapshotIdentifier :: Lens' DeleteCluster (Maybe Text)
drqFinalClusterSnapshotIdentifier = lens _drqFinalClusterSnapshotIdentifier (\ s a -> s{_drqFinalClusterSnapshotIdentifier = a});

-- | The identifier of the cluster to be deleted.
--
-- Constraints:
--
-- -   Must contain lowercase characters.
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
drqClusterIdentifier :: Lens' DeleteCluster Text
drqClusterIdentifier = lens _drqClusterIdentifier (\ s a -> s{_drqClusterIdentifier = a});

instance AWSRequest DeleteCluster where
        type Sv DeleteCluster = Redshift
        type Rs DeleteCluster = DeleteClusterResponse
        request = post
        response
          = receiveXMLWrapper "DeleteClusterResult"
              (\ s h x ->
                 DeleteClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance ToHeaders DeleteCluster where
        toHeaders = const mempty

instance ToPath DeleteCluster where
        toPath = const "/"

instance ToQuery DeleteCluster where
        toQuery DeleteCluster'{..}
          = mconcat
              ["Action" =: ("DeleteCluster" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SkipFinalClusterSnapshot" =:
                 _drqSkipFinalClusterSnapshot,
               "FinalClusterSnapshotIdentifier" =:
                 _drqFinalClusterSnapshotIdentifier,
               "ClusterIdentifier" =: _drqClusterIdentifier]

-- | /See:/ 'deleteClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsCluster'
--
-- * 'drsStatus'
data DeleteClusterResponse = DeleteClusterResponse'
    { _drsCluster :: !(Maybe Cluster)
    , _drsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterResponse' smart constructor.
deleteClusterResponse :: Int -> DeleteClusterResponse
deleteClusterResponse pStatus_ =
    DeleteClusterResponse'
    { _drsCluster = Nothing
    , _drsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
drsCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
drsCluster = lens _drsCluster (\ s a -> s{_drsCluster = a});

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeleteClusterResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
