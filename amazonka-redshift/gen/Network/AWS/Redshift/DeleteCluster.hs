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
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteCluster.html AWS API Reference> for DeleteCluster.
module Network.AWS.Redshift.DeleteCluster
    (
    -- * Creating a Request
      DeleteCluster
    , deleteCluster
    -- * Request Lenses
    , dSkipFinalClusterSnapshot
    , dFinalClusterSnapshotIdentifier
    , dClusterIdentifier

    -- * Destructuring the Response
    , DeleteClusterResponse
    , deleteClusterResponse
    -- * Response Lenses
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
-- * 'dSkipFinalClusterSnapshot'
--
-- * 'dFinalClusterSnapshotIdentifier'
--
-- * 'dClusterIdentifier'
data DeleteCluster = DeleteCluster'
    { _dSkipFinalClusterSnapshot       :: !(Maybe Bool)
    , _dFinalClusterSnapshotIdentifier :: !(Maybe Text)
    , _dClusterIdentifier              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCluster' smart constructor.
deleteCluster :: Text -> DeleteCluster
deleteCluster pClusterIdentifier_ =
    DeleteCluster'
    { _dSkipFinalClusterSnapshot = Nothing
    , _dFinalClusterSnapshotIdentifier = Nothing
    , _dClusterIdentifier = pClusterIdentifier_
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
dSkipFinalClusterSnapshot :: Lens' DeleteCluster (Maybe Bool)
dSkipFinalClusterSnapshot = lens _dSkipFinalClusterSnapshot (\ s a -> s{_dSkipFinalClusterSnapshot = a});

-- | The identifier of the final snapshot that is to be created immediately
-- before deleting the cluster. If this parameter is provided,
-- /SkipFinalClusterSnapshot/ must be @false@.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
dFinalClusterSnapshotIdentifier :: Lens' DeleteCluster (Maybe Text)
dFinalClusterSnapshotIdentifier = lens _dFinalClusterSnapshotIdentifier (\ s a -> s{_dFinalClusterSnapshotIdentifier = a});

-- | The identifier of the cluster to be deleted.
--
-- Constraints:
--
-- -   Must contain lowercase characters.
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
dClusterIdentifier :: Lens' DeleteCluster Text
dClusterIdentifier = lens _dClusterIdentifier (\ s a -> s{_dClusterIdentifier = a});

instance AWSRequest DeleteCluster where
        type Sv DeleteCluster = Redshift
        type Rs DeleteCluster = DeleteClusterResponse
        request = postQuery
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
                 _dSkipFinalClusterSnapshot,
               "FinalClusterSnapshotIdentifier" =:
                 _dFinalClusterSnapshotIdentifier,
               "ClusterIdentifier" =: _dClusterIdentifier]

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

-- | Undocumented member.
drsCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
drsCluster = lens _drsCluster (\ s a -> s{_drsCluster = a});

-- | Undocumented member.
drsStatus :: Lens' DeleteClusterResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
