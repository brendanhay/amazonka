{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.DeleteCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a previously provisioned cluster. A successful response from the
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
    , delSkipFinalClusterSnapshot
    , delFinalClusterSnapshotIdentifier
    , delClusterIdentifier

    -- * Response
    , DeleteClusterResponse
    -- ** Response constructor
    , deleteClusterResponse
    -- ** Response lenses
    , delCluster
    , delStatus
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
-- * 'delSkipFinalClusterSnapshot'
--
-- * 'delFinalClusterSnapshotIdentifier'
--
-- * 'delClusterIdentifier'
data DeleteCluster = DeleteCluster'
    { _delSkipFinalClusterSnapshot       :: !(Maybe Bool)
    , _delFinalClusterSnapshotIdentifier :: !(Maybe Text)
    , _delClusterIdentifier              :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteCluster' smart constructor.
deleteCluster :: Text -> DeleteCluster
deleteCluster pClusterIdentifier =
    DeleteCluster'
    { _delSkipFinalClusterSnapshot = Nothing
    , _delFinalClusterSnapshotIdentifier = Nothing
    , _delClusterIdentifier = pClusterIdentifier
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
delSkipFinalClusterSnapshot :: Lens' DeleteCluster (Maybe Bool)
delSkipFinalClusterSnapshot = lens _delSkipFinalClusterSnapshot (\ s a -> s{_delSkipFinalClusterSnapshot = a});

-- | The identifier of the final snapshot that is to be created immediately
-- before deleting the cluster. If this parameter is provided,
-- /SkipFinalClusterSnapshot/ must be @false@.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
delFinalClusterSnapshotIdentifier :: Lens' DeleteCluster (Maybe Text)
delFinalClusterSnapshotIdentifier = lens _delFinalClusterSnapshotIdentifier (\ s a -> s{_delFinalClusterSnapshotIdentifier = a});

-- | The identifier of the cluster to be deleted.
--
-- Constraints:
--
-- -   Must contain lowercase characters.
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
delClusterIdentifier :: Lens' DeleteCluster Text
delClusterIdentifier = lens _delClusterIdentifier (\ s a -> s{_delClusterIdentifier = a});

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
                 _delSkipFinalClusterSnapshot,
               "FinalClusterSnapshotIdentifier" =:
                 _delFinalClusterSnapshotIdentifier,
               "ClusterIdentifier" =: _delClusterIdentifier]

-- | /See:/ 'deleteClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delCluster'
--
-- * 'delStatus'
data DeleteClusterResponse = DeleteClusterResponse'
    { _delCluster :: !(Maybe Cluster)
    , _delStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'DeleteClusterResponse' smart constructor.
deleteClusterResponse :: Int -> DeleteClusterResponse
deleteClusterResponse pStatus =
    DeleteClusterResponse'
    { _delCluster = Nothing
    , _delStatus = pStatus
    }

-- | FIXME: Undocumented member.
delCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
delCluster = lens _delCluster (\ s a -> s{_delCluster = a});

-- | FIXME: Undocumented member.
delStatus :: Lens' DeleteClusterResponse Int
delStatus = lens _delStatus (\ s a -> s{_delStatus = a});
