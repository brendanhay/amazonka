{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned cluster. A successful response from the web service indicates that the request was received correctly. Use 'DescribeClusters' to monitor the status of the deletion. The delete operation cannot be canceled or reverted once submitted. For more information about managing clusters, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
--
-- If you want to shut down the cluster and retain it for future use, set /SkipFinalClusterSnapshot/ to @false@ and specify a name for /FinalClusterSnapshotIdentifier/ . You can later restore this snapshot to resume using the cluster. If a final cluster snapshot is requested, the status of the cluster will be "final-snapshot" while the snapshot is being taken, then it's "deleting" once Amazon Redshift begins deleting the cluster.
--
-- For more information about managing clusters, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.DeleteCluster
    (
    -- * Creating a Request
      deleteCluster
    , DeleteCluster
    -- * Request Lenses
    , delSkipFinalClusterSnapshot
    , delFinalClusterSnapshotIdentifier
    , delClusterIdentifier

    -- * Destructuring the Response
    , deleteClusterResponse
    , DeleteClusterResponse
    -- * Response Lenses
    , drsCluster
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { _delSkipFinalClusterSnapshot       :: !(Maybe Bool)
  , _delFinalClusterSnapshotIdentifier :: !(Maybe Text)
  , _delClusterIdentifier              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delSkipFinalClusterSnapshot' - Determines whether a final snapshot of the cluster is created before Amazon Redshift deletes the cluster. If @true@ , a final cluster snapshot is not created. If @false@ , a final cluster snapshot is created before the cluster is deleted.  Default: @false@
--
-- * 'delFinalClusterSnapshotIdentifier' - The identifier of the final snapshot that is to be created immediately before deleting the cluster. If this parameter is provided, /SkipFinalClusterSnapshot/ must be @false@ .  Constraints:     * Must be 1 to 255 alphanumeric characters.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'delClusterIdentifier' - The identifier of the cluster to be deleted. Constraints:     * Must contain lowercase characters.     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
deleteCluster
    :: Text -- ^ 'delClusterIdentifier'
    -> DeleteCluster
deleteCluster pClusterIdentifier_ =
  DeleteCluster'
    { _delSkipFinalClusterSnapshot = Nothing
    , _delFinalClusterSnapshotIdentifier = Nothing
    , _delClusterIdentifier = pClusterIdentifier_
    }


-- | Determines whether a final snapshot of the cluster is created before Amazon Redshift deletes the cluster. If @true@ , a final cluster snapshot is not created. If @false@ , a final cluster snapshot is created before the cluster is deleted.  Default: @false@
delSkipFinalClusterSnapshot :: Lens' DeleteCluster (Maybe Bool)
delSkipFinalClusterSnapshot = lens _delSkipFinalClusterSnapshot (\ s a -> s{_delSkipFinalClusterSnapshot = a})

-- | The identifier of the final snapshot that is to be created immediately before deleting the cluster. If this parameter is provided, /SkipFinalClusterSnapshot/ must be @false@ .  Constraints:     * Must be 1 to 255 alphanumeric characters.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
delFinalClusterSnapshotIdentifier :: Lens' DeleteCluster (Maybe Text)
delFinalClusterSnapshotIdentifier = lens _delFinalClusterSnapshotIdentifier (\ s a -> s{_delFinalClusterSnapshotIdentifier = a})

-- | The identifier of the cluster to be deleted. Constraints:     * Must contain lowercase characters.     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
delClusterIdentifier :: Lens' DeleteCluster Text
delClusterIdentifier = lens _delClusterIdentifier (\ s a -> s{_delClusterIdentifier = a})

instance AWSRequest DeleteCluster where
        type Rs DeleteCluster = DeleteClusterResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "DeleteClusterResult"
              (\ s h x ->
                 DeleteClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable DeleteCluster where

instance NFData DeleteCluster where

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
data DeleteClusterResponse = DeleteClusterResponse'
  { _drsCluster        :: !(Maybe Cluster)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCluster' - Undocumented member.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteClusterResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteClusterResponse
deleteClusterResponse pResponseStatus_ =
  DeleteClusterResponse'
    {_drsCluster = Nothing, _drsResponseStatus = pResponseStatus_}


-- | Undocumented member.
drsCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
drsCluster = lens _drsCluster (\ s a -> s{_drsCluster = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteClusterResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteClusterResponse where
