{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified manual snapshot. The snapshot must be in the
-- @available@ state, with no other users authorized to access the
-- snapshot.
--
-- Unlike automated snapshots, manual snapshots are retained even after you
-- delete your cluster. Amazon Redshift does not delete your manual
-- snapshots. You must delete manual snapshot explicitly to avoid getting
-- charged. If other accounts are authorized to access the snapshot, you
-- must revoke all of the authorizations before you can delete the
-- snapshot.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteClusterSnapshot.html>
module Network.AWS.Redshift.DeleteClusterSnapshot
    (
    -- * Request
      DeleteClusterSnapshot
    -- ** Request constructor
    , deleteClusterSnapshot
    -- ** Request lenses
    , dcsSnapshotClusterIdentifier
    , dcsSnapshotIdentifier

    -- * Response
    , DeleteClusterSnapshotResponse
    -- ** Response constructor
    , deleteClusterSnapshotResponse
    -- ** Response lenses
    , dcsrsSnapshot
    , dcsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteClusterSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsSnapshotClusterIdentifier'
--
-- * 'dcsSnapshotIdentifier'
data DeleteClusterSnapshot = DeleteClusterSnapshot'
    { _dcsSnapshotClusterIdentifier :: !(Maybe Text)
    , _dcsSnapshotIdentifier        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterSnapshot' smart constructor.
deleteClusterSnapshot :: Text -> DeleteClusterSnapshot
deleteClusterSnapshot pSnapshotIdentifier_ =
    DeleteClusterSnapshot'
    { _dcsSnapshotClusterIdentifier = Nothing
    , _dcsSnapshotIdentifier = pSnapshotIdentifier_
    }

-- | The unique identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- Constraints: Must be the name of valid cluster.
dcsSnapshotClusterIdentifier :: Lens' DeleteClusterSnapshot (Maybe Text)
dcsSnapshotClusterIdentifier = lens _dcsSnapshotClusterIdentifier (\ s a -> s{_dcsSnapshotClusterIdentifier = a});

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the
-- @available@ state.
dcsSnapshotIdentifier :: Lens' DeleteClusterSnapshot Text
dcsSnapshotIdentifier = lens _dcsSnapshotIdentifier (\ s a -> s{_dcsSnapshotIdentifier = a});

instance AWSRequest DeleteClusterSnapshot where
        type Sv DeleteClusterSnapshot = Redshift
        type Rs DeleteClusterSnapshot =
             DeleteClusterSnapshotResponse
        request = post "DeleteClusterSnapshot"
        response
          = receiveXMLWrapper "DeleteClusterSnapshotResult"
              (\ s h x ->
                 DeleteClusterSnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance ToHeaders DeleteClusterSnapshot where
        toHeaders = const mempty

instance ToPath DeleteClusterSnapshot where
        toPath = const "/"

instance ToQuery DeleteClusterSnapshot where
        toQuery DeleteClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("DeleteClusterSnapshot" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SnapshotClusterIdentifier" =:
                 _dcsSnapshotClusterIdentifier,
               "SnapshotIdentifier" =: _dcsSnapshotIdentifier]

-- | /See:/ 'deleteClusterSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsrsSnapshot'
--
-- * 'dcsrsStatus'
data DeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse'
    { _dcsrsSnapshot :: !(Maybe Snapshot)
    , _dcsrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterSnapshotResponse' smart constructor.
deleteClusterSnapshotResponse :: Int -> DeleteClusterSnapshotResponse
deleteClusterSnapshotResponse pStatus_ =
    DeleteClusterSnapshotResponse'
    { _dcsrsSnapshot = Nothing
    , _dcsrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dcsrsSnapshot :: Lens' DeleteClusterSnapshotResponse (Maybe Snapshot)
dcsrsSnapshot = lens _dcsrsSnapshot (\ s a -> s{_dcsrsSnapshot = a});

-- | FIXME: Undocumented member.
dcsrsStatus :: Lens' DeleteClusterSnapshotResponse Int
dcsrsStatus = lens _dcsrsStatus (\ s a -> s{_dcsrsStatus = a});
