{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot.
--
-- When you make periodic snapshots of a volume, the snapshots are
-- incremental, and only the blocks on the device that have changed since
-- your last snapshot are saved in the new snapshot. When you delete a
-- snapshot, only the data not needed for any other snapshot is removed. So
-- regardless of which prior snapshots have been deleted, all active
-- snapshots will have access to all the information needed to restore the
-- volume.
--
-- You cannot delete a snapshot of the root device of an EBS volume used by
-- a registered AMI. You must first de-register the AMI before you can
-- delete the snapshot.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-snapshot.html Deleting an Amazon EBS Snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSnapshot.html>
module Network.AWS.EC2.DeleteSnapshot
    (
    -- * Request
      DeleteSnapshot
    -- ** Request constructor
    , deleteSnapshot
    -- ** Request lenses
    , deleDryRun
    , deleSnapshotId

    -- * Response
    , DeleteSnapshotResponse
    -- ** Response constructor
    , deleteSnapshotResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deleDryRun'
--
-- * 'deleSnapshotId'
data DeleteSnapshot = DeleteSnapshot'
    { _deleDryRun     :: !(Maybe Bool)
    , _deleSnapshotId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshot' smart constructor.
deleteSnapshot :: Text -> DeleteSnapshot
deleteSnapshot pSnapshotId_ =
    DeleteSnapshot'
    { _deleDryRun = Nothing
    , _deleSnapshotId = pSnapshotId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleDryRun :: Lens' DeleteSnapshot (Maybe Bool)
deleDryRun = lens _deleDryRun (\ s a -> s{_deleDryRun = a});

-- | The ID of the EBS snapshot.
deleSnapshotId :: Lens' DeleteSnapshot Text
deleSnapshotId = lens _deleSnapshotId (\ s a -> s{_deleSnapshotId = a});

instance AWSRequest DeleteSnapshot where
        type Sv DeleteSnapshot = EC2
        type Rs DeleteSnapshot = DeleteSnapshotResponse
        request = post "DeleteSnapshot"
        response = receiveNull DeleteSnapshotResponse'

instance ToHeaders DeleteSnapshot where
        toHeaders = const mempty

instance ToPath DeleteSnapshot where
        toPath = const "/"

instance ToQuery DeleteSnapshot where
        toQuery DeleteSnapshot'{..}
          = mconcat
              ["Action" =: ("DeleteSnapshot" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _deleDryRun,
               "SnapshotId" =: _deleSnapshotId]

-- | /See:/ 'deleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse =
    DeleteSnapshotResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshotResponse' smart constructor.
deleteSnapshotResponse :: DeleteSnapshotResponse
deleteSnapshotResponse = DeleteSnapshotResponse'
