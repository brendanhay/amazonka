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
-- Module      : Network.AWS.RDS.DeleteDBClusterSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB cluster snapshot. If the snapshot is being copied, the copy
-- operation is terminated.
--
-- The DB cluster snapshot must be in the @available@ state to be deleted.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBClusterSnapshot.html AWS API Reference> for DeleteDBClusterSnapshot.
module Network.AWS.RDS.DeleteDBClusterSnapshot
    (
    -- * Creating a Request
      DeleteDBClusterSnapshot
    , deleteDBClusterSnapshot
    -- * Request Lenses
    , ddcsDBClusterSnapshotIdentifier

    -- * Destructuring the Response
    , DeleteDBClusterSnapshotResponse
    , deleteDBClusterSnapshotResponse
    -- * Response Lenses
    , ddcsrsDBClusterSnapshot
    , ddcsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBClusterSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcsDBClusterSnapshotIdentifier'
newtype DeleteDBClusterSnapshot = DeleteDBClusterSnapshot'
    { _ddcsDBClusterSnapshotIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBClusterSnapshot' smart constructor.
deleteDBClusterSnapshot :: Text -> DeleteDBClusterSnapshot
deleteDBClusterSnapshot pDBClusterSnapshotIdentifier_ =
    DeleteDBClusterSnapshot'
    { _ddcsDBClusterSnapshotIdentifier = pDBClusterSnapshotIdentifier_
    }

-- | The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the
-- @available@ state.
ddcsDBClusterSnapshotIdentifier :: Lens' DeleteDBClusterSnapshot Text
ddcsDBClusterSnapshotIdentifier = lens _ddcsDBClusterSnapshotIdentifier (\ s a -> s{_ddcsDBClusterSnapshotIdentifier = a});

instance AWSRequest DeleteDBClusterSnapshot where
        type Sv DeleteDBClusterSnapshot = RDS
        type Rs DeleteDBClusterSnapshot =
             DeleteDBClusterSnapshotResponse
        request = postQuery
        response
          = receiveXMLWrapper "DeleteDBClusterSnapshotResult"
              (\ s h x ->
                 DeleteDBClusterSnapshotResponse' <$>
                   (x .@? "DBClusterSnapshot") <*> (pure (fromEnum s)))

instance ToHeaders DeleteDBClusterSnapshot where
        toHeaders = const mempty

instance ToPath DeleteDBClusterSnapshot where
        toPath = const "/"

instance ToQuery DeleteDBClusterSnapshot where
        toQuery DeleteDBClusterSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("DeleteDBClusterSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterSnapshotIdentifier" =:
                 _ddcsDBClusterSnapshotIdentifier]

-- | /See:/ 'deleteDBClusterSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcsrsDBClusterSnapshot'
--
-- * 'ddcsrsStatus'
data DeleteDBClusterSnapshotResponse = DeleteDBClusterSnapshotResponse'
    { _ddcsrsDBClusterSnapshot :: !(Maybe DBClusterSnapshot)
    , _ddcsrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBClusterSnapshotResponse' smart constructor.
deleteDBClusterSnapshotResponse :: Int -> DeleteDBClusterSnapshotResponse
deleteDBClusterSnapshotResponse pStatus_ =
    DeleteDBClusterSnapshotResponse'
    { _ddcsrsDBClusterSnapshot = Nothing
    , _ddcsrsStatus = pStatus_
    }

-- | Undocumented member.
ddcsrsDBClusterSnapshot :: Lens' DeleteDBClusterSnapshotResponse (Maybe DBClusterSnapshot)
ddcsrsDBClusterSnapshot = lens _ddcsrsDBClusterSnapshot (\ s a -> s{_ddcsrsDBClusterSnapshot = a});

-- | Undocumented member.
ddcsrsStatus :: Lens' DeleteDBClusterSnapshotResponse Int
ddcsrsStatus = lens _ddcsrsStatus (\ s a -> s{_ddcsrsStatus = a});
