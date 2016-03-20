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
-- The DB cluster snapshot must be in the 'available' state to be deleted.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
module Network.AWS.RDS.DeleteDBClusterSnapshot
    (
    -- * Creating a Request
      deleteDBClusterSnapshot
    , DeleteDBClusterSnapshot
    -- * Request Lenses
    , ddcsDBClusterSnapshotIdentifier

    -- * Destructuring the Response
    , deleteDBClusterSnapshotResponse
    , DeleteDBClusterSnapshotResponse
    -- * Response Lenses
    , ddcsrsDBClusterSnapshot
    , ddcsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBClusterSnapshot' smart constructor.
newtype DeleteDBClusterSnapshot = DeleteDBClusterSnapshot'
    { _ddcsDBClusterSnapshotIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDBClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcsDBClusterSnapshotIdentifier'
deleteDBClusterSnapshot
    :: Text -- ^ 'ddcsDBClusterSnapshotIdentifier'
    -> DeleteDBClusterSnapshot
deleteDBClusterSnapshot pDBClusterSnapshotIdentifier_ =
    DeleteDBClusterSnapshot'
    { _ddcsDBClusterSnapshotIdentifier = pDBClusterSnapshotIdentifier_
    }

-- | The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the
-- 'available' state.
ddcsDBClusterSnapshotIdentifier :: Lens' DeleteDBClusterSnapshot Text
ddcsDBClusterSnapshotIdentifier = lens _ddcsDBClusterSnapshotIdentifier (\ s a -> s{_ddcsDBClusterSnapshotIdentifier = a});

instance AWSRequest DeleteDBClusterSnapshot where
        type Rs DeleteDBClusterSnapshot =
             DeleteDBClusterSnapshotResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper "DeleteDBClusterSnapshotResult"
              (\ s h x ->
                 DeleteDBClusterSnapshotResponse' <$>
                   (x .@? "DBClusterSnapshot") <*> (pure (fromEnum s)))

instance Hashable DeleteDBClusterSnapshot

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
data DeleteDBClusterSnapshotResponse = DeleteDBClusterSnapshotResponse'
    { _ddcsrsDBClusterSnapshot :: !(Maybe DBClusterSnapshot)
    , _ddcsrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDBClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcsrsDBClusterSnapshot'
--
-- * 'ddcsrsResponseStatus'
deleteDBClusterSnapshotResponse
    :: Int -- ^ 'ddcsrsResponseStatus'
    -> DeleteDBClusterSnapshotResponse
deleteDBClusterSnapshotResponse pResponseStatus_ =
    DeleteDBClusterSnapshotResponse'
    { _ddcsrsDBClusterSnapshot = Nothing
    , _ddcsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ddcsrsDBClusterSnapshot :: Lens' DeleteDBClusterSnapshotResponse (Maybe DBClusterSnapshot)
ddcsrsDBClusterSnapshot = lens _ddcsrsDBClusterSnapshot (\ s a -> s{_ddcsrsDBClusterSnapshot = a});

-- | The response status code.
ddcsrsResponseStatus :: Lens' DeleteDBClusterSnapshotResponse Int
ddcsrsResponseStatus = lens _ddcsrsResponseStatus (\ s a -> s{_ddcsrsResponseStatus = a});
