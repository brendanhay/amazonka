{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deletes a DBSnapshot. If the snapshot is being copied, the copy
-- operation is terminated.
--
-- The DBSnapshot must be in the @available@ state to be deleted.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBSnapshot.html>
module Network.AWS.RDS.DeleteDBSnapshot
    (
    -- * Request
      DeleteDBSnapshot
    -- ** Request constructor
    , deleteDBSnapshot
    -- ** Request lenses
    , delDBSnapshotIdentifier

    -- * Response
    , DeleteDBSnapshotResponse
    -- ** Response constructor
    , deleteDBSnapshotResponse
    -- ** Response lenses
    , dDBSnapshot
    , dStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delDBSnapshotIdentifier'
newtype DeleteDBSnapshot = DeleteDBSnapshot'
    { _delDBSnapshotIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBSnapshot' smart constructor.
deleteDBSnapshot :: Text -> DeleteDBSnapshot
deleteDBSnapshot pDBSnapshotIdentifier =
    DeleteDBSnapshot'
    { _delDBSnapshotIdentifier = pDBSnapshotIdentifier
    }

-- | The DBSnapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the
-- @available@ state.
delDBSnapshotIdentifier :: Lens' DeleteDBSnapshot Text
delDBSnapshotIdentifier = lens _delDBSnapshotIdentifier (\ s a -> s{_delDBSnapshotIdentifier = a});

instance AWSRequest DeleteDBSnapshot where
        type Sv DeleteDBSnapshot = RDS
        type Rs DeleteDBSnapshot = DeleteDBSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "DeleteDBSnapshotResult"
              (\ s h x ->
                 DeleteDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance ToHeaders DeleteDBSnapshot where
        toHeaders = const mempty

instance ToPath DeleteDBSnapshot where
        toPath = const "/"

instance ToQuery DeleteDBSnapshot where
        toQuery DeleteDBSnapshot'{..}
          = mconcat
              ["Action" =: ("DeleteDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSnapshotIdentifier" =: _delDBSnapshotIdentifier]

-- | /See:/ 'deleteDBSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dDBSnapshot'
--
-- * 'dStatus'
data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse'
    { _dDBSnapshot :: !(Maybe DBSnapshot)
    , _dStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBSnapshotResponse' smart constructor.
deleteDBSnapshotResponse :: Int -> DeleteDBSnapshotResponse
deleteDBSnapshotResponse pStatus =
    DeleteDBSnapshotResponse'
    { _dDBSnapshot = Nothing
    , _dStatus = pStatus
    }

-- | FIXME: Undocumented member.
dDBSnapshot :: Lens' DeleteDBSnapshotResponse (Maybe DBSnapshot)
dDBSnapshot = lens _dDBSnapshot (\ s a -> s{_dDBSnapshot = a});

-- | FIXME: Undocumented member.
dStatus :: Lens' DeleteDBSnapshotResponse Int
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});
