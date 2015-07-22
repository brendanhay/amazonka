{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DBSnapshot. If the snapshot is being copied, the copy
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
    , ddbsrqDBSnapshotIdentifier

    -- * Response
    , DeleteDBSnapshotResponse
    -- ** Response constructor
    , deleteDBSnapshotResponse
    -- ** Response lenses
    , ddbsrsDBSnapshot
    , ddbsrsStatus
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
-- * 'ddbsrqDBSnapshotIdentifier'
newtype DeleteDBSnapshot = DeleteDBSnapshot'
    { _ddbsrqDBSnapshotIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBSnapshot' smart constructor.
deleteDBSnapshot :: Text -> DeleteDBSnapshot
deleteDBSnapshot pDBSnapshotIdentifier =
    DeleteDBSnapshot'
    { _ddbsrqDBSnapshotIdentifier = pDBSnapshotIdentifier
    }

-- | The DBSnapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the
-- @available@ state.
ddbsrqDBSnapshotIdentifier :: Lens' DeleteDBSnapshot Text
ddbsrqDBSnapshotIdentifier = lens _ddbsrqDBSnapshotIdentifier (\ s a -> s{_ddbsrqDBSnapshotIdentifier = a});

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
               "DBSnapshotIdentifier" =:
                 _ddbsrqDBSnapshotIdentifier]

-- | /See:/ 'deleteDBSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsrsDBSnapshot'
--
-- * 'ddbsrsStatus'
data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse'
    { _ddbsrsDBSnapshot :: !(Maybe DBSnapshot)
    , _ddbsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBSnapshotResponse' smart constructor.
deleteDBSnapshotResponse :: Int -> DeleteDBSnapshotResponse
deleteDBSnapshotResponse pStatus =
    DeleteDBSnapshotResponse'
    { _ddbsrsDBSnapshot = Nothing
    , _ddbsrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ddbsrsDBSnapshot :: Lens' DeleteDBSnapshotResponse (Maybe DBSnapshot)
ddbsrsDBSnapshot = lens _ddbsrsDBSnapshot (\ s a -> s{_ddbsrsDBSnapshot = a});

-- | FIXME: Undocumented member.
ddbsrsStatus :: Lens' DeleteDBSnapshotResponse Int
ddbsrsStatus = lens _ddbsrsStatus (\ s a -> s{_ddbsrsStatus = a});
