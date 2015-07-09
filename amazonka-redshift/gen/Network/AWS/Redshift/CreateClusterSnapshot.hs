{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a manual snapshot of the specified cluster. The cluster must be
-- in the @available@ state.
--
-- For more information about working with snapshots, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterSnapshot.html>
module Network.AWS.Redshift.CreateClusterSnapshot
    (
    -- * Request
      CreateClusterSnapshot
    -- ** Request constructor
    , createClusterSnapshot
    -- ** Request lenses
    , ccsTags
    , ccsSnapshotIdentifier
    , ccsClusterIdentifier

    -- * Response
    , CreateClusterSnapshotResponse
    -- ** Response constructor
    , createClusterSnapshotResponse
    -- ** Response lenses
    , cSnapshot
    , cStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createClusterSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsTags'
--
-- * 'ccsSnapshotIdentifier'
--
-- * 'ccsClusterIdentifier'
data CreateClusterSnapshot = CreateClusterSnapshot'
    { _ccsTags               :: !(Maybe [Tag])
    , _ccsSnapshotIdentifier :: !Text
    , _ccsClusterIdentifier  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSnapshot' smart constructor.
createClusterSnapshot :: Text -> Text -> CreateClusterSnapshot
createClusterSnapshot pSnapshotIdentifier pClusterIdentifier =
    CreateClusterSnapshot'
    { _ccsTags = Nothing
    , _ccsSnapshotIdentifier = pSnapshotIdentifier
    , _ccsClusterIdentifier = pClusterIdentifier
    }

-- | A list of tag instances.
ccsTags :: Lens' CreateClusterSnapshot [Tag]
ccsTags = lens _ccsTags (\ s a -> s{_ccsTags = a}) . _Default;

-- | A unique identifier for the snapshot that you are requesting. This
-- identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
ccsSnapshotIdentifier :: Lens' CreateClusterSnapshot Text
ccsSnapshotIdentifier = lens _ccsSnapshotIdentifier (\ s a -> s{_ccsSnapshotIdentifier = a});

-- | The cluster identifier for which you want a snapshot.
ccsClusterIdentifier :: Lens' CreateClusterSnapshot Text
ccsClusterIdentifier = lens _ccsClusterIdentifier (\ s a -> s{_ccsClusterIdentifier = a});

instance AWSRequest CreateClusterSnapshot where
        type Sv CreateClusterSnapshot = Redshift
        type Rs CreateClusterSnapshot =
             CreateClusterSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CreateClusterSnapshotResult"
              (\ s h x ->
                 CreateClusterSnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance ToHeaders CreateClusterSnapshot where
        toHeaders = const mempty

instance ToPath CreateClusterSnapshot where
        toPath = const "/"

instance ToQuery CreateClusterSnapshot where
        toQuery CreateClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateClusterSnapshot" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _ccsTags),
               "SnapshotIdentifier" =: _ccsSnapshotIdentifier,
               "ClusterIdentifier" =: _ccsClusterIdentifier]

-- | /See:/ 'createClusterSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cSnapshot'
--
-- * 'cStatus'
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
    { _cSnapshot :: !(Maybe Snapshot)
    , _cStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSnapshotResponse' smart constructor.
createClusterSnapshotResponse :: Int -> CreateClusterSnapshotResponse
createClusterSnapshotResponse pStatus =
    CreateClusterSnapshotResponse'
    { _cSnapshot = Nothing
    , _cStatus = pStatus
    }

-- | FIXME: Undocumented member.
cSnapshot :: Lens' CreateClusterSnapshotResponse (Maybe Snapshot)
cSnapshot = lens _cSnapshot (\ s a -> s{_cSnapshot = a});

-- | FIXME: Undocumented member.
cStatus :: Lens' CreateClusterSnapshotResponse Int
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});
