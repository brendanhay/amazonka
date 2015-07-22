{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , ccsrqTags
    , ccsrqSnapshotIdentifier
    , ccsrqClusterIdentifier

    -- * Response
    , CreateClusterSnapshotResponse
    -- ** Response constructor
    , createClusterSnapshotResponse
    -- ** Response lenses
    , crersSnapshot
    , crersStatus
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
-- * 'ccsrqTags'
--
-- * 'ccsrqSnapshotIdentifier'
--
-- * 'ccsrqClusterIdentifier'
data CreateClusterSnapshot = CreateClusterSnapshot'
    { _ccsrqTags               :: !(Maybe [Tag])
    , _ccsrqSnapshotIdentifier :: !Text
    , _ccsrqClusterIdentifier  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSnapshot' smart constructor.
createClusterSnapshot :: Text -> Text -> CreateClusterSnapshot
createClusterSnapshot pSnapshotIdentifier_ pClusterIdentifier_ =
    CreateClusterSnapshot'
    { _ccsrqTags = Nothing
    , _ccsrqSnapshotIdentifier = pSnapshotIdentifier_
    , _ccsrqClusterIdentifier = pClusterIdentifier_
    }

-- | A list of tag instances.
ccsrqTags :: Lens' CreateClusterSnapshot [Tag]
ccsrqTags = lens _ccsrqTags (\ s a -> s{_ccsrqTags = a}) . _Default;

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
ccsrqSnapshotIdentifier :: Lens' CreateClusterSnapshot Text
ccsrqSnapshotIdentifier = lens _ccsrqSnapshotIdentifier (\ s a -> s{_ccsrqSnapshotIdentifier = a});

-- | The cluster identifier for which you want a snapshot.
ccsrqClusterIdentifier :: Lens' CreateClusterSnapshot Text
ccsrqClusterIdentifier = lens _ccsrqClusterIdentifier (\ s a -> s{_ccsrqClusterIdentifier = a});

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
               "Tags" =: toQuery (toQueryList "Tag" <$> _ccsrqTags),
               "SnapshotIdentifier" =: _ccsrqSnapshotIdentifier,
               "ClusterIdentifier" =: _ccsrqClusterIdentifier]

-- | /See:/ 'createClusterSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crersSnapshot'
--
-- * 'crersStatus'
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
    { _crersSnapshot :: !(Maybe Snapshot)
    , _crersStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSnapshotResponse' smart constructor.
createClusterSnapshotResponse :: Int -> CreateClusterSnapshotResponse
createClusterSnapshotResponse pStatus_ =
    CreateClusterSnapshotResponse'
    { _crersSnapshot = Nothing
    , _crersStatus = pStatus_
    }

-- | FIXME: Undocumented member.
crersSnapshot :: Lens' CreateClusterSnapshotResponse (Maybe Snapshot)
crersSnapshot = lens _crersSnapshot (\ s a -> s{_crersSnapshot = a});

-- | FIXME: Undocumented member.
crersStatus :: Lens' CreateClusterSnapshotResponse Int
crersStatus = lens _crersStatus (\ s a -> s{_crersStatus = a});
