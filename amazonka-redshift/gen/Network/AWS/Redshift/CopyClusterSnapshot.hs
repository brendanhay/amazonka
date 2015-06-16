{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.CopyClusterSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Copies the specified automated cluster snapshot to a new manual cluster
-- snapshot. The source must be an automated snapshot and it must be in the
-- available state.
--
-- When you delete a cluster, Amazon Redshift deletes any automated
-- snapshots of the cluster. Also, when the retention period of the
-- snapshot expires, Amazon Redshift automatically deletes it. If you want
-- to keep an automated snapshot for a longer period, you can make a manual
-- copy of the snapshot. Manual snapshots are retained until you delete
-- them.
--
-- For more information about working with snapshots, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CopyClusterSnapshot.html>
module Network.AWS.Redshift.CopyClusterSnapshot
    (
    -- * Request
      CopyClusterSnapshot
    -- ** Request constructor
    , copyClusterSnapshot
    -- ** Request lenses
    , ccsSourceSnapshotClusterIdentifier
    , ccsSourceSnapshotIdentifier
    , ccsTargetSnapshotIdentifier

    -- * Response
    , CopyClusterSnapshotResponse
    -- ** Response constructor
    , copyClusterSnapshotResponse
    -- ** Response lenses
    , ccsrSnapshot
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Redshift.Types

-- | /See:/ 'copyClusterSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsSourceSnapshotClusterIdentifier'
--
-- * 'ccsSourceSnapshotIdentifier'
--
-- * 'ccsTargetSnapshotIdentifier'
data CopyClusterSnapshot = CopyClusterSnapshot'{_ccsSourceSnapshotClusterIdentifier :: Maybe Text, _ccsSourceSnapshotIdentifier :: Text, _ccsTargetSnapshotIdentifier :: Text} deriving (Eq, Read, Show)

-- | 'CopyClusterSnapshot' smart constructor.
copyClusterSnapshot :: Text -> Text -> CopyClusterSnapshot
copyClusterSnapshot pSourceSnapshotIdentifier pTargetSnapshotIdentifier = CopyClusterSnapshot'{_ccsSourceSnapshotClusterIdentifier = Nothing, _ccsSourceSnapshotIdentifier = pSourceSnapshotIdentifier, _ccsTargetSnapshotIdentifier = pTargetSnapshotIdentifier};

-- | The identifier of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- Constraints:
--
-- -   Must be the identifier for a valid cluster.
ccsSourceSnapshotClusterIdentifier :: Lens' CopyClusterSnapshot (Maybe Text)
ccsSourceSnapshotClusterIdentifier = lens _ccsSourceSnapshotClusterIdentifier (\ s a -> s{_ccsSourceSnapshotClusterIdentifier = a});

-- | The identifier for the source snapshot.
--
-- Constraints:
--
-- -   Must be the identifier for a valid automated snapshot whose state is
--     @available@.
ccsSourceSnapshotIdentifier :: Lens' CopyClusterSnapshot Text
ccsSourceSnapshotIdentifier = lens _ccsSourceSnapshotIdentifier (\ s a -> s{_ccsSourceSnapshotIdentifier = a});

-- | The identifier given to the new manual snapshot.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank.
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
-- -   Must be unique for the AWS account that is making the request.
ccsTargetSnapshotIdentifier :: Lens' CopyClusterSnapshot Text
ccsTargetSnapshotIdentifier = lens _ccsTargetSnapshotIdentifier (\ s a -> s{_ccsTargetSnapshotIdentifier = a});

instance AWSRequest CopyClusterSnapshot where
        type Sv CopyClusterSnapshot = Redshift
        type Rs CopyClusterSnapshot =
             CopyClusterSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CopyClusterSnapshotResult"
              (\ s h x ->
                 CopyClusterSnapshotResponse' <$> (x .@? "Snapshot"))

instance ToHeaders CopyClusterSnapshot where
        toHeaders = const mempty

instance ToPath CopyClusterSnapshot where
        toPath = const "/"

instance ToQuery CopyClusterSnapshot where
        toQuery CopyClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyClusterSnapshot" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SourceSnapshotClusterIdentifier" =:
                 _ccsSourceSnapshotClusterIdentifier,
               "SourceSnapshotIdentifier" =:
                 _ccsSourceSnapshotIdentifier,
               "TargetSnapshotIdentifier" =:
                 _ccsTargetSnapshotIdentifier]

-- | /See:/ 'copyClusterSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsrSnapshot'
newtype CopyClusterSnapshotResponse = CopyClusterSnapshotResponse'{_ccsrSnapshot :: Maybe Snapshot} deriving (Eq, Read, Show)

-- | 'CopyClusterSnapshotResponse' smart constructor.
copyClusterSnapshotResponse :: CopyClusterSnapshotResponse
copyClusterSnapshotResponse = CopyClusterSnapshotResponse'{_ccsrSnapshot = Nothing};

-- | FIXME: Undocumented member.
ccsrSnapshot :: Lens' CopyClusterSnapshotResponse (Maybe Snapshot)
ccsrSnapshot = lens _ccsrSnapshot (\ s a -> s{_ccsrSnapshot = a});
