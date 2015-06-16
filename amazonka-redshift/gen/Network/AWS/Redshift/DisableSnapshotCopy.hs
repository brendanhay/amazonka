{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.DisableSnapshotCopy
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

-- | Disables the automatic copying of snapshots from one region to another
-- region for a specified cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DisableSnapshotCopy.html>
module Network.AWS.Redshift.DisableSnapshotCopy
    (
    -- * Request
      DisableSnapshotCopy
    -- ** Request constructor
    , disableSnapshotCopy
    -- ** Request lenses
    , dscClusterIdentifier

    -- * Response
    , DisableSnapshotCopyResponse
    -- ** Response constructor
    , disableSnapshotCopyResponse
    -- ** Response lenses
    , dscrCluster
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Redshift.Types

-- | /See:/ 'disableSnapshotCopy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscClusterIdentifier'
newtype DisableSnapshotCopy = DisableSnapshotCopy'{_dscClusterIdentifier :: Text} deriving (Eq, Read, Show)

-- | 'DisableSnapshotCopy' smart constructor.
disableSnapshotCopy :: Text -> DisableSnapshotCopy
disableSnapshotCopy pClusterIdentifier = DisableSnapshotCopy'{_dscClusterIdentifier = pClusterIdentifier};

-- | The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
dscClusterIdentifier :: Lens' DisableSnapshotCopy Text
dscClusterIdentifier = lens _dscClusterIdentifier (\ s a -> s{_dscClusterIdentifier = a});

instance AWSRequest DisableSnapshotCopy where
        type Sv DisableSnapshotCopy = Redshift
        type Rs DisableSnapshotCopy =
             DisableSnapshotCopyResponse
        request = post
        response
          = receiveXMLWrapper "DisableSnapshotCopyResult"
              (\ s h x ->
                 DisableSnapshotCopyResponse' <$> (x .@? "Cluster"))

instance ToHeaders DisableSnapshotCopy where
        toHeaders = const mempty

instance ToPath DisableSnapshotCopy where
        toPath = const "/"

instance ToQuery DisableSnapshotCopy where
        toQuery DisableSnapshotCopy'{..}
          = mconcat
              ["Action" =: ("DisableSnapshotCopy" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterIdentifier" =: _dscClusterIdentifier]

-- | /See:/ 'disableSnapshotCopyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscrCluster'
newtype DisableSnapshotCopyResponse = DisableSnapshotCopyResponse'{_dscrCluster :: Maybe Cluster} deriving (Eq, Read, Show)

-- | 'DisableSnapshotCopyResponse' smart constructor.
disableSnapshotCopyResponse :: DisableSnapshotCopyResponse
disableSnapshotCopyResponse = DisableSnapshotCopyResponse'{_dscrCluster = Nothing};

-- | FIXME: Undocumented member.
dscrCluster :: Lens' DisableSnapshotCopyResponse (Maybe Cluster)
dscrCluster = lens _dscrCluster (\ s a -> s{_dscrCluster = a});
