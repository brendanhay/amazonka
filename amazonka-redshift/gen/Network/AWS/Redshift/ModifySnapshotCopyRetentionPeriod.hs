{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
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

-- | Modifies the number of days to retain automated snapshots in the
-- destination region after they are copied from the source region.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ModifySnapshotCopyRetentionPeriod.html>
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
    (
    -- * Request
      ModifySnapshotCopyRetentionPeriod
    -- ** Request constructor
    , modifySnapshotCopyRetentionPeriod
    -- ** Request lenses
    , mscrpClusterIdentifier
    , mscrpRetentionPeriod

    -- * Response
    , ModifySnapshotCopyRetentionPeriodResponse
    -- ** Response constructor
    , modifySnapshotCopyRetentionPeriodResponse
    -- ** Response lenses
    , mscrprCluster
    , mscrprStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifySnapshotCopyRetentionPeriod' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mscrpClusterIdentifier'
--
-- * 'mscrpRetentionPeriod'
data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod'
    { _mscrpClusterIdentifier :: !Text
    , _mscrpRetentionPeriod   :: !Int
    } deriving (Eq,Read,Show)

-- | 'ModifySnapshotCopyRetentionPeriod' smart constructor.
modifySnapshotCopyRetentionPeriod :: Text -> Int -> ModifySnapshotCopyRetentionPeriod
modifySnapshotCopyRetentionPeriod pClusterIdentifier pRetentionPeriod =
    ModifySnapshotCopyRetentionPeriod'
    { _mscrpClusterIdentifier = pClusterIdentifier
    , _mscrpRetentionPeriod = pRetentionPeriod
    }

-- | The unique identifier of the cluster for which you want to change the
-- retention period for automated snapshots that are copied to a
-- destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
mscrpClusterIdentifier :: Lens' ModifySnapshotCopyRetentionPeriod Text
mscrpClusterIdentifier = lens _mscrpClusterIdentifier (\ s a -> s{_mscrpClusterIdentifier = a});

-- | The number of days to retain automated snapshots in the destination
-- region after they are copied from the source region.
--
-- If you decrease the retention period for automated snapshots that are
-- copied to a destination region, Amazon Redshift will delete any existing
-- automated snapshots that were copied to the destination region and that
-- fall outside of the new retention period.
--
-- Constraints: Must be at least 1 and no more than 35.
mscrpRetentionPeriod :: Lens' ModifySnapshotCopyRetentionPeriod Int
mscrpRetentionPeriod = lens _mscrpRetentionPeriod (\ s a -> s{_mscrpRetentionPeriod = a});

instance AWSRequest ModifySnapshotCopyRetentionPeriod
         where
        type Sv ModifySnapshotCopyRetentionPeriod = Redshift
        type Rs ModifySnapshotCopyRetentionPeriod =
             ModifySnapshotCopyRetentionPeriodResponse
        request = post
        response
          = receiveXMLWrapper
              "ModifySnapshotCopyRetentionPeriodResult"
              (\ s h x ->
                 ModifySnapshotCopyRetentionPeriodResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance ToHeaders ModifySnapshotCopyRetentionPeriod
         where
        toHeaders = const mempty

instance ToPath ModifySnapshotCopyRetentionPeriod
         where
        toPath = const "/"

instance ToQuery ModifySnapshotCopyRetentionPeriod
         where
        toQuery ModifySnapshotCopyRetentionPeriod'{..}
          = mconcat
              ["Action" =:
                 ("ModifySnapshotCopyRetentionPeriod" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterIdentifier" =: _mscrpClusterIdentifier,
               "RetentionPeriod" =: _mscrpRetentionPeriod]

-- | /See:/ 'modifySnapshotCopyRetentionPeriodResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mscrprCluster'
--
-- * 'mscrprStatus'
data ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse'
    { _mscrprCluster :: !(Maybe Cluster)
    , _mscrprStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'ModifySnapshotCopyRetentionPeriodResponse' smart constructor.
modifySnapshotCopyRetentionPeriodResponse :: Int -> ModifySnapshotCopyRetentionPeriodResponse
modifySnapshotCopyRetentionPeriodResponse pStatus =
    ModifySnapshotCopyRetentionPeriodResponse'
    { _mscrprCluster = Nothing
    , _mscrprStatus = pStatus
    }

-- | FIXME: Undocumented member.
mscrprCluster :: Lens' ModifySnapshotCopyRetentionPeriodResponse (Maybe Cluster)
mscrprCluster = lens _mscrprCluster (\ s a -> s{_mscrprCluster = a});

-- | FIXME: Undocumented member.
mscrprStatus :: Lens' ModifySnapshotCopyRetentionPeriodResponse Int
mscrprStatus = lens _mscrprStatus (\ s a -> s{_mscrprStatus = a});
