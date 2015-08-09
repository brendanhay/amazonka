{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of days to retain automated snapshots in the
-- destination region after they are copied from the source region.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ModifySnapshotCopyRetentionPeriod.html AWS API Reference> for ModifySnapshotCopyRetentionPeriod.
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
    (
    -- * Creating a Request
      ModifySnapshotCopyRetentionPeriod
    , modifySnapshotCopyRetentionPeriod
    -- * Request Lenses
    , mscrpClusterIdentifier
    , mscrpRetentionPeriod

    -- * Destructuring the Response
    , ModifySnapshotCopyRetentionPeriodResponse
    , modifySnapshotCopyRetentionPeriodResponse
    -- * Response Lenses
    , mscrprsCluster
    , mscrprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifySnapshotCopyRetentionPeriod' smart constructor.
modifySnapshotCopyRetentionPeriod :: Text -> Int -> ModifySnapshotCopyRetentionPeriod
modifySnapshotCopyRetentionPeriod pClusterIdentifier_ pRetentionPeriod_ =
    ModifySnapshotCopyRetentionPeriod'
    { _mscrpClusterIdentifier = pClusterIdentifier_
    , _mscrpRetentionPeriod = pRetentionPeriod_
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
        request = postQuery
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
-- * 'mscrprsCluster'
--
-- * 'mscrprsStatus'
data ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse'
    { _mscrprsCluster :: !(Maybe Cluster)
    , _mscrprsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifySnapshotCopyRetentionPeriodResponse' smart constructor.
modifySnapshotCopyRetentionPeriodResponse :: Int -> ModifySnapshotCopyRetentionPeriodResponse
modifySnapshotCopyRetentionPeriodResponse pStatus_ =
    ModifySnapshotCopyRetentionPeriodResponse'
    { _mscrprsCluster = Nothing
    , _mscrprsStatus = pStatus_
    }

-- | Undocumented member.
mscrprsCluster :: Lens' ModifySnapshotCopyRetentionPeriodResponse (Maybe Cluster)
mscrprsCluster = lens _mscrprsCluster (\ s a -> s{_mscrprsCluster = a});

-- | Undocumented member.
mscrprsStatus :: Lens' ModifySnapshotCopyRetentionPeriodResponse Int
mscrprsStatus = lens _mscrprsStatus (\ s a -> s{_mscrprsStatus = a});
