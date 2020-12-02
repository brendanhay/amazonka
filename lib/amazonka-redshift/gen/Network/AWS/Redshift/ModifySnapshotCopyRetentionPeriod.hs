{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of days to retain snapshots in the destination AWS Region after they are copied from the source AWS Region. By default, this operation only changes the retention period of copied automated snapshots. The retention periods for both new and existing copied automated snapshots are updated with the new retention period. You can set the manual option to change only the retention periods of copied manual snapshots. If you set this option, only newly copied manual snapshots have the new retention period.
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
  ( -- * Creating a Request
    modifySnapshotCopyRetentionPeriod,
    ModifySnapshotCopyRetentionPeriod,

    -- * Request Lenses
    mscrpManual,
    mscrpClusterIdentifier,
    mscrpRetentionPeriod,

    -- * Destructuring the Response
    modifySnapshotCopyRetentionPeriodResponse,
    ModifySnapshotCopyRetentionPeriodResponse,

    -- * Response Lenses
    mscrprsCluster,
    mscrprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifySnapshotCopyRetentionPeriod' smart constructor.
data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod'
  { _mscrpManual ::
      !(Maybe Bool),
    _mscrpClusterIdentifier ::
      !Text,
    _mscrpRetentionPeriod ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifySnapshotCopyRetentionPeriod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mscrpManual' - Indicates whether to apply the snapshot retention period to newly copied manual snapshots instead of automated snapshots.
--
-- * 'mscrpClusterIdentifier' - The unique identifier of the cluster for which you want to change the retention period for either automated or manual snapshots that are copied to a destination AWS Region. Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
--
-- * 'mscrpRetentionPeriod' - The number of days to retain automated snapshots in the destination AWS Region after they are copied from the source AWS Region. By default, this only changes the retention period of copied automated snapshots.  If you decrease the retention period for automated snapshots that are copied to a destination AWS Region, Amazon Redshift deletes any existing automated snapshots that were copied to the destination AWS Region and that fall outside of the new retention period. Constraints: Must be at least 1 and no more than 35 for automated snapshots.  If you specify the @manual@ option, only newly copied manual snapshots will have the new retention period.  If you specify the value of -1 newly copied manual snapshots are retained indefinitely. Constraints: The number of days must be either -1 or an integer between 1 and 3,653 for manual snapshots.
modifySnapshotCopyRetentionPeriod ::
  -- | 'mscrpClusterIdentifier'
  Text ->
  -- | 'mscrpRetentionPeriod'
  Int ->
  ModifySnapshotCopyRetentionPeriod
modifySnapshotCopyRetentionPeriod
  pClusterIdentifier_
  pRetentionPeriod_ =
    ModifySnapshotCopyRetentionPeriod'
      { _mscrpManual = Nothing,
        _mscrpClusterIdentifier = pClusterIdentifier_,
        _mscrpRetentionPeriod = pRetentionPeriod_
      }

-- | Indicates whether to apply the snapshot retention period to newly copied manual snapshots instead of automated snapshots.
mscrpManual :: Lens' ModifySnapshotCopyRetentionPeriod (Maybe Bool)
mscrpManual = lens _mscrpManual (\s a -> s {_mscrpManual = a})

-- | The unique identifier of the cluster for which you want to change the retention period for either automated or manual snapshots that are copied to a destination AWS Region. Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
mscrpClusterIdentifier :: Lens' ModifySnapshotCopyRetentionPeriod Text
mscrpClusterIdentifier = lens _mscrpClusterIdentifier (\s a -> s {_mscrpClusterIdentifier = a})

-- | The number of days to retain automated snapshots in the destination AWS Region after they are copied from the source AWS Region. By default, this only changes the retention period of copied automated snapshots.  If you decrease the retention period for automated snapshots that are copied to a destination AWS Region, Amazon Redshift deletes any existing automated snapshots that were copied to the destination AWS Region and that fall outside of the new retention period. Constraints: Must be at least 1 and no more than 35 for automated snapshots.  If you specify the @manual@ option, only newly copied manual snapshots will have the new retention period.  If you specify the value of -1 newly copied manual snapshots are retained indefinitely. Constraints: The number of days must be either -1 or an integer between 1 and 3,653 for manual snapshots.
mscrpRetentionPeriod :: Lens' ModifySnapshotCopyRetentionPeriod Int
mscrpRetentionPeriod = lens _mscrpRetentionPeriod (\s a -> s {_mscrpRetentionPeriod = a})

instance AWSRequest ModifySnapshotCopyRetentionPeriod where
  type
    Rs ModifySnapshotCopyRetentionPeriod =
      ModifySnapshotCopyRetentionPeriodResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "ModifySnapshotCopyRetentionPeriodResult"
      ( \s h x ->
          ModifySnapshotCopyRetentionPeriodResponse'
            <$> (x .@? "Cluster") <*> (pure (fromEnum s))
      )

instance Hashable ModifySnapshotCopyRetentionPeriod

instance NFData ModifySnapshotCopyRetentionPeriod

instance ToHeaders ModifySnapshotCopyRetentionPeriod where
  toHeaders = const mempty

instance ToPath ModifySnapshotCopyRetentionPeriod where
  toPath = const "/"

instance ToQuery ModifySnapshotCopyRetentionPeriod where
  toQuery ModifySnapshotCopyRetentionPeriod' {..} =
    mconcat
      [ "Action" =: ("ModifySnapshotCopyRetentionPeriod" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "Manual" =: _mscrpManual,
        "ClusterIdentifier" =: _mscrpClusterIdentifier,
        "RetentionPeriod" =: _mscrpRetentionPeriod
      ]

-- | /See:/ 'modifySnapshotCopyRetentionPeriodResponse' smart constructor.
data ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse'
  { _mscrprsCluster ::
      !( Maybe
           Cluster
       ),
    _mscrprsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ModifySnapshotCopyRetentionPeriodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mscrprsCluster' - Undocumented member.
--
-- * 'mscrprsResponseStatus' - -- | The response status code.
modifySnapshotCopyRetentionPeriodResponse ::
  -- | 'mscrprsResponseStatus'
  Int ->
  ModifySnapshotCopyRetentionPeriodResponse
modifySnapshotCopyRetentionPeriodResponse pResponseStatus_ =
  ModifySnapshotCopyRetentionPeriodResponse'
    { _mscrprsCluster =
        Nothing,
      _mscrprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mscrprsCluster :: Lens' ModifySnapshotCopyRetentionPeriodResponse (Maybe Cluster)
mscrprsCluster = lens _mscrprsCluster (\s a -> s {_mscrprsCluster = a})

-- | -- | The response status code.
mscrprsResponseStatus :: Lens' ModifySnapshotCopyRetentionPeriodResponse Int
mscrprsResponseStatus = lens _mscrprsResponseStatus (\s a -> s {_mscrprsResponseStatus = a})

instance NFData ModifySnapshotCopyRetentionPeriodResponse
