{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Returns the destination region and retention period that are configured for cross-region snapshot copy.
--
--
--
-- /See:/ 'clusterSnapshotCopyStatus' smart constructor.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus'
  { _cscsManualSnapshotRetentionPeriod ::
      !(Maybe Int),
    _cscsRetentionPeriod ::
      !(Maybe Integer),
    _cscsDestinationRegion :: !(Maybe Text),
    _cscsSnapshotCopyGrantName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterSnapshotCopyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscsManualSnapshotRetentionPeriod' - The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely.  The value must be either -1 or an integer between 1 and 3,653.
--
-- * 'cscsRetentionPeriod' - The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
--
-- * 'cscsDestinationRegion' - The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
--
-- * 'cscsSnapshotCopyGrantName' - The name of the snapshot copy grant.
clusterSnapshotCopyStatus ::
  ClusterSnapshotCopyStatus
clusterSnapshotCopyStatus =
  ClusterSnapshotCopyStatus'
    { _cscsManualSnapshotRetentionPeriod =
        Nothing,
      _cscsRetentionPeriod = Nothing,
      _cscsDestinationRegion = Nothing,
      _cscsSnapshotCopyGrantName = Nothing
    }

-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely.  The value must be either -1 or an integer between 1 and 3,653.
cscsManualSnapshotRetentionPeriod :: Lens' ClusterSnapshotCopyStatus (Maybe Int)
cscsManualSnapshotRetentionPeriod = lens _cscsManualSnapshotRetentionPeriod (\s a -> s {_cscsManualSnapshotRetentionPeriod = a})

-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
cscsRetentionPeriod :: Lens' ClusterSnapshotCopyStatus (Maybe Integer)
cscsRetentionPeriod = lens _cscsRetentionPeriod (\s a -> s {_cscsRetentionPeriod = a})

-- | The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
cscsDestinationRegion :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsDestinationRegion = lens _cscsDestinationRegion (\s a -> s {_cscsDestinationRegion = a})

-- | The name of the snapshot copy grant.
cscsSnapshotCopyGrantName :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsSnapshotCopyGrantName = lens _cscsSnapshotCopyGrantName (\s a -> s {_cscsSnapshotCopyGrantName = a})

instance FromXML ClusterSnapshotCopyStatus where
  parseXML x =
    ClusterSnapshotCopyStatus'
      <$> (x .@? "ManualSnapshotRetentionPeriod")
      <*> (x .@? "RetentionPeriod")
      <*> (x .@? "DestinationRegion")
      <*> (x .@? "SnapshotCopyGrantName")

instance Hashable ClusterSnapshotCopyStatus

instance NFData ClusterSnapshotCopyStatus
