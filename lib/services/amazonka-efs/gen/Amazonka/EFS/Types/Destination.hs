{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EFS.Types.Destination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types.ReplicationStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination file system in the replication configuration.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | The time when the most recent sync was successfully completed on the
    -- destination file system. Any changes to data on the source file system
    -- that occurred before this time have been successfully replicated to the
    -- destination file system. Any changes that occurred after this time might
    -- not be fully replicated.
    lastReplicatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Describes the status of the destination Amazon EFS file system.
    --
    -- -   The @Paused@ state occurs as a result of opting out of the source or
    --     destination Region after the replication configuration was created.
    --     To resume replication for the file system, you need to again opt in
    --     to the Amazon Web Services Region. For more information, see
    --     <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-enable Managing Amazon Web Services Regions>
    --     in the /Amazon Web Services General Reference Guide/.
    --
    -- -   The @Error@ state occurs when either the source or the destination
    --     file system (or both) is in a failed state and is unrecoverable. For
    --     more information, see
    --     <https://docs.aws.amazon.com/efs/latest/ug/awsbackup.html#restoring-backup-efsmonitoring-replication-status.html Monitoring replication status>
    --     in the /Amazon EFS User Guide/. You must delete the replication
    --     configuration, and then restore the most recent backup of the failed
    --     file system (either the source or the destination) to a new file
    --     system.
    status :: ReplicationStatus,
    -- | The ID of the destination Amazon EFS file system.
    fileSystemId :: Prelude.Text,
    -- | The Amazon Web Services Region in which the destination file system is
    -- located.
    region :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastReplicatedTimestamp', 'destination_lastReplicatedTimestamp' - The time when the most recent sync was successfully completed on the
-- destination file system. Any changes to data on the source file system
-- that occurred before this time have been successfully replicated to the
-- destination file system. Any changes that occurred after this time might
-- not be fully replicated.
--
-- 'status', 'destination_status' - Describes the status of the destination Amazon EFS file system.
--
-- -   The @Paused@ state occurs as a result of opting out of the source or
--     destination Region after the replication configuration was created.
--     To resume replication for the file system, you need to again opt in
--     to the Amazon Web Services Region. For more information, see
--     <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-enable Managing Amazon Web Services Regions>
--     in the /Amazon Web Services General Reference Guide/.
--
-- -   The @Error@ state occurs when either the source or the destination
--     file system (or both) is in a failed state and is unrecoverable. For
--     more information, see
--     <https://docs.aws.amazon.com/efs/latest/ug/awsbackup.html#restoring-backup-efsmonitoring-replication-status.html Monitoring replication status>
--     in the /Amazon EFS User Guide/. You must delete the replication
--     configuration, and then restore the most recent backup of the failed
--     file system (either the source or the destination) to a new file
--     system.
--
-- 'fileSystemId', 'destination_fileSystemId' - The ID of the destination Amazon EFS file system.
--
-- 'region', 'destination_region' - The Amazon Web Services Region in which the destination file system is
-- located.
newDestination ::
  -- | 'status'
  ReplicationStatus ->
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'region'
  Prelude.Text ->
  Destination
newDestination pStatus_ pFileSystemId_ pRegion_ =
  Destination'
    { lastReplicatedTimestamp =
        Prelude.Nothing,
      status = pStatus_,
      fileSystemId = pFileSystemId_,
      region = pRegion_
    }

-- | The time when the most recent sync was successfully completed on the
-- destination file system. Any changes to data on the source file system
-- that occurred before this time have been successfully replicated to the
-- destination file system. Any changes that occurred after this time might
-- not be fully replicated.
destination_lastReplicatedTimestamp :: Lens.Lens' Destination (Prelude.Maybe Prelude.UTCTime)
destination_lastReplicatedTimestamp = Lens.lens (\Destination' {lastReplicatedTimestamp} -> lastReplicatedTimestamp) (\s@Destination' {} a -> s {lastReplicatedTimestamp = a} :: Destination) Prelude.. Lens.mapping Data._Time

-- | Describes the status of the destination Amazon EFS file system.
--
-- -   The @Paused@ state occurs as a result of opting out of the source or
--     destination Region after the replication configuration was created.
--     To resume replication for the file system, you need to again opt in
--     to the Amazon Web Services Region. For more information, see
--     <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html#rande-manage-enable Managing Amazon Web Services Regions>
--     in the /Amazon Web Services General Reference Guide/.
--
-- -   The @Error@ state occurs when either the source or the destination
--     file system (or both) is in a failed state and is unrecoverable. For
--     more information, see
--     <https://docs.aws.amazon.com/efs/latest/ug/awsbackup.html#restoring-backup-efsmonitoring-replication-status.html Monitoring replication status>
--     in the /Amazon EFS User Guide/. You must delete the replication
--     configuration, and then restore the most recent backup of the failed
--     file system (either the source or the destination) to a new file
--     system.
destination_status :: Lens.Lens' Destination ReplicationStatus
destination_status = Lens.lens (\Destination' {status} -> status) (\s@Destination' {} a -> s {status = a} :: Destination)

-- | The ID of the destination Amazon EFS file system.
destination_fileSystemId :: Lens.Lens' Destination Prelude.Text
destination_fileSystemId = Lens.lens (\Destination' {fileSystemId} -> fileSystemId) (\s@Destination' {} a -> s {fileSystemId = a} :: Destination)

-- | The Amazon Web Services Region in which the destination file system is
-- located.
destination_region :: Lens.Lens' Destination Prelude.Text
destination_region = Lens.lens (\Destination' {region} -> region) (\s@Destination' {} a -> s {region = a} :: Destination)

instance Data.FromJSON Destination where
  parseJSON =
    Data.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Data..:? "LastReplicatedTimestamp")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "FileSystemId")
            Prelude.<*> (x Data..: "Region")
      )

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt
      `Prelude.hashWithSalt` lastReplicatedTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` region

instance Prelude.NFData Destination where
  rnf Destination' {..} =
    Prelude.rnf lastReplicatedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf region
