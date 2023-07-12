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
-- Module      : Amazonka.SSMIncidents.Types.ReplicationSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ReplicationSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.RegionInfo
import Amazonka.SSMIncidents.Types.ReplicationSetStatus

-- | The set of Amazon Web Services Region that your Incident Manager data
-- will be replicated to and the KMS key used to encrypt the data.
--
-- /See:/ 'newReplicationSet' smart constructor.
data ReplicationSet = ReplicationSet'
  { -- | The Amazon Resource Name (ARN) of the replication set.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Details about who created the replication set.
    createdBy :: Prelude.Text,
    -- | When the replication set was created.
    createdTime :: Data.POSIX,
    -- | Determines if the replication set deletion protection is enabled or not.
    -- If deletion protection is enabled, you can\'t delete the last Amazon Web
    -- Services Region in the replication set.
    deletionProtected :: Prelude.Bool,
    -- | Who last modified the replication set.
    lastModifiedBy :: Prelude.Text,
    -- | When the replication set was last updated.
    lastModifiedTime :: Data.POSIX,
    -- | The map between each Amazon Web Services Region in your replication set
    -- and the KMS key that\'s used to encrypt the data in that Region.
    regionMap :: Prelude.HashMap Prelude.Text RegionInfo,
    -- | The status of the replication set. If the replication set is still
    -- pending, you can\'t use Incident Manager functionality.
    status :: ReplicationSetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'replicationSet_arn' - The Amazon Resource Name (ARN) of the replication set.
--
-- 'createdBy', 'replicationSet_createdBy' - Details about who created the replication set.
--
-- 'createdTime', 'replicationSet_createdTime' - When the replication set was created.
--
-- 'deletionProtected', 'replicationSet_deletionProtected' - Determines if the replication set deletion protection is enabled or not.
-- If deletion protection is enabled, you can\'t delete the last Amazon Web
-- Services Region in the replication set.
--
-- 'lastModifiedBy', 'replicationSet_lastModifiedBy' - Who last modified the replication set.
--
-- 'lastModifiedTime', 'replicationSet_lastModifiedTime' - When the replication set was last updated.
--
-- 'regionMap', 'replicationSet_regionMap' - The map between each Amazon Web Services Region in your replication set
-- and the KMS key that\'s used to encrypt the data in that Region.
--
-- 'status', 'replicationSet_status' - The status of the replication set. If the replication set is still
-- pending, you can\'t use Incident Manager functionality.
newReplicationSet ::
  -- | 'createdBy'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'deletionProtected'
  Prelude.Bool ->
  -- | 'lastModifiedBy'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'status'
  ReplicationSetStatus ->
  ReplicationSet
newReplicationSet
  pCreatedBy_
  pCreatedTime_
  pDeletionProtected_
  pLastModifiedBy_
  pLastModifiedTime_
  pStatus_ =
    ReplicationSet'
      { arn = Prelude.Nothing,
        createdBy = pCreatedBy_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        deletionProtected = pDeletionProtected_,
        lastModifiedBy = pLastModifiedBy_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        regionMap = Prelude.mempty,
        status = pStatus_
      }

-- | The Amazon Resource Name (ARN) of the replication set.
replicationSet_arn :: Lens.Lens' ReplicationSet (Prelude.Maybe Prelude.Text)
replicationSet_arn = Lens.lens (\ReplicationSet' {arn} -> arn) (\s@ReplicationSet' {} a -> s {arn = a} :: ReplicationSet)

-- | Details about who created the replication set.
replicationSet_createdBy :: Lens.Lens' ReplicationSet Prelude.Text
replicationSet_createdBy = Lens.lens (\ReplicationSet' {createdBy} -> createdBy) (\s@ReplicationSet' {} a -> s {createdBy = a} :: ReplicationSet)

-- | When the replication set was created.
replicationSet_createdTime :: Lens.Lens' ReplicationSet Prelude.UTCTime
replicationSet_createdTime = Lens.lens (\ReplicationSet' {createdTime} -> createdTime) (\s@ReplicationSet' {} a -> s {createdTime = a} :: ReplicationSet) Prelude.. Data._Time

-- | Determines if the replication set deletion protection is enabled or not.
-- If deletion protection is enabled, you can\'t delete the last Amazon Web
-- Services Region in the replication set.
replicationSet_deletionProtected :: Lens.Lens' ReplicationSet Prelude.Bool
replicationSet_deletionProtected = Lens.lens (\ReplicationSet' {deletionProtected} -> deletionProtected) (\s@ReplicationSet' {} a -> s {deletionProtected = a} :: ReplicationSet)

-- | Who last modified the replication set.
replicationSet_lastModifiedBy :: Lens.Lens' ReplicationSet Prelude.Text
replicationSet_lastModifiedBy = Lens.lens (\ReplicationSet' {lastModifiedBy} -> lastModifiedBy) (\s@ReplicationSet' {} a -> s {lastModifiedBy = a} :: ReplicationSet)

-- | When the replication set was last updated.
replicationSet_lastModifiedTime :: Lens.Lens' ReplicationSet Prelude.UTCTime
replicationSet_lastModifiedTime = Lens.lens (\ReplicationSet' {lastModifiedTime} -> lastModifiedTime) (\s@ReplicationSet' {} a -> s {lastModifiedTime = a} :: ReplicationSet) Prelude.. Data._Time

-- | The map between each Amazon Web Services Region in your replication set
-- and the KMS key that\'s used to encrypt the data in that Region.
replicationSet_regionMap :: Lens.Lens' ReplicationSet (Prelude.HashMap Prelude.Text RegionInfo)
replicationSet_regionMap = Lens.lens (\ReplicationSet' {regionMap} -> regionMap) (\s@ReplicationSet' {} a -> s {regionMap = a} :: ReplicationSet) Prelude.. Lens.coerced

-- | The status of the replication set. If the replication set is still
-- pending, you can\'t use Incident Manager functionality.
replicationSet_status :: Lens.Lens' ReplicationSet ReplicationSetStatus
replicationSet_status = Lens.lens (\ReplicationSet' {status} -> status) (\s@ReplicationSet' {} a -> s {status = a} :: ReplicationSet)

instance Data.FromJSON ReplicationSet where
  parseJSON =
    Data.withObject
      "ReplicationSet"
      ( \x ->
          ReplicationSet'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..: "createdBy")
            Prelude.<*> (x Data..: "createdTime")
            Prelude.<*> (x Data..: "deletionProtected")
            Prelude.<*> (x Data..: "lastModifiedBy")
            Prelude.<*> (x Data..: "lastModifiedTime")
            Prelude.<*> (x Data..:? "regionMap" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ReplicationSet where
  hashWithSalt _salt ReplicationSet' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` deletionProtected
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` regionMap
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReplicationSet where
  rnf ReplicationSet' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf deletionProtected
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf regionMap
      `Prelude.seq` Prelude.rnf status
