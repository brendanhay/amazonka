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
-- Module      : Amazonka.EFS.Types.ReplicationConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.ReplicationConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types.Destination
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newReplicationConfigurationDescription' smart constructor.
data ReplicationConfigurationDescription = ReplicationConfigurationDescription'
  { -- | The ID of the source Amazon EFS file system that is being replicated.
    sourceFileSystemId :: Prelude.Text,
    -- | The Amazon Web Services Region in which the source Amazon EFS file
    -- system is located.
    sourceFileSystemRegion :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the current source file system in the
    -- replication configuration.
    sourceFileSystemArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the original source Amazon EFS file
    -- system in the replication configuration.
    originalSourceFileSystemArn :: Prelude.Text,
    -- | Describes when the replication configuration was created.
    creationTime :: Data.POSIX,
    -- | An array of destination objects. Only one destination object is
    -- supported.
    destinations :: [Destination]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFileSystemId', 'replicationConfigurationDescription_sourceFileSystemId' - The ID of the source Amazon EFS file system that is being replicated.
--
-- 'sourceFileSystemRegion', 'replicationConfigurationDescription_sourceFileSystemRegion' - The Amazon Web Services Region in which the source Amazon EFS file
-- system is located.
--
-- 'sourceFileSystemArn', 'replicationConfigurationDescription_sourceFileSystemArn' - The Amazon Resource Name (ARN) of the current source file system in the
-- replication configuration.
--
-- 'originalSourceFileSystemArn', 'replicationConfigurationDescription_originalSourceFileSystemArn' - The Amazon Resource Name (ARN) of the original source Amazon EFS file
-- system in the replication configuration.
--
-- 'creationTime', 'replicationConfigurationDescription_creationTime' - Describes when the replication configuration was created.
--
-- 'destinations', 'replicationConfigurationDescription_destinations' - An array of destination objects. Only one destination object is
-- supported.
newReplicationConfigurationDescription ::
  -- | 'sourceFileSystemId'
  Prelude.Text ->
  -- | 'sourceFileSystemRegion'
  Prelude.Text ->
  -- | 'sourceFileSystemArn'
  Prelude.Text ->
  -- | 'originalSourceFileSystemArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  ReplicationConfigurationDescription
newReplicationConfigurationDescription
  pSourceFileSystemId_
  pSourceFileSystemRegion_
  pSourceFileSystemArn_
  pOriginalSourceFileSystemArn_
  pCreationTime_ =
    ReplicationConfigurationDescription'
      { sourceFileSystemId =
          pSourceFileSystemId_,
        sourceFileSystemRegion =
          pSourceFileSystemRegion_,
        sourceFileSystemArn =
          pSourceFileSystemArn_,
        originalSourceFileSystemArn =
          pOriginalSourceFileSystemArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        destinations = Prelude.mempty
      }

-- | The ID of the source Amazon EFS file system that is being replicated.
replicationConfigurationDescription_sourceFileSystemId :: Lens.Lens' ReplicationConfigurationDescription Prelude.Text
replicationConfigurationDescription_sourceFileSystemId = Lens.lens (\ReplicationConfigurationDescription' {sourceFileSystemId} -> sourceFileSystemId) (\s@ReplicationConfigurationDescription' {} a -> s {sourceFileSystemId = a} :: ReplicationConfigurationDescription)

-- | The Amazon Web Services Region in which the source Amazon EFS file
-- system is located.
replicationConfigurationDescription_sourceFileSystemRegion :: Lens.Lens' ReplicationConfigurationDescription Prelude.Text
replicationConfigurationDescription_sourceFileSystemRegion = Lens.lens (\ReplicationConfigurationDescription' {sourceFileSystemRegion} -> sourceFileSystemRegion) (\s@ReplicationConfigurationDescription' {} a -> s {sourceFileSystemRegion = a} :: ReplicationConfigurationDescription)

-- | The Amazon Resource Name (ARN) of the current source file system in the
-- replication configuration.
replicationConfigurationDescription_sourceFileSystemArn :: Lens.Lens' ReplicationConfigurationDescription Prelude.Text
replicationConfigurationDescription_sourceFileSystemArn = Lens.lens (\ReplicationConfigurationDescription' {sourceFileSystemArn} -> sourceFileSystemArn) (\s@ReplicationConfigurationDescription' {} a -> s {sourceFileSystemArn = a} :: ReplicationConfigurationDescription)

-- | The Amazon Resource Name (ARN) of the original source Amazon EFS file
-- system in the replication configuration.
replicationConfigurationDescription_originalSourceFileSystemArn :: Lens.Lens' ReplicationConfigurationDescription Prelude.Text
replicationConfigurationDescription_originalSourceFileSystemArn = Lens.lens (\ReplicationConfigurationDescription' {originalSourceFileSystemArn} -> originalSourceFileSystemArn) (\s@ReplicationConfigurationDescription' {} a -> s {originalSourceFileSystemArn = a} :: ReplicationConfigurationDescription)

-- | Describes when the replication configuration was created.
replicationConfigurationDescription_creationTime :: Lens.Lens' ReplicationConfigurationDescription Prelude.UTCTime
replicationConfigurationDescription_creationTime = Lens.lens (\ReplicationConfigurationDescription' {creationTime} -> creationTime) (\s@ReplicationConfigurationDescription' {} a -> s {creationTime = a} :: ReplicationConfigurationDescription) Prelude.. Data._Time

-- | An array of destination objects. Only one destination object is
-- supported.
replicationConfigurationDescription_destinations :: Lens.Lens' ReplicationConfigurationDescription [Destination]
replicationConfigurationDescription_destinations = Lens.lens (\ReplicationConfigurationDescription' {destinations} -> destinations) (\s@ReplicationConfigurationDescription' {} a -> s {destinations = a} :: ReplicationConfigurationDescription) Prelude.. Lens.coerced

instance
  Data.FromJSON
    ReplicationConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "ReplicationConfigurationDescription"
      ( \x ->
          ReplicationConfigurationDescription'
            Prelude.<$> (x Data..: "SourceFileSystemId")
            Prelude.<*> (x Data..: "SourceFileSystemRegion")
            Prelude.<*> (x Data..: "SourceFileSystemArn")
            Prelude.<*> (x Data..: "OriginalSourceFileSystemArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..:? "Destinations" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ReplicationConfigurationDescription
  where
  hashWithSalt
    _salt
    ReplicationConfigurationDescription' {..} =
      _salt `Prelude.hashWithSalt` sourceFileSystemId
        `Prelude.hashWithSalt` sourceFileSystemRegion
        `Prelude.hashWithSalt` sourceFileSystemArn
        `Prelude.hashWithSalt` originalSourceFileSystemArn
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` destinations

instance
  Prelude.NFData
    ReplicationConfigurationDescription
  where
  rnf ReplicationConfigurationDescription' {..} =
    Prelude.rnf sourceFileSystemId
      `Prelude.seq` Prelude.rnf sourceFileSystemRegion
      `Prelude.seq` Prelude.rnf sourceFileSystemArn
      `Prelude.seq` Prelude.rnf originalSourceFileSystemArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinations
