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
-- Module      : Network.AWS.CloudHSMv2.Types.DestinationBackup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.DestinationBackup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the backup that will be copied and created by
-- the CopyBackupToRegion operation.
--
-- /See:/ 'newDestinationBackup' smart constructor.
data DestinationBackup = DestinationBackup'
  { -- | The identifier (ID) of the source backup from which the new backup was
    -- copied.
    sourceBackup :: Core.Maybe Core.Text,
    -- | The date and time when both the source backup was created.
    createTimestamp :: Core.Maybe Core.POSIX,
    -- | The identifier (ID) of the cluster containing the source backup from
    -- which the new backup was copied.
    sourceCluster :: Core.Maybe Core.Text,
    -- | The AWS region that contains the source backup from which the new backup
    -- was copied.
    sourceRegion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DestinationBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceBackup', 'destinationBackup_sourceBackup' - The identifier (ID) of the source backup from which the new backup was
-- copied.
--
-- 'createTimestamp', 'destinationBackup_createTimestamp' - The date and time when both the source backup was created.
--
-- 'sourceCluster', 'destinationBackup_sourceCluster' - The identifier (ID) of the cluster containing the source backup from
-- which the new backup was copied.
--
-- 'sourceRegion', 'destinationBackup_sourceRegion' - The AWS region that contains the source backup from which the new backup
-- was copied.
newDestinationBackup ::
  DestinationBackup
newDestinationBackup =
  DestinationBackup'
    { sourceBackup = Core.Nothing,
      createTimestamp = Core.Nothing,
      sourceCluster = Core.Nothing,
      sourceRegion = Core.Nothing
    }

-- | The identifier (ID) of the source backup from which the new backup was
-- copied.
destinationBackup_sourceBackup :: Lens.Lens' DestinationBackup (Core.Maybe Core.Text)
destinationBackup_sourceBackup = Lens.lens (\DestinationBackup' {sourceBackup} -> sourceBackup) (\s@DestinationBackup' {} a -> s {sourceBackup = a} :: DestinationBackup)

-- | The date and time when both the source backup was created.
destinationBackup_createTimestamp :: Lens.Lens' DestinationBackup (Core.Maybe Core.UTCTime)
destinationBackup_createTimestamp = Lens.lens (\DestinationBackup' {createTimestamp} -> createTimestamp) (\s@DestinationBackup' {} a -> s {createTimestamp = a} :: DestinationBackup) Core.. Lens.mapping Core._Time

-- | The identifier (ID) of the cluster containing the source backup from
-- which the new backup was copied.
destinationBackup_sourceCluster :: Lens.Lens' DestinationBackup (Core.Maybe Core.Text)
destinationBackup_sourceCluster = Lens.lens (\DestinationBackup' {sourceCluster} -> sourceCluster) (\s@DestinationBackup' {} a -> s {sourceCluster = a} :: DestinationBackup)

-- | The AWS region that contains the source backup from which the new backup
-- was copied.
destinationBackup_sourceRegion :: Lens.Lens' DestinationBackup (Core.Maybe Core.Text)
destinationBackup_sourceRegion = Lens.lens (\DestinationBackup' {sourceRegion} -> sourceRegion) (\s@DestinationBackup' {} a -> s {sourceRegion = a} :: DestinationBackup)

instance Core.FromJSON DestinationBackup where
  parseJSON =
    Core.withObject
      "DestinationBackup"
      ( \x ->
          DestinationBackup'
            Core.<$> (x Core..:? "SourceBackup")
            Core.<*> (x Core..:? "CreateTimestamp")
            Core.<*> (x Core..:? "SourceCluster")
            Core.<*> (x Core..:? "SourceRegion")
      )

instance Core.Hashable DestinationBackup

instance Core.NFData DestinationBackup
