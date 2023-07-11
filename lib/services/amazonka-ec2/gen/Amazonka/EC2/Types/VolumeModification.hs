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
-- Module      : Amazonka.EC2.Types.VolumeModification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeModification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VolumeModificationState
import Amazonka.EC2.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Describes the modification status of an EBS volume.
--
-- If the volume has never been modified, some element values will be null.
--
-- /See:/ 'newVolumeModification' smart constructor.
data VolumeModification = VolumeModification'
  { -- | The modification completion or failure time.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The current modification state. The modification state is null for
    -- unmodified volumes.
    modificationState :: Prelude.Maybe VolumeModificationState,
    -- | The original IOPS rate of the volume.
    originalIops :: Prelude.Maybe Prelude.Int,
    -- | The original setting for Amazon EBS Multi-Attach.
    originalMultiAttachEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The original size of the volume, in GiB.
    originalSize :: Prelude.Maybe Prelude.Int,
    -- | The original throughput of the volume, in MiB\/s.
    originalThroughput :: Prelude.Maybe Prelude.Int,
    -- | The original EBS volume type of the volume.
    originalVolumeType :: Prelude.Maybe VolumeType,
    -- | The modification progress, from 0 to 100 percent complete.
    progress :: Prelude.Maybe Prelude.Integer,
    -- | The modification start time.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | A status message about the modification progress or failure.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The target IOPS rate of the volume.
    targetIops :: Prelude.Maybe Prelude.Int,
    -- | The target setting for Amazon EBS Multi-Attach.
    targetMultiAttachEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The target size of the volume, in GiB.
    targetSize :: Prelude.Maybe Prelude.Int,
    -- | The target throughput of the volume, in MiB\/s.
    targetThroughput :: Prelude.Maybe Prelude.Int,
    -- | The target EBS volume type of the volume.
    targetVolumeType :: Prelude.Maybe VolumeType,
    -- | The ID of the volume.
    volumeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeModification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'volumeModification_endTime' - The modification completion or failure time.
--
-- 'modificationState', 'volumeModification_modificationState' - The current modification state. The modification state is null for
-- unmodified volumes.
--
-- 'originalIops', 'volumeModification_originalIops' - The original IOPS rate of the volume.
--
-- 'originalMultiAttachEnabled', 'volumeModification_originalMultiAttachEnabled' - The original setting for Amazon EBS Multi-Attach.
--
-- 'originalSize', 'volumeModification_originalSize' - The original size of the volume, in GiB.
--
-- 'originalThroughput', 'volumeModification_originalThroughput' - The original throughput of the volume, in MiB\/s.
--
-- 'originalVolumeType', 'volumeModification_originalVolumeType' - The original EBS volume type of the volume.
--
-- 'progress', 'volumeModification_progress' - The modification progress, from 0 to 100 percent complete.
--
-- 'startTime', 'volumeModification_startTime' - The modification start time.
--
-- 'statusMessage', 'volumeModification_statusMessage' - A status message about the modification progress or failure.
--
-- 'targetIops', 'volumeModification_targetIops' - The target IOPS rate of the volume.
--
-- 'targetMultiAttachEnabled', 'volumeModification_targetMultiAttachEnabled' - The target setting for Amazon EBS Multi-Attach.
--
-- 'targetSize', 'volumeModification_targetSize' - The target size of the volume, in GiB.
--
-- 'targetThroughput', 'volumeModification_targetThroughput' - The target throughput of the volume, in MiB\/s.
--
-- 'targetVolumeType', 'volumeModification_targetVolumeType' - The target EBS volume type of the volume.
--
-- 'volumeId', 'volumeModification_volumeId' - The ID of the volume.
newVolumeModification ::
  VolumeModification
newVolumeModification =
  VolumeModification'
    { endTime = Prelude.Nothing,
      modificationState = Prelude.Nothing,
      originalIops = Prelude.Nothing,
      originalMultiAttachEnabled = Prelude.Nothing,
      originalSize = Prelude.Nothing,
      originalThroughput = Prelude.Nothing,
      originalVolumeType = Prelude.Nothing,
      progress = Prelude.Nothing,
      startTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      targetIops = Prelude.Nothing,
      targetMultiAttachEnabled = Prelude.Nothing,
      targetSize = Prelude.Nothing,
      targetThroughput = Prelude.Nothing,
      targetVolumeType = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | The modification completion or failure time.
volumeModification_endTime :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.UTCTime)
volumeModification_endTime = Lens.lens (\VolumeModification' {endTime} -> endTime) (\s@VolumeModification' {} a -> s {endTime = a} :: VolumeModification) Prelude.. Lens.mapping Data._Time

-- | The current modification state. The modification state is null for
-- unmodified volumes.
volumeModification_modificationState :: Lens.Lens' VolumeModification (Prelude.Maybe VolumeModificationState)
volumeModification_modificationState = Lens.lens (\VolumeModification' {modificationState} -> modificationState) (\s@VolumeModification' {} a -> s {modificationState = a} :: VolumeModification)

-- | The original IOPS rate of the volume.
volumeModification_originalIops :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_originalIops = Lens.lens (\VolumeModification' {originalIops} -> originalIops) (\s@VolumeModification' {} a -> s {originalIops = a} :: VolumeModification)

-- | The original setting for Amazon EBS Multi-Attach.
volumeModification_originalMultiAttachEnabled :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Bool)
volumeModification_originalMultiAttachEnabled = Lens.lens (\VolumeModification' {originalMultiAttachEnabled} -> originalMultiAttachEnabled) (\s@VolumeModification' {} a -> s {originalMultiAttachEnabled = a} :: VolumeModification)

-- | The original size of the volume, in GiB.
volumeModification_originalSize :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_originalSize = Lens.lens (\VolumeModification' {originalSize} -> originalSize) (\s@VolumeModification' {} a -> s {originalSize = a} :: VolumeModification)

-- | The original throughput of the volume, in MiB\/s.
volumeModification_originalThroughput :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_originalThroughput = Lens.lens (\VolumeModification' {originalThroughput} -> originalThroughput) (\s@VolumeModification' {} a -> s {originalThroughput = a} :: VolumeModification)

-- | The original EBS volume type of the volume.
volumeModification_originalVolumeType :: Lens.Lens' VolumeModification (Prelude.Maybe VolumeType)
volumeModification_originalVolumeType = Lens.lens (\VolumeModification' {originalVolumeType} -> originalVolumeType) (\s@VolumeModification' {} a -> s {originalVolumeType = a} :: VolumeModification)

-- | The modification progress, from 0 to 100 percent complete.
volumeModification_progress :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Integer)
volumeModification_progress = Lens.lens (\VolumeModification' {progress} -> progress) (\s@VolumeModification' {} a -> s {progress = a} :: VolumeModification)

-- | The modification start time.
volumeModification_startTime :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.UTCTime)
volumeModification_startTime = Lens.lens (\VolumeModification' {startTime} -> startTime) (\s@VolumeModification' {} a -> s {startTime = a} :: VolumeModification) Prelude.. Lens.mapping Data._Time

-- | A status message about the modification progress or failure.
volumeModification_statusMessage :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Text)
volumeModification_statusMessage = Lens.lens (\VolumeModification' {statusMessage} -> statusMessage) (\s@VolumeModification' {} a -> s {statusMessage = a} :: VolumeModification)

-- | The target IOPS rate of the volume.
volumeModification_targetIops :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_targetIops = Lens.lens (\VolumeModification' {targetIops} -> targetIops) (\s@VolumeModification' {} a -> s {targetIops = a} :: VolumeModification)

-- | The target setting for Amazon EBS Multi-Attach.
volumeModification_targetMultiAttachEnabled :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Bool)
volumeModification_targetMultiAttachEnabled = Lens.lens (\VolumeModification' {targetMultiAttachEnabled} -> targetMultiAttachEnabled) (\s@VolumeModification' {} a -> s {targetMultiAttachEnabled = a} :: VolumeModification)

-- | The target size of the volume, in GiB.
volumeModification_targetSize :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_targetSize = Lens.lens (\VolumeModification' {targetSize} -> targetSize) (\s@VolumeModification' {} a -> s {targetSize = a} :: VolumeModification)

-- | The target throughput of the volume, in MiB\/s.
volumeModification_targetThroughput :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_targetThroughput = Lens.lens (\VolumeModification' {targetThroughput} -> targetThroughput) (\s@VolumeModification' {} a -> s {targetThroughput = a} :: VolumeModification)

-- | The target EBS volume type of the volume.
volumeModification_targetVolumeType :: Lens.Lens' VolumeModification (Prelude.Maybe VolumeType)
volumeModification_targetVolumeType = Lens.lens (\VolumeModification' {targetVolumeType} -> targetVolumeType) (\s@VolumeModification' {} a -> s {targetVolumeType = a} :: VolumeModification)

-- | The ID of the volume.
volumeModification_volumeId :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Text)
volumeModification_volumeId = Lens.lens (\VolumeModification' {volumeId} -> volumeId) (\s@VolumeModification' {} a -> s {volumeId = a} :: VolumeModification)

instance Data.FromXML VolumeModification where
  parseXML x =
    VolumeModification'
      Prelude.<$> (x Data..@? "endTime")
      Prelude.<*> (x Data..@? "modificationState")
      Prelude.<*> (x Data..@? "originalIops")
      Prelude.<*> (x Data..@? "originalMultiAttachEnabled")
      Prelude.<*> (x Data..@? "originalSize")
      Prelude.<*> (x Data..@? "originalThroughput")
      Prelude.<*> (x Data..@? "originalVolumeType")
      Prelude.<*> (x Data..@? "progress")
      Prelude.<*> (x Data..@? "startTime")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> (x Data..@? "targetIops")
      Prelude.<*> (x Data..@? "targetMultiAttachEnabled")
      Prelude.<*> (x Data..@? "targetSize")
      Prelude.<*> (x Data..@? "targetThroughput")
      Prelude.<*> (x Data..@? "targetVolumeType")
      Prelude.<*> (x Data..@? "volumeId")

instance Prelude.Hashable VolumeModification where
  hashWithSalt _salt VolumeModification' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` modificationState
      `Prelude.hashWithSalt` originalIops
      `Prelude.hashWithSalt` originalMultiAttachEnabled
      `Prelude.hashWithSalt` originalSize
      `Prelude.hashWithSalt` originalThroughput
      `Prelude.hashWithSalt` originalVolumeType
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` targetIops
      `Prelude.hashWithSalt` targetMultiAttachEnabled
      `Prelude.hashWithSalt` targetSize
      `Prelude.hashWithSalt` targetThroughput
      `Prelude.hashWithSalt` targetVolumeType
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData VolumeModification where
  rnf VolumeModification' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf modificationState
      `Prelude.seq` Prelude.rnf originalIops
      `Prelude.seq` Prelude.rnf originalMultiAttachEnabled
      `Prelude.seq` Prelude.rnf originalSize
      `Prelude.seq` Prelude.rnf originalThroughput
      `Prelude.seq` Prelude.rnf originalVolumeType
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf targetIops
      `Prelude.seq` Prelude.rnf targetMultiAttachEnabled
      `Prelude.seq` Prelude.rnf targetSize
      `Prelude.seq` Prelude.rnf targetThroughput
      `Prelude.seq` Prelude.rnf targetVolumeType
      `Prelude.seq` Prelude.rnf volumeId
