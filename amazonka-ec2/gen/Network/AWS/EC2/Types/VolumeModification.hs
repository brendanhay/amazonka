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
-- Module      : Network.AWS.EC2.Types.VolumeModification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeModification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeModificationState
import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens

-- | Describes the modification status of an EBS volume.
--
-- If the volume has never been modified, some element values will be null.
--
-- /See:/ 'newVolumeModification' smart constructor.
data VolumeModification = VolumeModification'
  { -- | A status message about the modification progress or failure.
    statusMessage :: Core.Maybe Core.Text,
    -- | The target EBS volume type of the volume.
    targetVolumeType :: Core.Maybe VolumeType,
    -- | The original EBS volume type of the volume.
    originalVolumeType :: Core.Maybe VolumeType,
    -- | The modification start time.
    startTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the volume.
    volumeId :: Core.Maybe Core.Text,
    -- | The modification completion or failure time.
    endTime :: Core.Maybe Core.ISO8601,
    -- | The original IOPS rate of the volume.
    originalIops :: Core.Maybe Core.Int,
    -- | The target size of the volume, in GiB.
    targetSize :: Core.Maybe Core.Int,
    -- | The original size of the volume, in GiB.
    originalSize :: Core.Maybe Core.Int,
    -- | The target IOPS rate of the volume.
    targetIops :: Core.Maybe Core.Int,
    -- | The current modification state. The modification state is null for
    -- unmodified volumes.
    modificationState :: Core.Maybe VolumeModificationState,
    -- | The target setting for Amazon EBS Multi-Attach.
    targetMultiAttachEnabled :: Core.Maybe Core.Bool,
    -- | The original setting for Amazon EBS Multi-Attach.
    originalMultiAttachEnabled :: Core.Maybe Core.Bool,
    -- | The target throughput of the volume, in MiB\/s.
    targetThroughput :: Core.Maybe Core.Int,
    -- | The original throughput of the volume, in MiB\/s.
    originalThroughput :: Core.Maybe Core.Int,
    -- | The modification progress, from 0 to 100 percent complete.
    progress :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeModification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'volumeModification_statusMessage' - A status message about the modification progress or failure.
--
-- 'targetVolumeType', 'volumeModification_targetVolumeType' - The target EBS volume type of the volume.
--
-- 'originalVolumeType', 'volumeModification_originalVolumeType' - The original EBS volume type of the volume.
--
-- 'startTime', 'volumeModification_startTime' - The modification start time.
--
-- 'volumeId', 'volumeModification_volumeId' - The ID of the volume.
--
-- 'endTime', 'volumeModification_endTime' - The modification completion or failure time.
--
-- 'originalIops', 'volumeModification_originalIops' - The original IOPS rate of the volume.
--
-- 'targetSize', 'volumeModification_targetSize' - The target size of the volume, in GiB.
--
-- 'originalSize', 'volumeModification_originalSize' - The original size of the volume, in GiB.
--
-- 'targetIops', 'volumeModification_targetIops' - The target IOPS rate of the volume.
--
-- 'modificationState', 'volumeModification_modificationState' - The current modification state. The modification state is null for
-- unmodified volumes.
--
-- 'targetMultiAttachEnabled', 'volumeModification_targetMultiAttachEnabled' - The target setting for Amazon EBS Multi-Attach.
--
-- 'originalMultiAttachEnabled', 'volumeModification_originalMultiAttachEnabled' - The original setting for Amazon EBS Multi-Attach.
--
-- 'targetThroughput', 'volumeModification_targetThroughput' - The target throughput of the volume, in MiB\/s.
--
-- 'originalThroughput', 'volumeModification_originalThroughput' - The original throughput of the volume, in MiB\/s.
--
-- 'progress', 'volumeModification_progress' - The modification progress, from 0 to 100 percent complete.
newVolumeModification ::
  VolumeModification
newVolumeModification =
  VolumeModification'
    { statusMessage = Core.Nothing,
      targetVolumeType = Core.Nothing,
      originalVolumeType = Core.Nothing,
      startTime = Core.Nothing,
      volumeId = Core.Nothing,
      endTime = Core.Nothing,
      originalIops = Core.Nothing,
      targetSize = Core.Nothing,
      originalSize = Core.Nothing,
      targetIops = Core.Nothing,
      modificationState = Core.Nothing,
      targetMultiAttachEnabled = Core.Nothing,
      originalMultiAttachEnabled = Core.Nothing,
      targetThroughput = Core.Nothing,
      originalThroughput = Core.Nothing,
      progress = Core.Nothing
    }

-- | A status message about the modification progress or failure.
volumeModification_statusMessage :: Lens.Lens' VolumeModification (Core.Maybe Core.Text)
volumeModification_statusMessage = Lens.lens (\VolumeModification' {statusMessage} -> statusMessage) (\s@VolumeModification' {} a -> s {statusMessage = a} :: VolumeModification)

-- | The target EBS volume type of the volume.
volumeModification_targetVolumeType :: Lens.Lens' VolumeModification (Core.Maybe VolumeType)
volumeModification_targetVolumeType = Lens.lens (\VolumeModification' {targetVolumeType} -> targetVolumeType) (\s@VolumeModification' {} a -> s {targetVolumeType = a} :: VolumeModification)

-- | The original EBS volume type of the volume.
volumeModification_originalVolumeType :: Lens.Lens' VolumeModification (Core.Maybe VolumeType)
volumeModification_originalVolumeType = Lens.lens (\VolumeModification' {originalVolumeType} -> originalVolumeType) (\s@VolumeModification' {} a -> s {originalVolumeType = a} :: VolumeModification)

-- | The modification start time.
volumeModification_startTime :: Lens.Lens' VolumeModification (Core.Maybe Core.UTCTime)
volumeModification_startTime = Lens.lens (\VolumeModification' {startTime} -> startTime) (\s@VolumeModification' {} a -> s {startTime = a} :: VolumeModification) Core.. Lens.mapping Core._Time

-- | The ID of the volume.
volumeModification_volumeId :: Lens.Lens' VolumeModification (Core.Maybe Core.Text)
volumeModification_volumeId = Lens.lens (\VolumeModification' {volumeId} -> volumeId) (\s@VolumeModification' {} a -> s {volumeId = a} :: VolumeModification)

-- | The modification completion or failure time.
volumeModification_endTime :: Lens.Lens' VolumeModification (Core.Maybe Core.UTCTime)
volumeModification_endTime = Lens.lens (\VolumeModification' {endTime} -> endTime) (\s@VolumeModification' {} a -> s {endTime = a} :: VolumeModification) Core.. Lens.mapping Core._Time

-- | The original IOPS rate of the volume.
volumeModification_originalIops :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
volumeModification_originalIops = Lens.lens (\VolumeModification' {originalIops} -> originalIops) (\s@VolumeModification' {} a -> s {originalIops = a} :: VolumeModification)

-- | The target size of the volume, in GiB.
volumeModification_targetSize :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
volumeModification_targetSize = Lens.lens (\VolumeModification' {targetSize} -> targetSize) (\s@VolumeModification' {} a -> s {targetSize = a} :: VolumeModification)

-- | The original size of the volume, in GiB.
volumeModification_originalSize :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
volumeModification_originalSize = Lens.lens (\VolumeModification' {originalSize} -> originalSize) (\s@VolumeModification' {} a -> s {originalSize = a} :: VolumeModification)

-- | The target IOPS rate of the volume.
volumeModification_targetIops :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
volumeModification_targetIops = Lens.lens (\VolumeModification' {targetIops} -> targetIops) (\s@VolumeModification' {} a -> s {targetIops = a} :: VolumeModification)

-- | The current modification state. The modification state is null for
-- unmodified volumes.
volumeModification_modificationState :: Lens.Lens' VolumeModification (Core.Maybe VolumeModificationState)
volumeModification_modificationState = Lens.lens (\VolumeModification' {modificationState} -> modificationState) (\s@VolumeModification' {} a -> s {modificationState = a} :: VolumeModification)

-- | The target setting for Amazon EBS Multi-Attach.
volumeModification_targetMultiAttachEnabled :: Lens.Lens' VolumeModification (Core.Maybe Core.Bool)
volumeModification_targetMultiAttachEnabled = Lens.lens (\VolumeModification' {targetMultiAttachEnabled} -> targetMultiAttachEnabled) (\s@VolumeModification' {} a -> s {targetMultiAttachEnabled = a} :: VolumeModification)

-- | The original setting for Amazon EBS Multi-Attach.
volumeModification_originalMultiAttachEnabled :: Lens.Lens' VolumeModification (Core.Maybe Core.Bool)
volumeModification_originalMultiAttachEnabled = Lens.lens (\VolumeModification' {originalMultiAttachEnabled} -> originalMultiAttachEnabled) (\s@VolumeModification' {} a -> s {originalMultiAttachEnabled = a} :: VolumeModification)

-- | The target throughput of the volume, in MiB\/s.
volumeModification_targetThroughput :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
volumeModification_targetThroughput = Lens.lens (\VolumeModification' {targetThroughput} -> targetThroughput) (\s@VolumeModification' {} a -> s {targetThroughput = a} :: VolumeModification)

-- | The original throughput of the volume, in MiB\/s.
volumeModification_originalThroughput :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
volumeModification_originalThroughput = Lens.lens (\VolumeModification' {originalThroughput} -> originalThroughput) (\s@VolumeModification' {} a -> s {originalThroughput = a} :: VolumeModification)

-- | The modification progress, from 0 to 100 percent complete.
volumeModification_progress :: Lens.Lens' VolumeModification (Core.Maybe Core.Integer)
volumeModification_progress = Lens.lens (\VolumeModification' {progress} -> progress) (\s@VolumeModification' {} a -> s {progress = a} :: VolumeModification)

instance Core.FromXML VolumeModification where
  parseXML x =
    VolumeModification'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "targetVolumeType")
      Core.<*> (x Core..@? "originalVolumeType")
      Core.<*> (x Core..@? "startTime")
      Core.<*> (x Core..@? "volumeId")
      Core.<*> (x Core..@? "endTime")
      Core.<*> (x Core..@? "originalIops")
      Core.<*> (x Core..@? "targetSize")
      Core.<*> (x Core..@? "originalSize")
      Core.<*> (x Core..@? "targetIops")
      Core.<*> (x Core..@? "modificationState")
      Core.<*> (x Core..@? "targetMultiAttachEnabled")
      Core.<*> (x Core..@? "originalMultiAttachEnabled")
      Core.<*> (x Core..@? "targetThroughput")
      Core.<*> (x Core..@? "originalThroughput")
      Core.<*> (x Core..@? "progress")

instance Core.Hashable VolumeModification

instance Core.NFData VolumeModification
