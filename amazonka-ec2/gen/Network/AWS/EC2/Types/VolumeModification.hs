{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeModificationState
import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the modification status of an EBS volume.
--
-- If the volume has never been modified, some element values will be null.
--
-- /See:/ 'newVolumeModification' smart constructor.
data VolumeModification = VolumeModification'
  { -- | A status message about the modification progress or failure.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The target EBS volume type of the volume.
    targetVolumeType :: Prelude.Maybe VolumeType,
    -- | The original EBS volume type of the volume.
    originalVolumeType :: Prelude.Maybe VolumeType,
    -- | The modification start time.
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The ID of the volume.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The modification completion or failure time.
    endTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The original IOPS rate of the volume.
    originalIops :: Prelude.Maybe Prelude.Int,
    -- | The target size of the volume, in GiB.
    targetSize :: Prelude.Maybe Prelude.Int,
    -- | The original size of the volume, in GiB.
    originalSize :: Prelude.Maybe Prelude.Int,
    -- | The target IOPS rate of the volume.
    targetIops :: Prelude.Maybe Prelude.Int,
    -- | The current modification state. The modification state is null for
    -- unmodified volumes.
    modificationState :: Prelude.Maybe VolumeModificationState,
    -- | The target setting for Amazon EBS Multi-Attach.
    targetMultiAttachEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The original setting for Amazon EBS Multi-Attach.
    originalMultiAttachEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The target throughput of the volume, in MiB\/s.
    targetThroughput :: Prelude.Maybe Prelude.Int,
    -- | The original throughput of the volume, in MiB\/s.
    originalThroughput :: Prelude.Maybe Prelude.Int,
    -- | The modification progress, from 0 to 100 percent complete.
    progress :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { statusMessage =
        Prelude.Nothing,
      targetVolumeType = Prelude.Nothing,
      originalVolumeType = Prelude.Nothing,
      startTime = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      endTime = Prelude.Nothing,
      originalIops = Prelude.Nothing,
      targetSize = Prelude.Nothing,
      originalSize = Prelude.Nothing,
      targetIops = Prelude.Nothing,
      modificationState = Prelude.Nothing,
      targetMultiAttachEnabled = Prelude.Nothing,
      originalMultiAttachEnabled = Prelude.Nothing,
      targetThroughput = Prelude.Nothing,
      originalThroughput = Prelude.Nothing,
      progress = Prelude.Nothing
    }

-- | A status message about the modification progress or failure.
volumeModification_statusMessage :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Text)
volumeModification_statusMessage = Lens.lens (\VolumeModification' {statusMessage} -> statusMessage) (\s@VolumeModification' {} a -> s {statusMessage = a} :: VolumeModification)

-- | The target EBS volume type of the volume.
volumeModification_targetVolumeType :: Lens.Lens' VolumeModification (Prelude.Maybe VolumeType)
volumeModification_targetVolumeType = Lens.lens (\VolumeModification' {targetVolumeType} -> targetVolumeType) (\s@VolumeModification' {} a -> s {targetVolumeType = a} :: VolumeModification)

-- | The original EBS volume type of the volume.
volumeModification_originalVolumeType :: Lens.Lens' VolumeModification (Prelude.Maybe VolumeType)
volumeModification_originalVolumeType = Lens.lens (\VolumeModification' {originalVolumeType} -> originalVolumeType) (\s@VolumeModification' {} a -> s {originalVolumeType = a} :: VolumeModification)

-- | The modification start time.
volumeModification_startTime :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.UTCTime)
volumeModification_startTime = Lens.lens (\VolumeModification' {startTime} -> startTime) (\s@VolumeModification' {} a -> s {startTime = a} :: VolumeModification) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the volume.
volumeModification_volumeId :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Text)
volumeModification_volumeId = Lens.lens (\VolumeModification' {volumeId} -> volumeId) (\s@VolumeModification' {} a -> s {volumeId = a} :: VolumeModification)

-- | The modification completion or failure time.
volumeModification_endTime :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.UTCTime)
volumeModification_endTime = Lens.lens (\VolumeModification' {endTime} -> endTime) (\s@VolumeModification' {} a -> s {endTime = a} :: VolumeModification) Prelude.. Lens.mapping Prelude._Time

-- | The original IOPS rate of the volume.
volumeModification_originalIops :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_originalIops = Lens.lens (\VolumeModification' {originalIops} -> originalIops) (\s@VolumeModification' {} a -> s {originalIops = a} :: VolumeModification)

-- | The target size of the volume, in GiB.
volumeModification_targetSize :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_targetSize = Lens.lens (\VolumeModification' {targetSize} -> targetSize) (\s@VolumeModification' {} a -> s {targetSize = a} :: VolumeModification)

-- | The original size of the volume, in GiB.
volumeModification_originalSize :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_originalSize = Lens.lens (\VolumeModification' {originalSize} -> originalSize) (\s@VolumeModification' {} a -> s {originalSize = a} :: VolumeModification)

-- | The target IOPS rate of the volume.
volumeModification_targetIops :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_targetIops = Lens.lens (\VolumeModification' {targetIops} -> targetIops) (\s@VolumeModification' {} a -> s {targetIops = a} :: VolumeModification)

-- | The current modification state. The modification state is null for
-- unmodified volumes.
volumeModification_modificationState :: Lens.Lens' VolumeModification (Prelude.Maybe VolumeModificationState)
volumeModification_modificationState = Lens.lens (\VolumeModification' {modificationState} -> modificationState) (\s@VolumeModification' {} a -> s {modificationState = a} :: VolumeModification)

-- | The target setting for Amazon EBS Multi-Attach.
volumeModification_targetMultiAttachEnabled :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Bool)
volumeModification_targetMultiAttachEnabled = Lens.lens (\VolumeModification' {targetMultiAttachEnabled} -> targetMultiAttachEnabled) (\s@VolumeModification' {} a -> s {targetMultiAttachEnabled = a} :: VolumeModification)

-- | The original setting for Amazon EBS Multi-Attach.
volumeModification_originalMultiAttachEnabled :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Bool)
volumeModification_originalMultiAttachEnabled = Lens.lens (\VolumeModification' {originalMultiAttachEnabled} -> originalMultiAttachEnabled) (\s@VolumeModification' {} a -> s {originalMultiAttachEnabled = a} :: VolumeModification)

-- | The target throughput of the volume, in MiB\/s.
volumeModification_targetThroughput :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_targetThroughput = Lens.lens (\VolumeModification' {targetThroughput} -> targetThroughput) (\s@VolumeModification' {} a -> s {targetThroughput = a} :: VolumeModification)

-- | The original throughput of the volume, in MiB\/s.
volumeModification_originalThroughput :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Int)
volumeModification_originalThroughput = Lens.lens (\VolumeModification' {originalThroughput} -> originalThroughput) (\s@VolumeModification' {} a -> s {originalThroughput = a} :: VolumeModification)

-- | The modification progress, from 0 to 100 percent complete.
volumeModification_progress :: Lens.Lens' VolumeModification (Prelude.Maybe Prelude.Integer)
volumeModification_progress = Lens.lens (\VolumeModification' {progress} -> progress) (\s@VolumeModification' {} a -> s {progress = a} :: VolumeModification)

instance Prelude.FromXML VolumeModification where
  parseXML x =
    VolumeModification'
      Prelude.<$> (x Prelude..@? "statusMessage")
      Prelude.<*> (x Prelude..@? "targetVolumeType")
      Prelude.<*> (x Prelude..@? "originalVolumeType")
      Prelude.<*> (x Prelude..@? "startTime")
      Prelude.<*> (x Prelude..@? "volumeId")
      Prelude.<*> (x Prelude..@? "endTime")
      Prelude.<*> (x Prelude..@? "originalIops")
      Prelude.<*> (x Prelude..@? "targetSize")
      Prelude.<*> (x Prelude..@? "originalSize")
      Prelude.<*> (x Prelude..@? "targetIops")
      Prelude.<*> (x Prelude..@? "modificationState")
      Prelude.<*> (x Prelude..@? "targetMultiAttachEnabled")
      Prelude.<*> (x Prelude..@? "originalMultiAttachEnabled")
      Prelude.<*> (x Prelude..@? "targetThroughput")
      Prelude.<*> (x Prelude..@? "originalThroughput")
      Prelude.<*> (x Prelude..@? "progress")

instance Prelude.Hashable VolumeModification

instance Prelude.NFData VolumeModification
