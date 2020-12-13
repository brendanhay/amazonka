{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeModification
  ( VolumeModification (..),

    -- * Smart constructor
    mkVolumeModification,

    -- * Lenses
    vmProgress,
    vmStartTime,
    vmModificationState,
    vmTargetVolumeType,
    vmOriginalVolumeType,
    vmTargetSize,
    vmTargetIOPS,
    vmOriginalSize,
    vmOriginalIOPS,
    vmStatusMessage,
    vmEndTime,
    vmVolumeId,
  )
where

import Network.AWS.EC2.Types.VolumeModificationState
import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the modification status of an EBS volume.
--
-- If the volume has never been modified, some element values will be null.
--
-- /See:/ 'mkVolumeModification' smart constructor.
data VolumeModification = VolumeModification'
  { -- | The modification progress, from 0 to 100 percent complete.
    progress :: Lude.Maybe Lude.Integer,
    -- | The modification start time.
    startTime :: Lude.Maybe Lude.DateTime,
    -- | The current modification state. The modification state is null for unmodified volumes.
    modificationState :: Lude.Maybe VolumeModificationState,
    -- | The target EBS volume type of the volume.
    targetVolumeType :: Lude.Maybe VolumeType,
    -- | The original EBS volume type of the volume.
    originalVolumeType :: Lude.Maybe VolumeType,
    -- | The target size of the volume, in GiB.
    targetSize :: Lude.Maybe Lude.Int,
    -- | The target IOPS rate of the volume.
    targetIOPS :: Lude.Maybe Lude.Int,
    -- | The original size of the volume, in GiB.
    originalSize :: Lude.Maybe Lude.Int,
    -- | The original IOPS rate of the volume.
    originalIOPS :: Lude.Maybe Lude.Int,
    -- | A status message about the modification progress or failure.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The modification completion or failure time.
    endTime :: Lude.Maybe Lude.DateTime,
    -- | The ID of the volume.
    volumeId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeModification' with the minimum fields required to make a request.
--
-- * 'progress' - The modification progress, from 0 to 100 percent complete.
-- * 'startTime' - The modification start time.
-- * 'modificationState' - The current modification state. The modification state is null for unmodified volumes.
-- * 'targetVolumeType' - The target EBS volume type of the volume.
-- * 'originalVolumeType' - The original EBS volume type of the volume.
-- * 'targetSize' - The target size of the volume, in GiB.
-- * 'targetIOPS' - The target IOPS rate of the volume.
-- * 'originalSize' - The original size of the volume, in GiB.
-- * 'originalIOPS' - The original IOPS rate of the volume.
-- * 'statusMessage' - A status message about the modification progress or failure.
-- * 'endTime' - The modification completion or failure time.
-- * 'volumeId' - The ID of the volume.
mkVolumeModification ::
  VolumeModification
mkVolumeModification =
  VolumeModification'
    { progress = Lude.Nothing,
      startTime = Lude.Nothing,
      modificationState = Lude.Nothing,
      targetVolumeType = Lude.Nothing,
      originalVolumeType = Lude.Nothing,
      targetSize = Lude.Nothing,
      targetIOPS = Lude.Nothing,
      originalSize = Lude.Nothing,
      originalIOPS = Lude.Nothing,
      statusMessage = Lude.Nothing,
      endTime = Lude.Nothing,
      volumeId = Lude.Nothing
    }

-- | The modification progress, from 0 to 100 percent complete.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmProgress :: Lens.Lens' VolumeModification (Lude.Maybe Lude.Integer)
vmProgress = Lens.lens (progress :: VolumeModification -> Lude.Maybe Lude.Integer) (\s a -> s {progress = a} :: VolumeModification)
{-# DEPRECATED vmProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The modification start time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmStartTime :: Lens.Lens' VolumeModification (Lude.Maybe Lude.DateTime)
vmStartTime = Lens.lens (startTime :: VolumeModification -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: VolumeModification)
{-# DEPRECATED vmStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The current modification state. The modification state is null for unmodified volumes.
--
-- /Note:/ Consider using 'modificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmModificationState :: Lens.Lens' VolumeModification (Lude.Maybe VolumeModificationState)
vmModificationState = Lens.lens (modificationState :: VolumeModification -> Lude.Maybe VolumeModificationState) (\s a -> s {modificationState = a} :: VolumeModification)
{-# DEPRECATED vmModificationState "Use generic-lens or generic-optics with 'modificationState' instead." #-}

-- | The target EBS volume type of the volume.
--
-- /Note:/ Consider using 'targetVolumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmTargetVolumeType :: Lens.Lens' VolumeModification (Lude.Maybe VolumeType)
vmTargetVolumeType = Lens.lens (targetVolumeType :: VolumeModification -> Lude.Maybe VolumeType) (\s a -> s {targetVolumeType = a} :: VolumeModification)
{-# DEPRECATED vmTargetVolumeType "Use generic-lens or generic-optics with 'targetVolumeType' instead." #-}

-- | The original EBS volume type of the volume.
--
-- /Note:/ Consider using 'originalVolumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOriginalVolumeType :: Lens.Lens' VolumeModification (Lude.Maybe VolumeType)
vmOriginalVolumeType = Lens.lens (originalVolumeType :: VolumeModification -> Lude.Maybe VolumeType) (\s a -> s {originalVolumeType = a} :: VolumeModification)
{-# DEPRECATED vmOriginalVolumeType "Use generic-lens or generic-optics with 'originalVolumeType' instead." #-}

-- | The target size of the volume, in GiB.
--
-- /Note:/ Consider using 'targetSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmTargetSize :: Lens.Lens' VolumeModification (Lude.Maybe Lude.Int)
vmTargetSize = Lens.lens (targetSize :: VolumeModification -> Lude.Maybe Lude.Int) (\s a -> s {targetSize = a} :: VolumeModification)
{-# DEPRECATED vmTargetSize "Use generic-lens or generic-optics with 'targetSize' instead." #-}

-- | The target IOPS rate of the volume.
--
-- /Note:/ Consider using 'targetIOPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmTargetIOPS :: Lens.Lens' VolumeModification (Lude.Maybe Lude.Int)
vmTargetIOPS = Lens.lens (targetIOPS :: VolumeModification -> Lude.Maybe Lude.Int) (\s a -> s {targetIOPS = a} :: VolumeModification)
{-# DEPRECATED vmTargetIOPS "Use generic-lens or generic-optics with 'targetIOPS' instead." #-}

-- | The original size of the volume, in GiB.
--
-- /Note:/ Consider using 'originalSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOriginalSize :: Lens.Lens' VolumeModification (Lude.Maybe Lude.Int)
vmOriginalSize = Lens.lens (originalSize :: VolumeModification -> Lude.Maybe Lude.Int) (\s a -> s {originalSize = a} :: VolumeModification)
{-# DEPRECATED vmOriginalSize "Use generic-lens or generic-optics with 'originalSize' instead." #-}

-- | The original IOPS rate of the volume.
--
-- /Note:/ Consider using 'originalIOPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOriginalIOPS :: Lens.Lens' VolumeModification (Lude.Maybe Lude.Int)
vmOriginalIOPS = Lens.lens (originalIOPS :: VolumeModification -> Lude.Maybe Lude.Int) (\s a -> s {originalIOPS = a} :: VolumeModification)
{-# DEPRECATED vmOriginalIOPS "Use generic-lens or generic-optics with 'originalIOPS' instead." #-}

-- | A status message about the modification progress or failure.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmStatusMessage :: Lens.Lens' VolumeModification (Lude.Maybe Lude.Text)
vmStatusMessage = Lens.lens (statusMessage :: VolumeModification -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: VolumeModification)
{-# DEPRECATED vmStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The modification completion or failure time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmEndTime :: Lens.Lens' VolumeModification (Lude.Maybe Lude.DateTime)
vmEndTime = Lens.lens (endTime :: VolumeModification -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: VolumeModification)
{-# DEPRECATED vmEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmVolumeId :: Lens.Lens' VolumeModification (Lude.Maybe Lude.Text)
vmVolumeId = Lens.lens (volumeId :: VolumeModification -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: VolumeModification)
{-# DEPRECATED vmVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.FromXML VolumeModification where
  parseXML x =
    VolumeModification'
      Lude.<$> (x Lude..@? "progress")
      Lude.<*> (x Lude..@? "startTime")
      Lude.<*> (x Lude..@? "modificationState")
      Lude.<*> (x Lude..@? "targetVolumeType")
      Lude.<*> (x Lude..@? "originalVolumeType")
      Lude.<*> (x Lude..@? "targetSize")
      Lude.<*> (x Lude..@? "targetIops")
      Lude.<*> (x Lude..@? "originalSize")
      Lude.<*> (x Lude..@? "originalIops")
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "endTime")
      Lude.<*> (x Lude..@? "volumeId")
