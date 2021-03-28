{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeModification
  ( VolumeModification (..)
  -- * Smart constructor
  , mkVolumeModification
  -- * Lenses
  , vmEndTime
  , vmModificationState
  , vmOriginalIops
  , vmOriginalSize
  , vmOriginalVolumeType
  , vmProgress
  , vmStartTime
  , vmStatusMessage
  , vmTargetIops
  , vmTargetSize
  , vmTargetVolumeType
  , vmVolumeId
  ) where

import qualified Network.AWS.EC2.Types.VolumeModificationState as Types
import qualified Network.AWS.EC2.Types.VolumeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the modification status of an EBS volume.
--
-- If the volume has never been modified, some element values will be null.
--
-- /See:/ 'mkVolumeModification' smart constructor.
data VolumeModification = VolumeModification'
  { endTime :: Core.Maybe Core.UTCTime
    -- ^ The modification completion or failure time.
  , modificationState :: Core.Maybe Types.VolumeModificationState
    -- ^ The current modification state. The modification state is null for unmodified volumes.
  , originalIops :: Core.Maybe Core.Int
    -- ^ The original IOPS rate of the volume.
  , originalSize :: Core.Maybe Core.Int
    -- ^ The original size of the volume, in GiB.
  , originalVolumeType :: Core.Maybe Types.VolumeType
    -- ^ The original EBS volume type of the volume.
  , progress :: Core.Maybe Core.Integer
    -- ^ The modification progress, from 0 to 100 percent complete.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The modification start time.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A status message about the modification progress or failure.
  , targetIops :: Core.Maybe Core.Int
    -- ^ The target IOPS rate of the volume.
  , targetSize :: Core.Maybe Core.Int
    -- ^ The target size of the volume, in GiB.
  , targetVolumeType :: Core.Maybe Types.VolumeType
    -- ^ The target EBS volume type of the volume.
  , volumeId :: Core.Maybe Core.Text
    -- ^ The ID of the volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VolumeModification' value with any optional fields omitted.
mkVolumeModification
    :: VolumeModification
mkVolumeModification
  = VolumeModification'{endTime = Core.Nothing,
                        modificationState = Core.Nothing, originalIops = Core.Nothing,
                        originalSize = Core.Nothing, originalVolumeType = Core.Nothing,
                        progress = Core.Nothing, startTime = Core.Nothing,
                        statusMessage = Core.Nothing, targetIops = Core.Nothing,
                        targetSize = Core.Nothing, targetVolumeType = Core.Nothing,
                        volumeId = Core.Nothing}

-- | The modification completion or failure time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmEndTime :: Lens.Lens' VolumeModification (Core.Maybe Core.UTCTime)
vmEndTime = Lens.field @"endTime"
{-# INLINEABLE vmEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The current modification state. The modification state is null for unmodified volumes.
--
-- /Note:/ Consider using 'modificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmModificationState :: Lens.Lens' VolumeModification (Core.Maybe Types.VolumeModificationState)
vmModificationState = Lens.field @"modificationState"
{-# INLINEABLE vmModificationState #-}
{-# DEPRECATED modificationState "Use generic-lens or generic-optics with 'modificationState' instead"  #-}

-- | The original IOPS rate of the volume.
--
-- /Note:/ Consider using 'originalIops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOriginalIops :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
vmOriginalIops = Lens.field @"originalIops"
{-# INLINEABLE vmOriginalIops #-}
{-# DEPRECATED originalIops "Use generic-lens or generic-optics with 'originalIops' instead"  #-}

-- | The original size of the volume, in GiB.
--
-- /Note:/ Consider using 'originalSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOriginalSize :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
vmOriginalSize = Lens.field @"originalSize"
{-# INLINEABLE vmOriginalSize #-}
{-# DEPRECATED originalSize "Use generic-lens or generic-optics with 'originalSize' instead"  #-}

-- | The original EBS volume type of the volume.
--
-- /Note:/ Consider using 'originalVolumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOriginalVolumeType :: Lens.Lens' VolumeModification (Core.Maybe Types.VolumeType)
vmOriginalVolumeType = Lens.field @"originalVolumeType"
{-# INLINEABLE vmOriginalVolumeType #-}
{-# DEPRECATED originalVolumeType "Use generic-lens or generic-optics with 'originalVolumeType' instead"  #-}

-- | The modification progress, from 0 to 100 percent complete.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmProgress :: Lens.Lens' VolumeModification (Core.Maybe Core.Integer)
vmProgress = Lens.field @"progress"
{-# INLINEABLE vmProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The modification start time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmStartTime :: Lens.Lens' VolumeModification (Core.Maybe Core.UTCTime)
vmStartTime = Lens.field @"startTime"
{-# INLINEABLE vmStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | A status message about the modification progress or failure.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmStatusMessage :: Lens.Lens' VolumeModification (Core.Maybe Core.Text)
vmStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE vmStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The target IOPS rate of the volume.
--
-- /Note:/ Consider using 'targetIops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmTargetIops :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
vmTargetIops = Lens.field @"targetIops"
{-# INLINEABLE vmTargetIops #-}
{-# DEPRECATED targetIops "Use generic-lens or generic-optics with 'targetIops' instead"  #-}

-- | The target size of the volume, in GiB.
--
-- /Note:/ Consider using 'targetSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmTargetSize :: Lens.Lens' VolumeModification (Core.Maybe Core.Int)
vmTargetSize = Lens.field @"targetSize"
{-# INLINEABLE vmTargetSize #-}
{-# DEPRECATED targetSize "Use generic-lens or generic-optics with 'targetSize' instead"  #-}

-- | The target EBS volume type of the volume.
--
-- /Note:/ Consider using 'targetVolumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmTargetVolumeType :: Lens.Lens' VolumeModification (Core.Maybe Types.VolumeType)
vmTargetVolumeType = Lens.field @"targetVolumeType"
{-# INLINEABLE vmTargetVolumeType #-}
{-# DEPRECATED targetVolumeType "Use generic-lens or generic-optics with 'targetVolumeType' instead"  #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmVolumeId :: Lens.Lens' VolumeModification (Core.Maybe Core.Text)
vmVolumeId = Lens.field @"volumeId"
{-# INLINEABLE vmVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

instance Core.FromXML VolumeModification where
        parseXML x
          = VolumeModification' Core.<$>
              (x Core..@? "endTime") Core.<*> x Core..@? "modificationState"
                Core.<*> x Core..@? "originalIops"
                Core.<*> x Core..@? "originalSize"
                Core.<*> x Core..@? "originalVolumeType"
                Core.<*> x Core..@? "progress"
                Core.<*> x Core..@? "startTime"
                Core.<*> x Core..@? "statusMessage"
                Core.<*> x Core..@? "targetIops"
                Core.<*> x Core..@? "targetSize"
                Core.<*> x Core..@? "targetVolumeType"
                Core.<*> x Core..@? "volumeId"
