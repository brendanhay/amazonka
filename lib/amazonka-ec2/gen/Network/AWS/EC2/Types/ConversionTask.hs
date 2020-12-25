{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConversionTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConversionTask
  ( ConversionTask (..),

    -- * Smart constructor
    mkConversionTask,

    -- * Lenses
    ctConversionTaskId,
    ctExpirationTime,
    ctImportInstance,
    ctImportVolume,
    ctState,
    ctStatusMessage,
    ctTags,
  )
where

import qualified Network.AWS.EC2.Types.ConversionTaskId as Types
import qualified Network.AWS.EC2.Types.ConversionTaskState as Types
import qualified Network.AWS.EC2.Types.ExpirationTime as Types
import qualified Network.AWS.EC2.Types.ImportInstanceTaskDetails as Types
import qualified Network.AWS.EC2.Types.ImportVolumeTaskDetails as Types
import qualified Network.AWS.EC2.Types.StatusMessage as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a conversion task.
--
-- /See:/ 'mkConversionTask' smart constructor.
data ConversionTask = ConversionTask'
  { -- | The ID of the conversion task.
    conversionTaskId :: Core.Maybe Types.ConversionTaskId,
    -- | The time when the task expires. If the upload isn't complete before the expiration time, we automatically cancel the task.
    expirationTime :: Core.Maybe Types.ExpirationTime,
    -- | If the task is for importing an instance, this contains information about the import instance task.
    importInstance :: Core.Maybe Types.ImportInstanceTaskDetails,
    -- | If the task is for importing a volume, this contains information about the import volume task.
    importVolume :: Core.Maybe Types.ImportVolumeTaskDetails,
    -- | The state of the conversion task.
    state :: Core.Maybe Types.ConversionTaskState,
    -- | The status message related to the conversion task.
    statusMessage :: Core.Maybe Types.StatusMessage,
    -- | Any tags assigned to the task.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConversionTask' value with any optional fields omitted.
mkConversionTask ::
  ConversionTask
mkConversionTask =
  ConversionTask'
    { conversionTaskId = Core.Nothing,
      expirationTime = Core.Nothing,
      importInstance = Core.Nothing,
      importVolume = Core.Nothing,
      state = Core.Nothing,
      statusMessage = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ID of the conversion task.
--
-- /Note:/ Consider using 'conversionTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConversionTaskId :: Lens.Lens' ConversionTask (Core.Maybe Types.ConversionTaskId)
ctConversionTaskId = Lens.field @"conversionTaskId"
{-# DEPRECATED ctConversionTaskId "Use generic-lens or generic-optics with 'conversionTaskId' instead." #-}

-- | The time when the task expires. If the upload isn't complete before the expiration time, we automatically cancel the task.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctExpirationTime :: Lens.Lens' ConversionTask (Core.Maybe Types.ExpirationTime)
ctExpirationTime = Lens.field @"expirationTime"
{-# DEPRECATED ctExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | If the task is for importing an instance, this contains information about the import instance task.
--
-- /Note:/ Consider using 'importInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctImportInstance :: Lens.Lens' ConversionTask (Core.Maybe Types.ImportInstanceTaskDetails)
ctImportInstance = Lens.field @"importInstance"
{-# DEPRECATED ctImportInstance "Use generic-lens or generic-optics with 'importInstance' instead." #-}

-- | If the task is for importing a volume, this contains information about the import volume task.
--
-- /Note:/ Consider using 'importVolume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctImportVolume :: Lens.Lens' ConversionTask (Core.Maybe Types.ImportVolumeTaskDetails)
ctImportVolume = Lens.field @"importVolume"
{-# DEPRECATED ctImportVolume "Use generic-lens or generic-optics with 'importVolume' instead." #-}

-- | The state of the conversion task.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctState :: Lens.Lens' ConversionTask (Core.Maybe Types.ConversionTaskState)
ctState = Lens.field @"state"
{-# DEPRECATED ctState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The status message related to the conversion task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctStatusMessage :: Lens.Lens' ConversionTask (Core.Maybe Types.StatusMessage)
ctStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED ctStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Any tags assigned to the task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' ConversionTask (Core.Maybe [Types.Tag])
ctTags = Lens.field @"tags"
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ConversionTask where
  parseXML x =
    ConversionTask'
      Core.<$> (x Core..@? "conversionTaskId")
      Core.<*> (x Core..@? "expirationTime")
      Core.<*> (x Core..@? "importInstance")
      Core.<*> (x Core..@? "importVolume")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
