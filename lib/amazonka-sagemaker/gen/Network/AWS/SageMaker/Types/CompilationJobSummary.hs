{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompilationJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CompilationJobSummary
  ( CompilationJobSummary (..)
  -- * Smart constructor
  , mkCompilationJobSummary
  -- * Lenses
  , cjsCompilationJobName
  , cjsCompilationJobArn
  , cjsCreationTime
  , cjsCompilationJobStatus
  , cjsCompilationEndTime
  , cjsCompilationStartTime
  , cjsCompilationTargetDevice
  , cjsCompilationTargetPlatformAccelerator
  , cjsCompilationTargetPlatformArch
  , cjsCompilationTargetPlatformOs
  , cjsLastModifiedTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CompilationJobArn as Types
import qualified Network.AWS.SageMaker.Types.CompilationJobStatus as Types
import qualified Network.AWS.SageMaker.Types.EntityName as Types
import qualified Network.AWS.SageMaker.Types.TargetDevice as Types
import qualified Network.AWS.SageMaker.Types.TargetPlatformAccelerator as Types
import qualified Network.AWS.SageMaker.Types.TargetPlatformArch as Types
import qualified Network.AWS.SageMaker.Types.TargetPlatformOs as Types

-- | A summary of a model compilation job.
--
-- /See:/ 'mkCompilationJobSummary' smart constructor.
data CompilationJobSummary = CompilationJobSummary'
  { compilationJobName :: Types.EntityName
    -- ^ The name of the model compilation job that you want a summary for.
  , compilationJobArn :: Types.CompilationJobArn
    -- ^ The Amazon Resource Name (ARN) of the model compilation job.
  , creationTime :: Core.NominalDiffTime
    -- ^ The time when the model compilation job was created.
  , compilationJobStatus :: Types.CompilationJobStatus
    -- ^ The status of the model compilation job.
  , compilationEndTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the model compilation job completed.
  , compilationStartTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the model compilation job started.
  , compilationTargetDevice :: Core.Maybe Types.TargetDevice
    -- ^ The type of device that the model will run on after the compilation job has completed.
  , compilationTargetPlatformAccelerator :: Core.Maybe Types.TargetPlatformAccelerator
    -- ^ The type of accelerator that the model will run on after the compilation job has completed.
  , compilationTargetPlatformArch :: Core.Maybe Types.TargetPlatformArch
    -- ^ The type of architecture that the model will run on after the compilation job has completed.
  , compilationTargetPlatformOs :: Core.Maybe Types.TargetPlatformOs
    -- ^ The type of OS that the model will run on after the compilation job has completed.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the model compilation job was last modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CompilationJobSummary' value with any optional fields omitted.
mkCompilationJobSummary
    :: Types.EntityName -- ^ 'compilationJobName'
    -> Types.CompilationJobArn -- ^ 'compilationJobArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.CompilationJobStatus -- ^ 'compilationJobStatus'
    -> CompilationJobSummary
mkCompilationJobSummary compilationJobName compilationJobArn
  creationTime compilationJobStatus
  = CompilationJobSummary'{compilationJobName, compilationJobArn,
                           creationTime, compilationJobStatus,
                           compilationEndTime = Core.Nothing,
                           compilationStartTime = Core.Nothing,
                           compilationTargetDevice = Core.Nothing,
                           compilationTargetPlatformAccelerator = Core.Nothing,
                           compilationTargetPlatformArch = Core.Nothing,
                           compilationTargetPlatformOs = Core.Nothing,
                           lastModifiedTime = Core.Nothing}

-- | The name of the model compilation job that you want a summary for.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobName :: Lens.Lens' CompilationJobSummary Types.EntityName
cjsCompilationJobName = Lens.field @"compilationJobName"
{-# INLINEABLE cjsCompilationJobName #-}
{-# DEPRECATED compilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobArn :: Lens.Lens' CompilationJobSummary Types.CompilationJobArn
cjsCompilationJobArn = Lens.field @"compilationJobArn"
{-# INLINEABLE cjsCompilationJobArn #-}
{-# DEPRECATED compilationJobArn "Use generic-lens or generic-optics with 'compilationJobArn' instead"  #-}

-- | The time when the model compilation job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCreationTime :: Lens.Lens' CompilationJobSummary Core.NominalDiffTime
cjsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE cjsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The status of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobStatus :: Lens.Lens' CompilationJobSummary Types.CompilationJobStatus
cjsCompilationJobStatus = Lens.field @"compilationJobStatus"
{-# INLINEABLE cjsCompilationJobStatus #-}
{-# DEPRECATED compilationJobStatus "Use generic-lens or generic-optics with 'compilationJobStatus' instead"  #-}

-- | The time when the model compilation job completed.
--
-- /Note:/ Consider using 'compilationEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationEndTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.NominalDiffTime)
cjsCompilationEndTime = Lens.field @"compilationEndTime"
{-# INLINEABLE cjsCompilationEndTime #-}
{-# DEPRECATED compilationEndTime "Use generic-lens or generic-optics with 'compilationEndTime' instead"  #-}

-- | The time when the model compilation job started.
--
-- /Note:/ Consider using 'compilationStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationStartTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.NominalDiffTime)
cjsCompilationStartTime = Lens.field @"compilationStartTime"
{-# INLINEABLE cjsCompilationStartTime #-}
{-# DEPRECATED compilationStartTime "Use generic-lens or generic-optics with 'compilationStartTime' instead"  #-}

-- | The type of device that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetDevice :: Lens.Lens' CompilationJobSummary (Core.Maybe Types.TargetDevice)
cjsCompilationTargetDevice = Lens.field @"compilationTargetDevice"
{-# INLINEABLE cjsCompilationTargetDevice #-}
{-# DEPRECATED compilationTargetDevice "Use generic-lens or generic-optics with 'compilationTargetDevice' instead"  #-}

-- | The type of accelerator that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformAccelerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformAccelerator :: Lens.Lens' CompilationJobSummary (Core.Maybe Types.TargetPlatformAccelerator)
cjsCompilationTargetPlatformAccelerator = Lens.field @"compilationTargetPlatformAccelerator"
{-# INLINEABLE cjsCompilationTargetPlatformAccelerator #-}
{-# DEPRECATED compilationTargetPlatformAccelerator "Use generic-lens or generic-optics with 'compilationTargetPlatformAccelerator' instead"  #-}

-- | The type of architecture that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformArch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformArch :: Lens.Lens' CompilationJobSummary (Core.Maybe Types.TargetPlatformArch)
cjsCompilationTargetPlatformArch = Lens.field @"compilationTargetPlatformArch"
{-# INLINEABLE cjsCompilationTargetPlatformArch #-}
{-# DEPRECATED compilationTargetPlatformArch "Use generic-lens or generic-optics with 'compilationTargetPlatformArch' instead"  #-}

-- | The type of OS that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformOs :: Lens.Lens' CompilationJobSummary (Core.Maybe Types.TargetPlatformOs)
cjsCompilationTargetPlatformOs = Lens.field @"compilationTargetPlatformOs"
{-# INLINEABLE cjsCompilationTargetPlatformOs #-}
{-# DEPRECATED compilationTargetPlatformOs "Use generic-lens or generic-optics with 'compilationTargetPlatformOs' instead"  #-}

-- | The time when the model compilation job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsLastModifiedTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.NominalDiffTime)
cjsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE cjsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

instance Core.FromJSON CompilationJobSummary where
        parseJSON
          = Core.withObject "CompilationJobSummary" Core.$
              \ x ->
                CompilationJobSummary' Core.<$>
                  (x Core..: "CompilationJobName") Core.<*>
                    x Core..: "CompilationJobArn"
                    Core.<*> x Core..: "CreationTime"
                    Core.<*> x Core..: "CompilationJobStatus"
                    Core.<*> x Core..:? "CompilationEndTime"
                    Core.<*> x Core..:? "CompilationStartTime"
                    Core.<*> x Core..:? "CompilationTargetDevice"
                    Core.<*> x Core..:? "CompilationTargetPlatformAccelerator"
                    Core.<*> x Core..:? "CompilationTargetPlatformArch"
                    Core.<*> x Core..:? "CompilationTargetPlatformOs"
                    Core.<*> x Core..:? "LastModifiedTime"
