{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompilationJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CompilationJobSummary
  ( CompilationJobSummary (..),

    -- * Smart constructor
    mkCompilationJobSummary,

    -- * Lenses
    cjsCompilationJobName,
    cjsCompilationJobArn,
    cjsCreationTime,
    cjsCompilationJobStatus,
    cjsCompilationEndTime,
    cjsCompilationStartTime,
    cjsCompilationTargetDevice,
    cjsCompilationTargetPlatformAccelerator,
    cjsCompilationTargetPlatformArch,
    cjsCompilationTargetPlatformOs,
    cjsLastModifiedTime,
  )
where

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
  { -- | The name of the model compilation job that you want a summary for.
    compilationJobName :: Types.EntityName,
    -- | The Amazon Resource Name (ARN) of the model compilation job.
    compilationJobArn :: Types.CompilationJobArn,
    -- | The time when the model compilation job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The status of the model compilation job.
    compilationJobStatus :: Types.CompilationJobStatus,
    -- | The time when the model compilation job completed.
    compilationEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time when the model compilation job started.
    compilationStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The type of device that the model will run on after the compilation job has completed.
    compilationTargetDevice :: Core.Maybe Types.TargetDevice,
    -- | The type of accelerator that the model will run on after the compilation job has completed.
    compilationTargetPlatformAccelerator :: Core.Maybe Types.TargetPlatformAccelerator,
    -- | The type of architecture that the model will run on after the compilation job has completed.
    compilationTargetPlatformArch :: Core.Maybe Types.TargetPlatformArch,
    -- | The type of OS that the model will run on after the compilation job has completed.
    compilationTargetPlatformOs :: Core.Maybe Types.TargetPlatformOs,
    -- | The time when the model compilation job was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CompilationJobSummary' value with any optional fields omitted.
mkCompilationJobSummary ::
  -- | 'compilationJobName'
  Types.EntityName ->
  -- | 'compilationJobArn'
  Types.CompilationJobArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'compilationJobStatus'
  Types.CompilationJobStatus ->
  CompilationJobSummary
mkCompilationJobSummary
  compilationJobName
  compilationJobArn
  creationTime
  compilationJobStatus =
    CompilationJobSummary'
      { compilationJobName,
        compilationJobArn,
        creationTime,
        compilationJobStatus,
        compilationEndTime = Core.Nothing,
        compilationStartTime = Core.Nothing,
        compilationTargetDevice = Core.Nothing,
        compilationTargetPlatformAccelerator = Core.Nothing,
        compilationTargetPlatformArch = Core.Nothing,
        compilationTargetPlatformOs = Core.Nothing,
        lastModifiedTime = Core.Nothing
      }

-- | The name of the model compilation job that you want a summary for.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobName :: Lens.Lens' CompilationJobSummary Types.EntityName
cjsCompilationJobName = Lens.field @"compilationJobName"
{-# DEPRECATED cjsCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobArn :: Lens.Lens' CompilationJobSummary Types.CompilationJobArn
cjsCompilationJobArn = Lens.field @"compilationJobArn"
{-# DEPRECATED cjsCompilationJobArn "Use generic-lens or generic-optics with 'compilationJobArn' instead." #-}

-- | The time when the model compilation job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCreationTime :: Lens.Lens' CompilationJobSummary Core.NominalDiffTime
cjsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED cjsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobStatus :: Lens.Lens' CompilationJobSummary Types.CompilationJobStatus
cjsCompilationJobStatus = Lens.field @"compilationJobStatus"
{-# DEPRECATED cjsCompilationJobStatus "Use generic-lens or generic-optics with 'compilationJobStatus' instead." #-}

-- | The time when the model compilation job completed.
--
-- /Note:/ Consider using 'compilationEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationEndTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.NominalDiffTime)
cjsCompilationEndTime = Lens.field @"compilationEndTime"
{-# DEPRECATED cjsCompilationEndTime "Use generic-lens or generic-optics with 'compilationEndTime' instead." #-}

-- | The time when the model compilation job started.
--
-- /Note:/ Consider using 'compilationStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationStartTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.NominalDiffTime)
cjsCompilationStartTime = Lens.field @"compilationStartTime"
{-# DEPRECATED cjsCompilationStartTime "Use generic-lens or generic-optics with 'compilationStartTime' instead." #-}

-- | The type of device that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetDevice :: Lens.Lens' CompilationJobSummary (Core.Maybe Types.TargetDevice)
cjsCompilationTargetDevice = Lens.field @"compilationTargetDevice"
{-# DEPRECATED cjsCompilationTargetDevice "Use generic-lens or generic-optics with 'compilationTargetDevice' instead." #-}

-- | The type of accelerator that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformAccelerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformAccelerator :: Lens.Lens' CompilationJobSummary (Core.Maybe Types.TargetPlatformAccelerator)
cjsCompilationTargetPlatformAccelerator = Lens.field @"compilationTargetPlatformAccelerator"
{-# DEPRECATED cjsCompilationTargetPlatformAccelerator "Use generic-lens or generic-optics with 'compilationTargetPlatformAccelerator' instead." #-}

-- | The type of architecture that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformArch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformArch :: Lens.Lens' CompilationJobSummary (Core.Maybe Types.TargetPlatformArch)
cjsCompilationTargetPlatformArch = Lens.field @"compilationTargetPlatformArch"
{-# DEPRECATED cjsCompilationTargetPlatformArch "Use generic-lens or generic-optics with 'compilationTargetPlatformArch' instead." #-}

-- | The type of OS that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformOs :: Lens.Lens' CompilationJobSummary (Core.Maybe Types.TargetPlatformOs)
cjsCompilationTargetPlatformOs = Lens.field @"compilationTargetPlatformOs"
{-# DEPRECATED cjsCompilationTargetPlatformOs "Use generic-lens or generic-optics with 'compilationTargetPlatformOs' instead." #-}

-- | The time when the model compilation job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsLastModifiedTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.NominalDiffTime)
cjsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED cjsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

instance Core.FromJSON CompilationJobSummary where
  parseJSON =
    Core.withObject "CompilationJobSummary" Core.$
      \x ->
        CompilationJobSummary'
          Core.<$> (x Core..: "CompilationJobName")
          Core.<*> (x Core..: "CompilationJobArn")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "CompilationJobStatus")
          Core.<*> (x Core..:? "CompilationEndTime")
          Core.<*> (x Core..:? "CompilationStartTime")
          Core.<*> (x Core..:? "CompilationTargetDevice")
          Core.<*> (x Core..:? "CompilationTargetPlatformAccelerator")
          Core.<*> (x Core..:? "CompilationTargetPlatformArch")
          Core.<*> (x Core..:? "CompilationTargetPlatformOs")
          Core.<*> (x Core..:? "LastModifiedTime")
