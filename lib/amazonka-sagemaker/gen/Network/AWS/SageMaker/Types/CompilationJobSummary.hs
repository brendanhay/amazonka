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
    cjsCompilationStartTime,
    cjsCreationTime,
    cjsCompilationTargetPlatformAccelerator,
    cjsCompilationTargetDevice,
    cjsLastModifiedTime,
    cjsCompilationJobName,
    cjsCompilationTargetPlatformArch,
    cjsCompilationJobStatus,
    cjsCompilationEndTime,
    cjsCompilationJobARN,
    cjsCompilationTargetPlatformOS,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CompilationJobStatus
import Network.AWS.SageMaker.Types.TargetDevice
import Network.AWS.SageMaker.Types.TargetPlatformAccelerator
import Network.AWS.SageMaker.Types.TargetPlatformArch
import Network.AWS.SageMaker.Types.TargetPlatformOS

-- | A summary of a model compilation job.
--
-- /See:/ 'mkCompilationJobSummary' smart constructor.
data CompilationJobSummary = CompilationJobSummary'
  { -- | The time when the model compilation job started.
    compilationStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The time when the model compilation job was created.
    creationTime :: Lude.Timestamp,
    -- | The type of accelerator that the model will run on after the compilation job has completed.
    compilationTargetPlatformAccelerator :: Lude.Maybe TargetPlatformAccelerator,
    -- | The type of device that the model will run on after the compilation job has completed.
    compilationTargetDevice :: Lude.Maybe TargetDevice,
    -- | The time when the model compilation job was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the model compilation job that you want a summary for.
    compilationJobName :: Lude.Text,
    -- | The type of architecture that the model will run on after the compilation job has completed.
    compilationTargetPlatformArch :: Lude.Maybe TargetPlatformArch,
    -- | The status of the model compilation job.
    compilationJobStatus :: CompilationJobStatus,
    -- | The time when the model compilation job completed.
    compilationEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the model compilation job.
    compilationJobARN :: Lude.Text,
    -- | The type of OS that the model will run on after the compilation job has completed.
    compilationTargetPlatformOS :: Lude.Maybe TargetPlatformOS
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompilationJobSummary' with the minimum fields required to make a request.
--
-- * 'compilationStartTime' - The time when the model compilation job started.
-- * 'creationTime' - The time when the model compilation job was created.
-- * 'compilationTargetPlatformAccelerator' - The type of accelerator that the model will run on after the compilation job has completed.
-- * 'compilationTargetDevice' - The type of device that the model will run on after the compilation job has completed.
-- * 'lastModifiedTime' - The time when the model compilation job was last modified.
-- * 'compilationJobName' - The name of the model compilation job that you want a summary for.
-- * 'compilationTargetPlatformArch' - The type of architecture that the model will run on after the compilation job has completed.
-- * 'compilationJobStatus' - The status of the model compilation job.
-- * 'compilationEndTime' - The time when the model compilation job completed.
-- * 'compilationJobARN' - The Amazon Resource Name (ARN) of the model compilation job.
-- * 'compilationTargetPlatformOS' - The type of OS that the model will run on after the compilation job has completed.
mkCompilationJobSummary ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'compilationJobName'
  Lude.Text ->
  -- | 'compilationJobStatus'
  CompilationJobStatus ->
  -- | 'compilationJobARN'
  Lude.Text ->
  CompilationJobSummary
mkCompilationJobSummary
  pCreationTime_
  pCompilationJobName_
  pCompilationJobStatus_
  pCompilationJobARN_ =
    CompilationJobSummary'
      { compilationStartTime = Lude.Nothing,
        creationTime = pCreationTime_,
        compilationTargetPlatformAccelerator = Lude.Nothing,
        compilationTargetDevice = Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        compilationJobName = pCompilationJobName_,
        compilationTargetPlatformArch = Lude.Nothing,
        compilationJobStatus = pCompilationJobStatus_,
        compilationEndTime = Lude.Nothing,
        compilationJobARN = pCompilationJobARN_,
        compilationTargetPlatformOS = Lude.Nothing
      }

-- | The time when the model compilation job started.
--
-- /Note:/ Consider using 'compilationStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationStartTime :: Lens.Lens' CompilationJobSummary (Lude.Maybe Lude.Timestamp)
cjsCompilationStartTime = Lens.lens (compilationStartTime :: CompilationJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {compilationStartTime = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationStartTime "Use generic-lens or generic-optics with 'compilationStartTime' instead." #-}

-- | The time when the model compilation job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCreationTime :: Lens.Lens' CompilationJobSummary Lude.Timestamp
cjsCreationTime = Lens.lens (creationTime :: CompilationJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The type of accelerator that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformAccelerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformAccelerator :: Lens.Lens' CompilationJobSummary (Lude.Maybe TargetPlatformAccelerator)
cjsCompilationTargetPlatformAccelerator = Lens.lens (compilationTargetPlatformAccelerator :: CompilationJobSummary -> Lude.Maybe TargetPlatformAccelerator) (\s a -> s {compilationTargetPlatformAccelerator = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationTargetPlatformAccelerator "Use generic-lens or generic-optics with 'compilationTargetPlatformAccelerator' instead." #-}

-- | The type of device that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetDevice :: Lens.Lens' CompilationJobSummary (Lude.Maybe TargetDevice)
cjsCompilationTargetDevice = Lens.lens (compilationTargetDevice :: CompilationJobSummary -> Lude.Maybe TargetDevice) (\s a -> s {compilationTargetDevice = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationTargetDevice "Use generic-lens or generic-optics with 'compilationTargetDevice' instead." #-}

-- | The time when the model compilation job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsLastModifiedTime :: Lens.Lens' CompilationJobSummary (Lude.Maybe Lude.Timestamp)
cjsLastModifiedTime = Lens.lens (lastModifiedTime :: CompilationJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: CompilationJobSummary)
{-# DEPRECATED cjsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the model compilation job that you want a summary for.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobName :: Lens.Lens' CompilationJobSummary Lude.Text
cjsCompilationJobName = Lens.lens (compilationJobName :: CompilationJobSummary -> Lude.Text) (\s a -> s {compilationJobName = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

-- | The type of architecture that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformArch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformArch :: Lens.Lens' CompilationJobSummary (Lude.Maybe TargetPlatformArch)
cjsCompilationTargetPlatformArch = Lens.lens (compilationTargetPlatformArch :: CompilationJobSummary -> Lude.Maybe TargetPlatformArch) (\s a -> s {compilationTargetPlatformArch = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationTargetPlatformArch "Use generic-lens or generic-optics with 'compilationTargetPlatformArch' instead." #-}

-- | The status of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobStatus :: Lens.Lens' CompilationJobSummary CompilationJobStatus
cjsCompilationJobStatus = Lens.lens (compilationJobStatus :: CompilationJobSummary -> CompilationJobStatus) (\s a -> s {compilationJobStatus = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationJobStatus "Use generic-lens or generic-optics with 'compilationJobStatus' instead." #-}

-- | The time when the model compilation job completed.
--
-- /Note:/ Consider using 'compilationEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationEndTime :: Lens.Lens' CompilationJobSummary (Lude.Maybe Lude.Timestamp)
cjsCompilationEndTime = Lens.lens (compilationEndTime :: CompilationJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {compilationEndTime = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationEndTime "Use generic-lens or generic-optics with 'compilationEndTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationJobARN :: Lens.Lens' CompilationJobSummary Lude.Text
cjsCompilationJobARN = Lens.lens (compilationJobARN :: CompilationJobSummary -> Lude.Text) (\s a -> s {compilationJobARN = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationJobARN "Use generic-lens or generic-optics with 'compilationJobARN' instead." #-}

-- | The type of OS that the model will run on after the compilation job has completed.
--
-- /Note:/ Consider using 'compilationTargetPlatformOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjsCompilationTargetPlatformOS :: Lens.Lens' CompilationJobSummary (Lude.Maybe TargetPlatformOS)
cjsCompilationTargetPlatformOS = Lens.lens (compilationTargetPlatformOS :: CompilationJobSummary -> Lude.Maybe TargetPlatformOS) (\s a -> s {compilationTargetPlatformOS = a} :: CompilationJobSummary)
{-# DEPRECATED cjsCompilationTargetPlatformOS "Use generic-lens or generic-optics with 'compilationTargetPlatformOS' instead." #-}

instance Lude.FromJSON CompilationJobSummary where
  parseJSON =
    Lude.withObject
      "CompilationJobSummary"
      ( \x ->
          CompilationJobSummary'
            Lude.<$> (x Lude..:? "CompilationStartTime")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..:? "CompilationTargetPlatformAccelerator")
            Lude.<*> (x Lude..:? "CompilationTargetDevice")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..: "CompilationJobName")
            Lude.<*> (x Lude..:? "CompilationTargetPlatformArch")
            Lude.<*> (x Lude..: "CompilationJobStatus")
            Lude.<*> (x Lude..:? "CompilationEndTime")
            Lude.<*> (x Lude..: "CompilationJobArn")
            Lude.<*> (x Lude..:? "CompilationTargetPlatformOs")
      )
