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
-- Module      : Network.AWS.SageMaker.Types.CompilationJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CompilationJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.CompilationJobStatus
import Network.AWS.SageMaker.Types.TargetDevice
import Network.AWS.SageMaker.Types.TargetPlatformAccelerator
import Network.AWS.SageMaker.Types.TargetPlatformArch
import Network.AWS.SageMaker.Types.TargetPlatformOs

-- | A summary of a model compilation job.
--
-- /See:/ 'newCompilationJobSummary' smart constructor.
data CompilationJobSummary = CompilationJobSummary'
  { -- | The type of architecture that the model will run on after the
    -- compilation job has completed.
    compilationTargetPlatformArch :: Core.Maybe TargetPlatformArch,
    -- | The time when the model compilation job started.
    compilationStartTime :: Core.Maybe Core.POSIX,
    -- | The type of OS that the model will run on after the compilation job has
    -- completed.
    compilationTargetPlatformOs :: Core.Maybe TargetPlatformOs,
    -- | The type of accelerator that the model will run on after the compilation
    -- job has completed.
    compilationTargetPlatformAccelerator :: Core.Maybe TargetPlatformAccelerator,
    -- | The time when the model compilation job was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The time when the model compilation job completed.
    compilationEndTime :: Core.Maybe Core.POSIX,
    -- | The type of device that the model will run on after the compilation job
    -- has completed.
    compilationTargetDevice :: Core.Maybe TargetDevice,
    -- | The name of the model compilation job that you want a summary for.
    compilationJobName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the model compilation job.
    compilationJobArn :: Core.Text,
    -- | The time when the model compilation job was created.
    creationTime :: Core.POSIX,
    -- | The status of the model compilation job.
    compilationJobStatus :: CompilationJobStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CompilationJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilationTargetPlatformArch', 'compilationJobSummary_compilationTargetPlatformArch' - The type of architecture that the model will run on after the
-- compilation job has completed.
--
-- 'compilationStartTime', 'compilationJobSummary_compilationStartTime' - The time when the model compilation job started.
--
-- 'compilationTargetPlatformOs', 'compilationJobSummary_compilationTargetPlatformOs' - The type of OS that the model will run on after the compilation job has
-- completed.
--
-- 'compilationTargetPlatformAccelerator', 'compilationJobSummary_compilationTargetPlatformAccelerator' - The type of accelerator that the model will run on after the compilation
-- job has completed.
--
-- 'lastModifiedTime', 'compilationJobSummary_lastModifiedTime' - The time when the model compilation job was last modified.
--
-- 'compilationEndTime', 'compilationJobSummary_compilationEndTime' - The time when the model compilation job completed.
--
-- 'compilationTargetDevice', 'compilationJobSummary_compilationTargetDevice' - The type of device that the model will run on after the compilation job
-- has completed.
--
-- 'compilationJobName', 'compilationJobSummary_compilationJobName' - The name of the model compilation job that you want a summary for.
--
-- 'compilationJobArn', 'compilationJobSummary_compilationJobArn' - The Amazon Resource Name (ARN) of the model compilation job.
--
-- 'creationTime', 'compilationJobSummary_creationTime' - The time when the model compilation job was created.
--
-- 'compilationJobStatus', 'compilationJobSummary_compilationJobStatus' - The status of the model compilation job.
newCompilationJobSummary ::
  -- | 'compilationJobName'
  Core.Text ->
  -- | 'compilationJobArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'compilationJobStatus'
  CompilationJobStatus ->
  CompilationJobSummary
newCompilationJobSummary
  pCompilationJobName_
  pCompilationJobArn_
  pCreationTime_
  pCompilationJobStatus_ =
    CompilationJobSummary'
      { compilationTargetPlatformArch =
          Core.Nothing,
        compilationStartTime = Core.Nothing,
        compilationTargetPlatformOs = Core.Nothing,
        compilationTargetPlatformAccelerator = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        compilationEndTime = Core.Nothing,
        compilationTargetDevice = Core.Nothing,
        compilationJobName = pCompilationJobName_,
        compilationJobArn = pCompilationJobArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        compilationJobStatus = pCompilationJobStatus_
      }

-- | The type of architecture that the model will run on after the
-- compilation job has completed.
compilationJobSummary_compilationTargetPlatformArch :: Lens.Lens' CompilationJobSummary (Core.Maybe TargetPlatformArch)
compilationJobSummary_compilationTargetPlatformArch = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformArch} -> compilationTargetPlatformArch) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformArch = a} :: CompilationJobSummary)

-- | The time when the model compilation job started.
compilationJobSummary_compilationStartTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.UTCTime)
compilationJobSummary_compilationStartTime = Lens.lens (\CompilationJobSummary' {compilationStartTime} -> compilationStartTime) (\s@CompilationJobSummary' {} a -> s {compilationStartTime = a} :: CompilationJobSummary) Core.. Lens.mapping Core._Time

-- | The type of OS that the model will run on after the compilation job has
-- completed.
compilationJobSummary_compilationTargetPlatformOs :: Lens.Lens' CompilationJobSummary (Core.Maybe TargetPlatformOs)
compilationJobSummary_compilationTargetPlatformOs = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformOs} -> compilationTargetPlatformOs) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformOs = a} :: CompilationJobSummary)

-- | The type of accelerator that the model will run on after the compilation
-- job has completed.
compilationJobSummary_compilationTargetPlatformAccelerator :: Lens.Lens' CompilationJobSummary (Core.Maybe TargetPlatformAccelerator)
compilationJobSummary_compilationTargetPlatformAccelerator = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformAccelerator} -> compilationTargetPlatformAccelerator) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformAccelerator = a} :: CompilationJobSummary)

-- | The time when the model compilation job was last modified.
compilationJobSummary_lastModifiedTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.UTCTime)
compilationJobSummary_lastModifiedTime = Lens.lens (\CompilationJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@CompilationJobSummary' {} a -> s {lastModifiedTime = a} :: CompilationJobSummary) Core.. Lens.mapping Core._Time

-- | The time when the model compilation job completed.
compilationJobSummary_compilationEndTime :: Lens.Lens' CompilationJobSummary (Core.Maybe Core.UTCTime)
compilationJobSummary_compilationEndTime = Lens.lens (\CompilationJobSummary' {compilationEndTime} -> compilationEndTime) (\s@CompilationJobSummary' {} a -> s {compilationEndTime = a} :: CompilationJobSummary) Core.. Lens.mapping Core._Time

-- | The type of device that the model will run on after the compilation job
-- has completed.
compilationJobSummary_compilationTargetDevice :: Lens.Lens' CompilationJobSummary (Core.Maybe TargetDevice)
compilationJobSummary_compilationTargetDevice = Lens.lens (\CompilationJobSummary' {compilationTargetDevice} -> compilationTargetDevice) (\s@CompilationJobSummary' {} a -> s {compilationTargetDevice = a} :: CompilationJobSummary)

-- | The name of the model compilation job that you want a summary for.
compilationJobSummary_compilationJobName :: Lens.Lens' CompilationJobSummary Core.Text
compilationJobSummary_compilationJobName = Lens.lens (\CompilationJobSummary' {compilationJobName} -> compilationJobName) (\s@CompilationJobSummary' {} a -> s {compilationJobName = a} :: CompilationJobSummary)

-- | The Amazon Resource Name (ARN) of the model compilation job.
compilationJobSummary_compilationJobArn :: Lens.Lens' CompilationJobSummary Core.Text
compilationJobSummary_compilationJobArn = Lens.lens (\CompilationJobSummary' {compilationJobArn} -> compilationJobArn) (\s@CompilationJobSummary' {} a -> s {compilationJobArn = a} :: CompilationJobSummary)

-- | The time when the model compilation job was created.
compilationJobSummary_creationTime :: Lens.Lens' CompilationJobSummary Core.UTCTime
compilationJobSummary_creationTime = Lens.lens (\CompilationJobSummary' {creationTime} -> creationTime) (\s@CompilationJobSummary' {} a -> s {creationTime = a} :: CompilationJobSummary) Core.. Core._Time

-- | The status of the model compilation job.
compilationJobSummary_compilationJobStatus :: Lens.Lens' CompilationJobSummary CompilationJobStatus
compilationJobSummary_compilationJobStatus = Lens.lens (\CompilationJobSummary' {compilationJobStatus} -> compilationJobStatus) (\s@CompilationJobSummary' {} a -> s {compilationJobStatus = a} :: CompilationJobSummary)

instance Core.FromJSON CompilationJobSummary where
  parseJSON =
    Core.withObject
      "CompilationJobSummary"
      ( \x ->
          CompilationJobSummary'
            Core.<$> (x Core..:? "CompilationTargetPlatformArch")
            Core.<*> (x Core..:? "CompilationStartTime")
            Core.<*> (x Core..:? "CompilationTargetPlatformOs")
            Core.<*> (x Core..:? "CompilationTargetPlatformAccelerator")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "CompilationEndTime")
            Core.<*> (x Core..:? "CompilationTargetDevice")
            Core.<*> (x Core..: "CompilationJobName")
            Core.<*> (x Core..: "CompilationJobArn")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "CompilationJobStatus")
      )

instance Core.Hashable CompilationJobSummary

instance Core.NFData CompilationJobSummary
