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
-- Module      : Amazonka.SageMaker.Types.CompilationJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CompilationJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CompilationJobStatus
import Amazonka.SageMaker.Types.TargetDevice
import Amazonka.SageMaker.Types.TargetPlatformAccelerator
import Amazonka.SageMaker.Types.TargetPlatformArch
import Amazonka.SageMaker.Types.TargetPlatformOs

-- | A summary of a model compilation job.
--
-- /See:/ 'newCompilationJobSummary' smart constructor.
data CompilationJobSummary = CompilationJobSummary'
  { -- | The time when the model compilation job completed.
    compilationEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the model compilation job started.
    compilationStartTime :: Prelude.Maybe Data.POSIX,
    -- | The type of device that the model will run on after the compilation job
    -- has completed.
    compilationTargetDevice :: Prelude.Maybe TargetDevice,
    -- | The type of accelerator that the model will run on after the compilation
    -- job has completed.
    compilationTargetPlatformAccelerator :: Prelude.Maybe TargetPlatformAccelerator,
    -- | The type of architecture that the model will run on after the
    -- compilation job has completed.
    compilationTargetPlatformArch :: Prelude.Maybe TargetPlatformArch,
    -- | The type of OS that the model will run on after the compilation job has
    -- completed.
    compilationTargetPlatformOs :: Prelude.Maybe TargetPlatformOs,
    -- | The time when the model compilation job was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the model compilation job that you want a summary for.
    compilationJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model compilation job.
    compilationJobArn :: Prelude.Text,
    -- | The time when the model compilation job was created.
    creationTime :: Data.POSIX,
    -- | The status of the model compilation job.
    compilationJobStatus :: CompilationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompilationJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilationEndTime', 'compilationJobSummary_compilationEndTime' - The time when the model compilation job completed.
--
-- 'compilationStartTime', 'compilationJobSummary_compilationStartTime' - The time when the model compilation job started.
--
-- 'compilationTargetDevice', 'compilationJobSummary_compilationTargetDevice' - The type of device that the model will run on after the compilation job
-- has completed.
--
-- 'compilationTargetPlatformAccelerator', 'compilationJobSummary_compilationTargetPlatformAccelerator' - The type of accelerator that the model will run on after the compilation
-- job has completed.
--
-- 'compilationTargetPlatformArch', 'compilationJobSummary_compilationTargetPlatformArch' - The type of architecture that the model will run on after the
-- compilation job has completed.
--
-- 'compilationTargetPlatformOs', 'compilationJobSummary_compilationTargetPlatformOs' - The type of OS that the model will run on after the compilation job has
-- completed.
--
-- 'lastModifiedTime', 'compilationJobSummary_lastModifiedTime' - The time when the model compilation job was last modified.
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
  Prelude.Text ->
  -- | 'compilationJobArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'compilationJobStatus'
  CompilationJobStatus ->
  CompilationJobSummary
newCompilationJobSummary
  pCompilationJobName_
  pCompilationJobArn_
  pCreationTime_
  pCompilationJobStatus_ =
    CompilationJobSummary'
      { compilationEndTime =
          Prelude.Nothing,
        compilationStartTime = Prelude.Nothing,
        compilationTargetDevice = Prelude.Nothing,
        compilationTargetPlatformAccelerator =
          Prelude.Nothing,
        compilationTargetPlatformArch = Prelude.Nothing,
        compilationTargetPlatformOs = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        compilationJobName = pCompilationJobName_,
        compilationJobArn = pCompilationJobArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        compilationJobStatus = pCompilationJobStatus_
      }

-- | The time when the model compilation job completed.
compilationJobSummary_compilationEndTime :: Lens.Lens' CompilationJobSummary (Prelude.Maybe Prelude.UTCTime)
compilationJobSummary_compilationEndTime = Lens.lens (\CompilationJobSummary' {compilationEndTime} -> compilationEndTime) (\s@CompilationJobSummary' {} a -> s {compilationEndTime = a} :: CompilationJobSummary) Prelude.. Lens.mapping Data._Time

-- | The time when the model compilation job started.
compilationJobSummary_compilationStartTime :: Lens.Lens' CompilationJobSummary (Prelude.Maybe Prelude.UTCTime)
compilationJobSummary_compilationStartTime = Lens.lens (\CompilationJobSummary' {compilationStartTime} -> compilationStartTime) (\s@CompilationJobSummary' {} a -> s {compilationStartTime = a} :: CompilationJobSummary) Prelude.. Lens.mapping Data._Time

-- | The type of device that the model will run on after the compilation job
-- has completed.
compilationJobSummary_compilationTargetDevice :: Lens.Lens' CompilationJobSummary (Prelude.Maybe TargetDevice)
compilationJobSummary_compilationTargetDevice = Lens.lens (\CompilationJobSummary' {compilationTargetDevice} -> compilationTargetDevice) (\s@CompilationJobSummary' {} a -> s {compilationTargetDevice = a} :: CompilationJobSummary)

-- | The type of accelerator that the model will run on after the compilation
-- job has completed.
compilationJobSummary_compilationTargetPlatformAccelerator :: Lens.Lens' CompilationJobSummary (Prelude.Maybe TargetPlatformAccelerator)
compilationJobSummary_compilationTargetPlatformAccelerator = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformAccelerator} -> compilationTargetPlatformAccelerator) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformAccelerator = a} :: CompilationJobSummary)

-- | The type of architecture that the model will run on after the
-- compilation job has completed.
compilationJobSummary_compilationTargetPlatformArch :: Lens.Lens' CompilationJobSummary (Prelude.Maybe TargetPlatformArch)
compilationJobSummary_compilationTargetPlatformArch = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformArch} -> compilationTargetPlatformArch) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformArch = a} :: CompilationJobSummary)

-- | The type of OS that the model will run on after the compilation job has
-- completed.
compilationJobSummary_compilationTargetPlatformOs :: Lens.Lens' CompilationJobSummary (Prelude.Maybe TargetPlatformOs)
compilationJobSummary_compilationTargetPlatformOs = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformOs} -> compilationTargetPlatformOs) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformOs = a} :: CompilationJobSummary)

-- | The time when the model compilation job was last modified.
compilationJobSummary_lastModifiedTime :: Lens.Lens' CompilationJobSummary (Prelude.Maybe Prelude.UTCTime)
compilationJobSummary_lastModifiedTime = Lens.lens (\CompilationJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@CompilationJobSummary' {} a -> s {lastModifiedTime = a} :: CompilationJobSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the model compilation job that you want a summary for.
compilationJobSummary_compilationJobName :: Lens.Lens' CompilationJobSummary Prelude.Text
compilationJobSummary_compilationJobName = Lens.lens (\CompilationJobSummary' {compilationJobName} -> compilationJobName) (\s@CompilationJobSummary' {} a -> s {compilationJobName = a} :: CompilationJobSummary)

-- | The Amazon Resource Name (ARN) of the model compilation job.
compilationJobSummary_compilationJobArn :: Lens.Lens' CompilationJobSummary Prelude.Text
compilationJobSummary_compilationJobArn = Lens.lens (\CompilationJobSummary' {compilationJobArn} -> compilationJobArn) (\s@CompilationJobSummary' {} a -> s {compilationJobArn = a} :: CompilationJobSummary)

-- | The time when the model compilation job was created.
compilationJobSummary_creationTime :: Lens.Lens' CompilationJobSummary Prelude.UTCTime
compilationJobSummary_creationTime = Lens.lens (\CompilationJobSummary' {creationTime} -> creationTime) (\s@CompilationJobSummary' {} a -> s {creationTime = a} :: CompilationJobSummary) Prelude.. Data._Time

-- | The status of the model compilation job.
compilationJobSummary_compilationJobStatus :: Lens.Lens' CompilationJobSummary CompilationJobStatus
compilationJobSummary_compilationJobStatus = Lens.lens (\CompilationJobSummary' {compilationJobStatus} -> compilationJobStatus) (\s@CompilationJobSummary' {} a -> s {compilationJobStatus = a} :: CompilationJobSummary)

instance Data.FromJSON CompilationJobSummary where
  parseJSON =
    Data.withObject
      "CompilationJobSummary"
      ( \x ->
          CompilationJobSummary'
            Prelude.<$> (x Data..:? "CompilationEndTime")
            Prelude.<*> (x Data..:? "CompilationStartTime")
            Prelude.<*> (x Data..:? "CompilationTargetDevice")
            Prelude.<*> (x Data..:? "CompilationTargetPlatformAccelerator")
            Prelude.<*> (x Data..:? "CompilationTargetPlatformArch")
            Prelude.<*> (x Data..:? "CompilationTargetPlatformOs")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..: "CompilationJobName")
            Prelude.<*> (x Data..: "CompilationJobArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "CompilationJobStatus")
      )

instance Prelude.Hashable CompilationJobSummary where
  hashWithSalt _salt CompilationJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` compilationEndTime
      `Prelude.hashWithSalt` compilationStartTime
      `Prelude.hashWithSalt` compilationTargetDevice
      `Prelude.hashWithSalt` compilationTargetPlatformAccelerator
      `Prelude.hashWithSalt` compilationTargetPlatformArch
      `Prelude.hashWithSalt` compilationTargetPlatformOs
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` compilationJobName
      `Prelude.hashWithSalt` compilationJobArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` compilationJobStatus

instance Prelude.NFData CompilationJobSummary where
  rnf CompilationJobSummary' {..} =
    Prelude.rnf compilationEndTime
      `Prelude.seq` Prelude.rnf compilationStartTime
      `Prelude.seq` Prelude.rnf compilationTargetDevice
      `Prelude.seq` Prelude.rnf compilationTargetPlatformAccelerator
      `Prelude.seq` Prelude.rnf compilationTargetPlatformArch
      `Prelude.seq` Prelude.rnf compilationTargetPlatformOs
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf compilationJobName
      `Prelude.seq` Prelude.rnf compilationJobArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf compilationJobStatus
