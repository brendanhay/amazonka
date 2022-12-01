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
-- Module      : Amazonka.SageMaker.Types.TransformJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TransformJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TransformJobStatus

-- | Provides a summary of a transform job. Multiple @TransformJobSummary@
-- objects are returned as a list after in response to a ListTransformJobs
-- call.
--
-- /See:/ 'newTransformJobSummary' smart constructor.
data TransformJobSummary = TransformJobSummary'
  { -- | Indicates when the transform job ends on compute instances. For
    -- successful jobs and stopped jobs, this is the exact time recorded after
    -- the results are uploaded. For failed jobs, this is when Amazon SageMaker
    -- detected that the job failed.
    transformEndTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates when the transform job was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | If the transform job failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the transform job.
    transformJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Prelude.Text,
    -- | A timestamp that shows when the transform Job was created.
    creationTime :: Core.POSIX,
    -- | The status of the transform job.
    transformJobStatus :: TransformJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformEndTime', 'transformJobSummary_transformEndTime' - Indicates when the transform job ends on compute instances. For
-- successful jobs and stopped jobs, this is the exact time recorded after
-- the results are uploaded. For failed jobs, this is when Amazon SageMaker
-- detected that the job failed.
--
-- 'lastModifiedTime', 'transformJobSummary_lastModifiedTime' - Indicates when the transform job was last modified.
--
-- 'failureReason', 'transformJobSummary_failureReason' - If the transform job failed, the reason it failed.
--
-- 'transformJobName', 'transformJobSummary_transformJobName' - The name of the transform job.
--
-- 'transformJobArn', 'transformJobSummary_transformJobArn' - The Amazon Resource Name (ARN) of the transform job.
--
-- 'creationTime', 'transformJobSummary_creationTime' - A timestamp that shows when the transform Job was created.
--
-- 'transformJobStatus', 'transformJobSummary_transformJobStatus' - The status of the transform job.
newTransformJobSummary ::
  -- | 'transformJobName'
  Prelude.Text ->
  -- | 'transformJobArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'transformJobStatus'
  TransformJobStatus ->
  TransformJobSummary
newTransformJobSummary
  pTransformJobName_
  pTransformJobArn_
  pCreationTime_
  pTransformJobStatus_ =
    TransformJobSummary'
      { transformEndTime =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        transformJobName = pTransformJobName_,
        transformJobArn = pTransformJobArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        transformJobStatus = pTransformJobStatus_
      }

-- | Indicates when the transform job ends on compute instances. For
-- successful jobs and stopped jobs, this is the exact time recorded after
-- the results are uploaded. For failed jobs, this is when Amazon SageMaker
-- detected that the job failed.
transformJobSummary_transformEndTime :: Lens.Lens' TransformJobSummary (Prelude.Maybe Prelude.UTCTime)
transformJobSummary_transformEndTime = Lens.lens (\TransformJobSummary' {transformEndTime} -> transformEndTime) (\s@TransformJobSummary' {} a -> s {transformEndTime = a} :: TransformJobSummary) Prelude.. Lens.mapping Core._Time

-- | Indicates when the transform job was last modified.
transformJobSummary_lastModifiedTime :: Lens.Lens' TransformJobSummary (Prelude.Maybe Prelude.UTCTime)
transformJobSummary_lastModifiedTime = Lens.lens (\TransformJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TransformJobSummary' {} a -> s {lastModifiedTime = a} :: TransformJobSummary) Prelude.. Lens.mapping Core._Time

-- | If the transform job failed, the reason it failed.
transformJobSummary_failureReason :: Lens.Lens' TransformJobSummary (Prelude.Maybe Prelude.Text)
transformJobSummary_failureReason = Lens.lens (\TransformJobSummary' {failureReason} -> failureReason) (\s@TransformJobSummary' {} a -> s {failureReason = a} :: TransformJobSummary)

-- | The name of the transform job.
transformJobSummary_transformJobName :: Lens.Lens' TransformJobSummary Prelude.Text
transformJobSummary_transformJobName = Lens.lens (\TransformJobSummary' {transformJobName} -> transformJobName) (\s@TransformJobSummary' {} a -> s {transformJobName = a} :: TransformJobSummary)

-- | The Amazon Resource Name (ARN) of the transform job.
transformJobSummary_transformJobArn :: Lens.Lens' TransformJobSummary Prelude.Text
transformJobSummary_transformJobArn = Lens.lens (\TransformJobSummary' {transformJobArn} -> transformJobArn) (\s@TransformJobSummary' {} a -> s {transformJobArn = a} :: TransformJobSummary)

-- | A timestamp that shows when the transform Job was created.
transformJobSummary_creationTime :: Lens.Lens' TransformJobSummary Prelude.UTCTime
transformJobSummary_creationTime = Lens.lens (\TransformJobSummary' {creationTime} -> creationTime) (\s@TransformJobSummary' {} a -> s {creationTime = a} :: TransformJobSummary) Prelude.. Core._Time

-- | The status of the transform job.
transformJobSummary_transformJobStatus :: Lens.Lens' TransformJobSummary TransformJobStatus
transformJobSummary_transformJobStatus = Lens.lens (\TransformJobSummary' {transformJobStatus} -> transformJobStatus) (\s@TransformJobSummary' {} a -> s {transformJobStatus = a} :: TransformJobSummary)

instance Core.FromJSON TransformJobSummary where
  parseJSON =
    Core.withObject
      "TransformJobSummary"
      ( \x ->
          TransformJobSummary'
            Prelude.<$> (x Core..:? "TransformEndTime")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..: "TransformJobName")
            Prelude.<*> (x Core..: "TransformJobArn")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "TransformJobStatus")
      )

instance Prelude.Hashable TransformJobSummary where
  hashWithSalt _salt TransformJobSummary' {..} =
    _salt `Prelude.hashWithSalt` transformEndTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` transformJobName
      `Prelude.hashWithSalt` transformJobArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` transformJobStatus

instance Prelude.NFData TransformJobSummary where
  rnf TransformJobSummary' {..} =
    Prelude.rnf transformEndTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf transformJobName
      `Prelude.seq` Prelude.rnf transformJobArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf transformJobStatus
