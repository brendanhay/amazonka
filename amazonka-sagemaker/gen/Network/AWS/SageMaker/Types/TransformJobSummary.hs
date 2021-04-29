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
-- Module      : Network.AWS.SageMaker.Types.TransformJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJobSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TransformJobStatus

-- | Provides a summary of a transform job. Multiple @TransformJobSummary@
-- objects are returned as a list after in response to a ListTransformJobs
-- call.
--
-- /See:/ 'newTransformJobSummary' smart constructor.
data TransformJobSummary = TransformJobSummary'
  { -- | If the transform job failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the transform job was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | Indicates when the transform job ends on compute instances. For
    -- successful jobs and stopped jobs, this is the exact time recorded after
    -- the results are uploaded. For failed jobs, this is when Amazon SageMaker
    -- detected that the job failed.
    transformEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the transform job.
    transformJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobArn :: Prelude.Text,
    -- | A timestamp that shows when the transform Job was created.
    creationTime :: Prelude.POSIX,
    -- | The status of the transform job.
    transformJobStatus :: TransformJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransformJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'transformJobSummary_failureReason' - If the transform job failed, the reason it failed.
--
-- 'lastModifiedTime', 'transformJobSummary_lastModifiedTime' - Indicates when the transform job was last modified.
--
-- 'transformEndTime', 'transformJobSummary_transformEndTime' - Indicates when the transform job ends on compute instances. For
-- successful jobs and stopped jobs, this is the exact time recorded after
-- the results are uploaded. For failed jobs, this is when Amazon SageMaker
-- detected that the job failed.
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
      { failureReason =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        transformEndTime = Prelude.Nothing,
        transformJobName = pTransformJobName_,
        transformJobArn = pTransformJobArn_,
        creationTime = Prelude._Time Lens.# pCreationTime_,
        transformJobStatus = pTransformJobStatus_
      }

-- | If the transform job failed, the reason it failed.
transformJobSummary_failureReason :: Lens.Lens' TransformJobSummary (Prelude.Maybe Prelude.Text)
transformJobSummary_failureReason = Lens.lens (\TransformJobSummary' {failureReason} -> failureReason) (\s@TransformJobSummary' {} a -> s {failureReason = a} :: TransformJobSummary)

-- | Indicates when the transform job was last modified.
transformJobSummary_lastModifiedTime :: Lens.Lens' TransformJobSummary (Prelude.Maybe Prelude.UTCTime)
transformJobSummary_lastModifiedTime = Lens.lens (\TransformJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TransformJobSummary' {} a -> s {lastModifiedTime = a} :: TransformJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | Indicates when the transform job ends on compute instances. For
-- successful jobs and stopped jobs, this is the exact time recorded after
-- the results are uploaded. For failed jobs, this is when Amazon SageMaker
-- detected that the job failed.
transformJobSummary_transformEndTime :: Lens.Lens' TransformJobSummary (Prelude.Maybe Prelude.UTCTime)
transformJobSummary_transformEndTime = Lens.lens (\TransformJobSummary' {transformEndTime} -> transformEndTime) (\s@TransformJobSummary' {} a -> s {transformEndTime = a} :: TransformJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the transform job.
transformJobSummary_transformJobName :: Lens.Lens' TransformJobSummary Prelude.Text
transformJobSummary_transformJobName = Lens.lens (\TransformJobSummary' {transformJobName} -> transformJobName) (\s@TransformJobSummary' {} a -> s {transformJobName = a} :: TransformJobSummary)

-- | The Amazon Resource Name (ARN) of the transform job.
transformJobSummary_transformJobArn :: Lens.Lens' TransformJobSummary Prelude.Text
transformJobSummary_transformJobArn = Lens.lens (\TransformJobSummary' {transformJobArn} -> transformJobArn) (\s@TransformJobSummary' {} a -> s {transformJobArn = a} :: TransformJobSummary)

-- | A timestamp that shows when the transform Job was created.
transformJobSummary_creationTime :: Lens.Lens' TransformJobSummary Prelude.UTCTime
transformJobSummary_creationTime = Lens.lens (\TransformJobSummary' {creationTime} -> creationTime) (\s@TransformJobSummary' {} a -> s {creationTime = a} :: TransformJobSummary) Prelude.. Prelude._Time

-- | The status of the transform job.
transformJobSummary_transformJobStatus :: Lens.Lens' TransformJobSummary TransformJobStatus
transformJobSummary_transformJobStatus = Lens.lens (\TransformJobSummary' {transformJobStatus} -> transformJobStatus) (\s@TransformJobSummary' {} a -> s {transformJobStatus = a} :: TransformJobSummary)

instance Prelude.FromJSON TransformJobSummary where
  parseJSON =
    Prelude.withObject
      "TransformJobSummary"
      ( \x ->
          TransformJobSummary'
            Prelude.<$> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "TransformEndTime")
            Prelude.<*> (x Prelude..: "TransformJobName")
            Prelude.<*> (x Prelude..: "TransformJobArn")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "TransformJobStatus")
      )

instance Prelude.Hashable TransformJobSummary

instance Prelude.NFData TransformJobSummary
