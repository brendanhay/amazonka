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
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
import Network.AWS.SageMaker.Types.AutoMLJobStatus

-- | Provides a summary about a job.
--
-- /See:/ 'newAutoMLJobSummary' smart constructor.
data AutoMLJobSummary = AutoMLJobSummary'
  { -- | The end time of an AutoML job.
    endTime :: Core.Maybe Core.POSIX,
    -- | The failure reason of a job.
    failureReason :: Core.Maybe Core.Text,
    -- | The name of the object you are requesting.
    autoMLJobName :: Core.Text,
    -- | The ARN of the job.
    autoMLJobArn :: Core.Text,
    -- | The job\'s status.
    autoMLJobStatus :: AutoMLJobStatus,
    -- | The job\'s secondary status.
    autoMLJobSecondaryStatus :: AutoMLJobSecondaryStatus,
    -- | When the job was created.
    creationTime :: Core.POSIX,
    -- | When the job was last modified.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoMLJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'autoMLJobSummary_endTime' - The end time of an AutoML job.
--
-- 'failureReason', 'autoMLJobSummary_failureReason' - The failure reason of a job.
--
-- 'autoMLJobName', 'autoMLJobSummary_autoMLJobName' - The name of the object you are requesting.
--
-- 'autoMLJobArn', 'autoMLJobSummary_autoMLJobArn' - The ARN of the job.
--
-- 'autoMLJobStatus', 'autoMLJobSummary_autoMLJobStatus' - The job\'s status.
--
-- 'autoMLJobSecondaryStatus', 'autoMLJobSummary_autoMLJobSecondaryStatus' - The job\'s secondary status.
--
-- 'creationTime', 'autoMLJobSummary_creationTime' - When the job was created.
--
-- 'lastModifiedTime', 'autoMLJobSummary_lastModifiedTime' - When the job was last modified.
newAutoMLJobSummary ::
  -- | 'autoMLJobName'
  Core.Text ->
  -- | 'autoMLJobArn'
  Core.Text ->
  -- | 'autoMLJobStatus'
  AutoMLJobStatus ->
  -- | 'autoMLJobSecondaryStatus'
  AutoMLJobSecondaryStatus ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  AutoMLJobSummary
newAutoMLJobSummary
  pAutoMLJobName_
  pAutoMLJobArn_
  pAutoMLJobStatus_
  pAutoMLJobSecondaryStatus_
  pCreationTime_
  pLastModifiedTime_ =
    AutoMLJobSummary'
      { endTime = Core.Nothing,
        failureReason = Core.Nothing,
        autoMLJobName = pAutoMLJobName_,
        autoMLJobArn = pAutoMLJobArn_,
        autoMLJobStatus = pAutoMLJobStatus_,
        autoMLJobSecondaryStatus =
          pAutoMLJobSecondaryStatus_,
        creationTime = Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | The end time of an AutoML job.
autoMLJobSummary_endTime :: Lens.Lens' AutoMLJobSummary (Core.Maybe Core.UTCTime)
autoMLJobSummary_endTime = Lens.lens (\AutoMLJobSummary' {endTime} -> endTime) (\s@AutoMLJobSummary' {} a -> s {endTime = a} :: AutoMLJobSummary) Core.. Lens.mapping Core._Time

-- | The failure reason of a job.
autoMLJobSummary_failureReason :: Lens.Lens' AutoMLJobSummary (Core.Maybe Core.Text)
autoMLJobSummary_failureReason = Lens.lens (\AutoMLJobSummary' {failureReason} -> failureReason) (\s@AutoMLJobSummary' {} a -> s {failureReason = a} :: AutoMLJobSummary)

-- | The name of the object you are requesting.
autoMLJobSummary_autoMLJobName :: Lens.Lens' AutoMLJobSummary Core.Text
autoMLJobSummary_autoMLJobName = Lens.lens (\AutoMLJobSummary' {autoMLJobName} -> autoMLJobName) (\s@AutoMLJobSummary' {} a -> s {autoMLJobName = a} :: AutoMLJobSummary)

-- | The ARN of the job.
autoMLJobSummary_autoMLJobArn :: Lens.Lens' AutoMLJobSummary Core.Text
autoMLJobSummary_autoMLJobArn = Lens.lens (\AutoMLJobSummary' {autoMLJobArn} -> autoMLJobArn) (\s@AutoMLJobSummary' {} a -> s {autoMLJobArn = a} :: AutoMLJobSummary)

-- | The job\'s status.
autoMLJobSummary_autoMLJobStatus :: Lens.Lens' AutoMLJobSummary AutoMLJobStatus
autoMLJobSummary_autoMLJobStatus = Lens.lens (\AutoMLJobSummary' {autoMLJobStatus} -> autoMLJobStatus) (\s@AutoMLJobSummary' {} a -> s {autoMLJobStatus = a} :: AutoMLJobSummary)

-- | The job\'s secondary status.
autoMLJobSummary_autoMLJobSecondaryStatus :: Lens.Lens' AutoMLJobSummary AutoMLJobSecondaryStatus
autoMLJobSummary_autoMLJobSecondaryStatus = Lens.lens (\AutoMLJobSummary' {autoMLJobSecondaryStatus} -> autoMLJobSecondaryStatus) (\s@AutoMLJobSummary' {} a -> s {autoMLJobSecondaryStatus = a} :: AutoMLJobSummary)

-- | When the job was created.
autoMLJobSummary_creationTime :: Lens.Lens' AutoMLJobSummary Core.UTCTime
autoMLJobSummary_creationTime = Lens.lens (\AutoMLJobSummary' {creationTime} -> creationTime) (\s@AutoMLJobSummary' {} a -> s {creationTime = a} :: AutoMLJobSummary) Core.. Core._Time

-- | When the job was last modified.
autoMLJobSummary_lastModifiedTime :: Lens.Lens' AutoMLJobSummary Core.UTCTime
autoMLJobSummary_lastModifiedTime = Lens.lens (\AutoMLJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@AutoMLJobSummary' {} a -> s {lastModifiedTime = a} :: AutoMLJobSummary) Core.. Core._Time

instance Core.FromJSON AutoMLJobSummary where
  parseJSON =
    Core.withObject
      "AutoMLJobSummary"
      ( \x ->
          AutoMLJobSummary'
            Core.<$> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..: "AutoMLJobName")
            Core.<*> (x Core..: "AutoMLJobArn")
            Core.<*> (x Core..: "AutoMLJobStatus")
            Core.<*> (x Core..: "AutoMLJobSecondaryStatus")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "LastModifiedTime")
      )

instance Core.Hashable AutoMLJobSummary

instance Core.NFData AutoMLJobSummary
