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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.LabelCounters
import Network.AWS.SageMaker.Types.LabelingJobInputConfig
import Network.AWS.SageMaker.Types.LabelingJobOutput
import Network.AWS.SageMaker.Types.LabelingJobStatus

-- | Provides summary information about a labeling job.
--
-- /See:/ 'newLabelingJobSummary' smart constructor.
data LabelingJobSummary = LabelingJobSummary'
  { -- | The Amazon Resource Name (ARN) of the Lambda function used to
    -- consolidate the annotations from individual workers into a label for a
    -- data object. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation>.
    annotationConsolidationLambdaArn :: Prelude.Maybe Prelude.Text,
    -- | Input configuration for the labeling job.
    inputConfig :: Prelude.Maybe LabelingJobInputConfig,
    -- | If the @LabelingJobStatus@ field is @Failed@, this field contains a
    -- description of the error.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The location of the output produced by the labeling job.
    labelingJobOutput :: Prelude.Maybe LabelingJobOutput,
    -- | The name of the labeling job.
    labelingJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) assigned to the labeling job when it was
    -- created.
    labelingJobArn :: Prelude.Text,
    -- | The date and time that the job was created (timestamp).
    creationTime :: Prelude.POSIX,
    -- | The date and time that the job was last modified (timestamp).
    lastModifiedTime :: Prelude.POSIX,
    -- | The current status of the labeling job.
    labelingJobStatus :: LabelingJobStatus,
    -- | Counts showing the progress of the labeling job.
    labelCounters :: LabelCounters,
    -- | The Amazon Resource Name (ARN) of the work team assigned to the job.
    workteamArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a Lambda function. The function is run
    -- before each data object is sent to a worker.
    preHumanTaskLambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotationConsolidationLambdaArn', 'labelingJobSummary_annotationConsolidationLambdaArn' - The Amazon Resource Name (ARN) of the Lambda function used to
-- consolidate the annotations from individual workers into a label for a
-- data object. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation>.
--
-- 'inputConfig', 'labelingJobSummary_inputConfig' - Input configuration for the labeling job.
--
-- 'failureReason', 'labelingJobSummary_failureReason' - If the @LabelingJobStatus@ field is @Failed@, this field contains a
-- description of the error.
--
-- 'labelingJobOutput', 'labelingJobSummary_labelingJobOutput' - The location of the output produced by the labeling job.
--
-- 'labelingJobName', 'labelingJobSummary_labelingJobName' - The name of the labeling job.
--
-- 'labelingJobArn', 'labelingJobSummary_labelingJobArn' - The Amazon Resource Name (ARN) assigned to the labeling job when it was
-- created.
--
-- 'creationTime', 'labelingJobSummary_creationTime' - The date and time that the job was created (timestamp).
--
-- 'lastModifiedTime', 'labelingJobSummary_lastModifiedTime' - The date and time that the job was last modified (timestamp).
--
-- 'labelingJobStatus', 'labelingJobSummary_labelingJobStatus' - The current status of the labeling job.
--
-- 'labelCounters', 'labelingJobSummary_labelCounters' - Counts showing the progress of the labeling job.
--
-- 'workteamArn', 'labelingJobSummary_workteamArn' - The Amazon Resource Name (ARN) of the work team assigned to the job.
--
-- 'preHumanTaskLambdaArn', 'labelingJobSummary_preHumanTaskLambdaArn' - The Amazon Resource Name (ARN) of a Lambda function. The function is run
-- before each data object is sent to a worker.
newLabelingJobSummary ::
  -- | 'labelingJobName'
  Prelude.Text ->
  -- | 'labelingJobArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'labelingJobStatus'
  LabelingJobStatus ->
  -- | 'labelCounters'
  LabelCounters ->
  -- | 'workteamArn'
  Prelude.Text ->
  -- | 'preHumanTaskLambdaArn'
  Prelude.Text ->
  LabelingJobSummary
newLabelingJobSummary
  pLabelingJobName_
  pLabelingJobArn_
  pCreationTime_
  pLastModifiedTime_
  pLabelingJobStatus_
  pLabelCounters_
  pWorkteamArn_
  pPreHumanTaskLambdaArn_ =
    LabelingJobSummary'
      { annotationConsolidationLambdaArn =
          Prelude.Nothing,
        inputConfig = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        labelingJobOutput = Prelude.Nothing,
        labelingJobName = pLabelingJobName_,
        labelingJobArn = pLabelingJobArn_,
        creationTime = Prelude._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_,
        labelingJobStatus = pLabelingJobStatus_,
        labelCounters = pLabelCounters_,
        workteamArn = pWorkteamArn_,
        preHumanTaskLambdaArn = pPreHumanTaskLambdaArn_
      }

-- | The Amazon Resource Name (ARN) of the Lambda function used to
-- consolidate the annotations from individual workers into a label for a
-- data object. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation>.
labelingJobSummary_annotationConsolidationLambdaArn :: Lens.Lens' LabelingJobSummary (Prelude.Maybe Prelude.Text)
labelingJobSummary_annotationConsolidationLambdaArn = Lens.lens (\LabelingJobSummary' {annotationConsolidationLambdaArn} -> annotationConsolidationLambdaArn) (\s@LabelingJobSummary' {} a -> s {annotationConsolidationLambdaArn = a} :: LabelingJobSummary)

-- | Input configuration for the labeling job.
labelingJobSummary_inputConfig :: Lens.Lens' LabelingJobSummary (Prelude.Maybe LabelingJobInputConfig)
labelingJobSummary_inputConfig = Lens.lens (\LabelingJobSummary' {inputConfig} -> inputConfig) (\s@LabelingJobSummary' {} a -> s {inputConfig = a} :: LabelingJobSummary)

-- | If the @LabelingJobStatus@ field is @Failed@, this field contains a
-- description of the error.
labelingJobSummary_failureReason :: Lens.Lens' LabelingJobSummary (Prelude.Maybe Prelude.Text)
labelingJobSummary_failureReason = Lens.lens (\LabelingJobSummary' {failureReason} -> failureReason) (\s@LabelingJobSummary' {} a -> s {failureReason = a} :: LabelingJobSummary)

-- | The location of the output produced by the labeling job.
labelingJobSummary_labelingJobOutput :: Lens.Lens' LabelingJobSummary (Prelude.Maybe LabelingJobOutput)
labelingJobSummary_labelingJobOutput = Lens.lens (\LabelingJobSummary' {labelingJobOutput} -> labelingJobOutput) (\s@LabelingJobSummary' {} a -> s {labelingJobOutput = a} :: LabelingJobSummary)

-- | The name of the labeling job.
labelingJobSummary_labelingJobName :: Lens.Lens' LabelingJobSummary Prelude.Text
labelingJobSummary_labelingJobName = Lens.lens (\LabelingJobSummary' {labelingJobName} -> labelingJobName) (\s@LabelingJobSummary' {} a -> s {labelingJobName = a} :: LabelingJobSummary)

-- | The Amazon Resource Name (ARN) assigned to the labeling job when it was
-- created.
labelingJobSummary_labelingJobArn :: Lens.Lens' LabelingJobSummary Prelude.Text
labelingJobSummary_labelingJobArn = Lens.lens (\LabelingJobSummary' {labelingJobArn} -> labelingJobArn) (\s@LabelingJobSummary' {} a -> s {labelingJobArn = a} :: LabelingJobSummary)

-- | The date and time that the job was created (timestamp).
labelingJobSummary_creationTime :: Lens.Lens' LabelingJobSummary Prelude.UTCTime
labelingJobSummary_creationTime = Lens.lens (\LabelingJobSummary' {creationTime} -> creationTime) (\s@LabelingJobSummary' {} a -> s {creationTime = a} :: LabelingJobSummary) Prelude.. Prelude._Time

-- | The date and time that the job was last modified (timestamp).
labelingJobSummary_lastModifiedTime :: Lens.Lens' LabelingJobSummary Prelude.UTCTime
labelingJobSummary_lastModifiedTime = Lens.lens (\LabelingJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@LabelingJobSummary' {} a -> s {lastModifiedTime = a} :: LabelingJobSummary) Prelude.. Prelude._Time

-- | The current status of the labeling job.
labelingJobSummary_labelingJobStatus :: Lens.Lens' LabelingJobSummary LabelingJobStatus
labelingJobSummary_labelingJobStatus = Lens.lens (\LabelingJobSummary' {labelingJobStatus} -> labelingJobStatus) (\s@LabelingJobSummary' {} a -> s {labelingJobStatus = a} :: LabelingJobSummary)

-- | Counts showing the progress of the labeling job.
labelingJobSummary_labelCounters :: Lens.Lens' LabelingJobSummary LabelCounters
labelingJobSummary_labelCounters = Lens.lens (\LabelingJobSummary' {labelCounters} -> labelCounters) (\s@LabelingJobSummary' {} a -> s {labelCounters = a} :: LabelingJobSummary)

-- | The Amazon Resource Name (ARN) of the work team assigned to the job.
labelingJobSummary_workteamArn :: Lens.Lens' LabelingJobSummary Prelude.Text
labelingJobSummary_workteamArn = Lens.lens (\LabelingJobSummary' {workteamArn} -> workteamArn) (\s@LabelingJobSummary' {} a -> s {workteamArn = a} :: LabelingJobSummary)

-- | The Amazon Resource Name (ARN) of a Lambda function. The function is run
-- before each data object is sent to a worker.
labelingJobSummary_preHumanTaskLambdaArn :: Lens.Lens' LabelingJobSummary Prelude.Text
labelingJobSummary_preHumanTaskLambdaArn = Lens.lens (\LabelingJobSummary' {preHumanTaskLambdaArn} -> preHumanTaskLambdaArn) (\s@LabelingJobSummary' {} a -> s {preHumanTaskLambdaArn = a} :: LabelingJobSummary)

instance Prelude.FromJSON LabelingJobSummary where
  parseJSON =
    Prelude.withObject
      "LabelingJobSummary"
      ( \x ->
          LabelingJobSummary'
            Prelude.<$> (x Prelude..:? "AnnotationConsolidationLambdaArn")
            Prelude.<*> (x Prelude..:? "InputConfig")
            Prelude.<*> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "LabelingJobOutput")
            Prelude.<*> (x Prelude..: "LabelingJobName")
            Prelude.<*> (x Prelude..: "LabelingJobArn")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "LastModifiedTime")
            Prelude.<*> (x Prelude..: "LabelingJobStatus")
            Prelude.<*> (x Prelude..: "LabelCounters")
            Prelude.<*> (x Prelude..: "WorkteamArn")
            Prelude.<*> (x Prelude..: "PreHumanTaskLambdaArn")
      )

instance Prelude.Hashable LabelingJobSummary

instance Prelude.NFData LabelingJobSummary
