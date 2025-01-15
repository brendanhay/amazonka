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
-- Module      : Amazonka.SageMaker.Types.LabelingJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.LabelCounters
import Amazonka.SageMaker.Types.LabelingJobInputConfig
import Amazonka.SageMaker.Types.LabelingJobOutput
import Amazonka.SageMaker.Types.LabelingJobStatus

-- | Provides summary information about a labeling job.
--
-- /See:/ 'newLabelingJobSummary' smart constructor.
data LabelingJobSummary = LabelingJobSummary'
  { -- | The Amazon Resource Name (ARN) of the Lambda function used to
    -- consolidate the annotations from individual workers into a label for a
    -- data object. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation>.
    annotationConsolidationLambdaArn :: Prelude.Maybe Prelude.Text,
    -- | If the @LabelingJobStatus@ field is @Failed@, this field contains a
    -- description of the error.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Input configuration for the labeling job.
    inputConfig :: Prelude.Maybe LabelingJobInputConfig,
    -- | The location of the output produced by the labeling job.
    labelingJobOutput :: Prelude.Maybe LabelingJobOutput,
    -- | The name of the labeling job.
    labelingJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) assigned to the labeling job when it was
    -- created.
    labelingJobArn :: Prelude.Text,
    -- | The date and time that the job was created (timestamp).
    creationTime :: Data.POSIX,
    -- | The date and time that the job was last modified (timestamp).
    lastModifiedTime :: Data.POSIX,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'failureReason', 'labelingJobSummary_failureReason' - If the @LabelingJobStatus@ field is @Failed@, this field contains a
-- description of the error.
--
-- 'inputConfig', 'labelingJobSummary_inputConfig' - Input configuration for the labeling job.
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
        failureReason = Prelude.Nothing,
        inputConfig = Prelude.Nothing,
        labelingJobOutput = Prelude.Nothing,
        labelingJobName = pLabelingJobName_,
        labelingJobArn = pLabelingJobArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
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

-- | If the @LabelingJobStatus@ field is @Failed@, this field contains a
-- description of the error.
labelingJobSummary_failureReason :: Lens.Lens' LabelingJobSummary (Prelude.Maybe Prelude.Text)
labelingJobSummary_failureReason = Lens.lens (\LabelingJobSummary' {failureReason} -> failureReason) (\s@LabelingJobSummary' {} a -> s {failureReason = a} :: LabelingJobSummary)

-- | Input configuration for the labeling job.
labelingJobSummary_inputConfig :: Lens.Lens' LabelingJobSummary (Prelude.Maybe LabelingJobInputConfig)
labelingJobSummary_inputConfig = Lens.lens (\LabelingJobSummary' {inputConfig} -> inputConfig) (\s@LabelingJobSummary' {} a -> s {inputConfig = a} :: LabelingJobSummary)

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
labelingJobSummary_creationTime = Lens.lens (\LabelingJobSummary' {creationTime} -> creationTime) (\s@LabelingJobSummary' {} a -> s {creationTime = a} :: LabelingJobSummary) Prelude.. Data._Time

-- | The date and time that the job was last modified (timestamp).
labelingJobSummary_lastModifiedTime :: Lens.Lens' LabelingJobSummary Prelude.UTCTime
labelingJobSummary_lastModifiedTime = Lens.lens (\LabelingJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@LabelingJobSummary' {} a -> s {lastModifiedTime = a} :: LabelingJobSummary) Prelude.. Data._Time

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

instance Data.FromJSON LabelingJobSummary where
  parseJSON =
    Data.withObject
      "LabelingJobSummary"
      ( \x ->
          LabelingJobSummary'
            Prelude.<$> (x Data..:? "AnnotationConsolidationLambdaArn")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "InputConfig")
            Prelude.<*> (x Data..:? "LabelingJobOutput")
            Prelude.<*> (x Data..: "LabelingJobName")
            Prelude.<*> (x Data..: "LabelingJobArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "LabelingJobStatus")
            Prelude.<*> (x Data..: "LabelCounters")
            Prelude.<*> (x Data..: "WorkteamArn")
            Prelude.<*> (x Data..: "PreHumanTaskLambdaArn")
      )

instance Prelude.Hashable LabelingJobSummary where
  hashWithSalt _salt LabelingJobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` annotationConsolidationLambdaArn
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` inputConfig
      `Prelude.hashWithSalt` labelingJobOutput
      `Prelude.hashWithSalt` labelingJobName
      `Prelude.hashWithSalt` labelingJobArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` labelingJobStatus
      `Prelude.hashWithSalt` labelCounters
      `Prelude.hashWithSalt` workteamArn
      `Prelude.hashWithSalt` preHumanTaskLambdaArn

instance Prelude.NFData LabelingJobSummary where
  rnf LabelingJobSummary' {..} =
    Prelude.rnf annotationConsolidationLambdaArn `Prelude.seq`
      Prelude.rnf failureReason `Prelude.seq`
        Prelude.rnf inputConfig `Prelude.seq`
          Prelude.rnf labelingJobOutput `Prelude.seq`
            Prelude.rnf labelingJobName `Prelude.seq`
              Prelude.rnf labelingJobArn `Prelude.seq`
                Prelude.rnf creationTime `Prelude.seq`
                  Prelude.rnf lastModifiedTime `Prelude.seq`
                    Prelude.rnf labelingJobStatus `Prelude.seq`
                      Prelude.rnf labelCounters `Prelude.seq`
                        Prelude.rnf workteamArn `Prelude.seq`
                          Prelude.rnf preHumanTaskLambdaArn
