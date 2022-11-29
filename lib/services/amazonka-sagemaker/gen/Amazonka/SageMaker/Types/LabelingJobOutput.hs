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
-- Module      : Amazonka.SageMaker.Types.LabelingJobOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the location of the output produced by the labeling job.
--
-- /See:/ 'newLabelingJobOutput' smart constructor.
data LabelingJobOutput = LabelingJobOutput'
  { -- | The Amazon Resource Name (ARN) for the most recent SageMaker model
    -- trained as part of automated data labeling.
    finalActiveLearningModelArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket location of the manifest file for labeled data.
    outputDatasetS3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalActiveLearningModelArn', 'labelingJobOutput_finalActiveLearningModelArn' - The Amazon Resource Name (ARN) for the most recent SageMaker model
-- trained as part of automated data labeling.
--
-- 'outputDatasetS3Uri', 'labelingJobOutput_outputDatasetS3Uri' - The Amazon S3 bucket location of the manifest file for labeled data.
newLabelingJobOutput ::
  -- | 'outputDatasetS3Uri'
  Prelude.Text ->
  LabelingJobOutput
newLabelingJobOutput pOutputDatasetS3Uri_ =
  LabelingJobOutput'
    { finalActiveLearningModelArn =
        Prelude.Nothing,
      outputDatasetS3Uri = pOutputDatasetS3Uri_
    }

-- | The Amazon Resource Name (ARN) for the most recent SageMaker model
-- trained as part of automated data labeling.
labelingJobOutput_finalActiveLearningModelArn :: Lens.Lens' LabelingJobOutput (Prelude.Maybe Prelude.Text)
labelingJobOutput_finalActiveLearningModelArn = Lens.lens (\LabelingJobOutput' {finalActiveLearningModelArn} -> finalActiveLearningModelArn) (\s@LabelingJobOutput' {} a -> s {finalActiveLearningModelArn = a} :: LabelingJobOutput)

-- | The Amazon S3 bucket location of the manifest file for labeled data.
labelingJobOutput_outputDatasetS3Uri :: Lens.Lens' LabelingJobOutput Prelude.Text
labelingJobOutput_outputDatasetS3Uri = Lens.lens (\LabelingJobOutput' {outputDatasetS3Uri} -> outputDatasetS3Uri) (\s@LabelingJobOutput' {} a -> s {outputDatasetS3Uri = a} :: LabelingJobOutput)

instance Core.FromJSON LabelingJobOutput where
  parseJSON =
    Core.withObject
      "LabelingJobOutput"
      ( \x ->
          LabelingJobOutput'
            Prelude.<$> (x Core..:? "FinalActiveLearningModelArn")
            Prelude.<*> (x Core..: "OutputDatasetS3Uri")
      )

instance Prelude.Hashable LabelingJobOutput where
  hashWithSalt _salt LabelingJobOutput' {..} =
    _salt
      `Prelude.hashWithSalt` finalActiveLearningModelArn
      `Prelude.hashWithSalt` outputDatasetS3Uri

instance Prelude.NFData LabelingJobOutput where
  rnf LabelingJobOutput' {..} =
    Prelude.rnf finalActiveLearningModelArn
      `Prelude.seq` Prelude.rnf outputDatasetS3Uri
