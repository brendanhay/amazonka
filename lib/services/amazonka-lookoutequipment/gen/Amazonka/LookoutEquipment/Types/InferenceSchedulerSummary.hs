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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceSchedulerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceSchedulerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutEquipment.Types.DataUploadFrequency
import Amazonka.LookoutEquipment.Types.InferenceSchedulerStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the specific inference scheduler, including
-- data delay offset, model name and ARN, status, and so on.
--
-- /See:/ 'newInferenceSchedulerSummary' smart constructor.
data InferenceSchedulerSummary = InferenceSchedulerSummary'
  { -- | Indicates the status of the inference scheduler.
    status :: Prelude.Maybe InferenceSchedulerStatus,
    -- | How often data is uploaded to the source S3 bucket for the input data.
    -- This value is the length of time between data uploads. For instance, if
    -- you select 5 minutes, Amazon Lookout for Equipment will upload the
    -- real-time data to the source bucket once every 5 minutes. This frequency
    -- also determines how often Amazon Lookout for Equipment starts a
    -- scheduled inference on your data. In this example, it starts once every
    -- 5 minutes.
    dataUploadFrequency :: Prelude.Maybe DataUploadFrequency,
    -- | A period of time (in minutes) by which inference on the data is delayed
    -- after the data starts. For instance, if an offset delay time of five
    -- minutes was selected, inference will not begin on the data until the
    -- first data measurement after the five minute mark. For example, if five
    -- minutes is selected, the inference scheduler will wake up at the
    -- configured frequency with the additional five minute delay time to check
    -- the customer S3 bucket. The customer can upload data at the same
    -- frequency and they don\'t need to stop and restart the scheduler when
    -- uploading new data.
    dataDelayOffsetInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the ML model used by the inference
    -- scheduler.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the ML model used for the inference scheduler.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the inference scheduler.
    inferenceSchedulerArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the inference scheduler.
    inferenceSchedulerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceSchedulerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'inferenceSchedulerSummary_status' - Indicates the status of the inference scheduler.
--
-- 'dataUploadFrequency', 'inferenceSchedulerSummary_dataUploadFrequency' - How often data is uploaded to the source S3 bucket for the input data.
-- This value is the length of time between data uploads. For instance, if
-- you select 5 minutes, Amazon Lookout for Equipment will upload the
-- real-time data to the source bucket once every 5 minutes. This frequency
-- also determines how often Amazon Lookout for Equipment starts a
-- scheduled inference on your data. In this example, it starts once every
-- 5 minutes.
--
-- 'dataDelayOffsetInMinutes', 'inferenceSchedulerSummary_dataDelayOffsetInMinutes' - A period of time (in minutes) by which inference on the data is delayed
-- after the data starts. For instance, if an offset delay time of five
-- minutes was selected, inference will not begin on the data until the
-- first data measurement after the five minute mark. For example, if five
-- minutes is selected, the inference scheduler will wake up at the
-- configured frequency with the additional five minute delay time to check
-- the customer S3 bucket. The customer can upload data at the same
-- frequency and they don\'t need to stop and restart the scheduler when
-- uploading new data.
--
-- 'modelArn', 'inferenceSchedulerSummary_modelArn' - The Amazon Resource Name (ARN) of the ML model used by the inference
-- scheduler.
--
-- 'modelName', 'inferenceSchedulerSummary_modelName' - The name of the ML model used for the inference scheduler.
--
-- 'inferenceSchedulerArn', 'inferenceSchedulerSummary_inferenceSchedulerArn' - The Amazon Resource Name (ARN) of the inference scheduler.
--
-- 'inferenceSchedulerName', 'inferenceSchedulerSummary_inferenceSchedulerName' - The name of the inference scheduler.
newInferenceSchedulerSummary ::
  InferenceSchedulerSummary
newInferenceSchedulerSummary =
  InferenceSchedulerSummary'
    { status =
        Prelude.Nothing,
      dataUploadFrequency = Prelude.Nothing,
      dataDelayOffsetInMinutes = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      modelName = Prelude.Nothing,
      inferenceSchedulerArn = Prelude.Nothing,
      inferenceSchedulerName = Prelude.Nothing
    }

-- | Indicates the status of the inference scheduler.
inferenceSchedulerSummary_status :: Lens.Lens' InferenceSchedulerSummary (Prelude.Maybe InferenceSchedulerStatus)
inferenceSchedulerSummary_status = Lens.lens (\InferenceSchedulerSummary' {status} -> status) (\s@InferenceSchedulerSummary' {} a -> s {status = a} :: InferenceSchedulerSummary)

-- | How often data is uploaded to the source S3 bucket for the input data.
-- This value is the length of time between data uploads. For instance, if
-- you select 5 minutes, Amazon Lookout for Equipment will upload the
-- real-time data to the source bucket once every 5 minutes. This frequency
-- also determines how often Amazon Lookout for Equipment starts a
-- scheduled inference on your data. In this example, it starts once every
-- 5 minutes.
inferenceSchedulerSummary_dataUploadFrequency :: Lens.Lens' InferenceSchedulerSummary (Prelude.Maybe DataUploadFrequency)
inferenceSchedulerSummary_dataUploadFrequency = Lens.lens (\InferenceSchedulerSummary' {dataUploadFrequency} -> dataUploadFrequency) (\s@InferenceSchedulerSummary' {} a -> s {dataUploadFrequency = a} :: InferenceSchedulerSummary)

-- | A period of time (in minutes) by which inference on the data is delayed
-- after the data starts. For instance, if an offset delay time of five
-- minutes was selected, inference will not begin on the data until the
-- first data measurement after the five minute mark. For example, if five
-- minutes is selected, the inference scheduler will wake up at the
-- configured frequency with the additional five minute delay time to check
-- the customer S3 bucket. The customer can upload data at the same
-- frequency and they don\'t need to stop and restart the scheduler when
-- uploading new data.
inferenceSchedulerSummary_dataDelayOffsetInMinutes :: Lens.Lens' InferenceSchedulerSummary (Prelude.Maybe Prelude.Natural)
inferenceSchedulerSummary_dataDelayOffsetInMinutes = Lens.lens (\InferenceSchedulerSummary' {dataDelayOffsetInMinutes} -> dataDelayOffsetInMinutes) (\s@InferenceSchedulerSummary' {} a -> s {dataDelayOffsetInMinutes = a} :: InferenceSchedulerSummary)

-- | The Amazon Resource Name (ARN) of the ML model used by the inference
-- scheduler.
inferenceSchedulerSummary_modelArn :: Lens.Lens' InferenceSchedulerSummary (Prelude.Maybe Prelude.Text)
inferenceSchedulerSummary_modelArn = Lens.lens (\InferenceSchedulerSummary' {modelArn} -> modelArn) (\s@InferenceSchedulerSummary' {} a -> s {modelArn = a} :: InferenceSchedulerSummary)

-- | The name of the ML model used for the inference scheduler.
inferenceSchedulerSummary_modelName :: Lens.Lens' InferenceSchedulerSummary (Prelude.Maybe Prelude.Text)
inferenceSchedulerSummary_modelName = Lens.lens (\InferenceSchedulerSummary' {modelName} -> modelName) (\s@InferenceSchedulerSummary' {} a -> s {modelName = a} :: InferenceSchedulerSummary)

-- | The Amazon Resource Name (ARN) of the inference scheduler.
inferenceSchedulerSummary_inferenceSchedulerArn :: Lens.Lens' InferenceSchedulerSummary (Prelude.Maybe Prelude.Text)
inferenceSchedulerSummary_inferenceSchedulerArn = Lens.lens (\InferenceSchedulerSummary' {inferenceSchedulerArn} -> inferenceSchedulerArn) (\s@InferenceSchedulerSummary' {} a -> s {inferenceSchedulerArn = a} :: InferenceSchedulerSummary)

-- | The name of the inference scheduler.
inferenceSchedulerSummary_inferenceSchedulerName :: Lens.Lens' InferenceSchedulerSummary (Prelude.Maybe Prelude.Text)
inferenceSchedulerSummary_inferenceSchedulerName = Lens.lens (\InferenceSchedulerSummary' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@InferenceSchedulerSummary' {} a -> s {inferenceSchedulerName = a} :: InferenceSchedulerSummary)

instance Core.FromJSON InferenceSchedulerSummary where
  parseJSON =
    Core.withObject
      "InferenceSchedulerSummary"
      ( \x ->
          InferenceSchedulerSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "DataUploadFrequency")
            Prelude.<*> (x Core..:? "DataDelayOffsetInMinutes")
            Prelude.<*> (x Core..:? "ModelArn")
            Prelude.<*> (x Core..:? "ModelName")
            Prelude.<*> (x Core..:? "InferenceSchedulerArn")
            Prelude.<*> (x Core..:? "InferenceSchedulerName")
      )

instance Prelude.Hashable InferenceSchedulerSummary where
  hashWithSalt _salt InferenceSchedulerSummary' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` dataUploadFrequency
      `Prelude.hashWithSalt` dataDelayOffsetInMinutes
      `Prelude.hashWithSalt` modelArn
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` inferenceSchedulerArn
      `Prelude.hashWithSalt` inferenceSchedulerName

instance Prelude.NFData InferenceSchedulerSummary where
  rnf InferenceSchedulerSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf dataUploadFrequency
      `Prelude.seq` Prelude.rnf dataDelayOffsetInMinutes
      `Prelude.seq` Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf inferenceSchedulerArn
      `Prelude.seq` Prelude.rnf inferenceSchedulerName
