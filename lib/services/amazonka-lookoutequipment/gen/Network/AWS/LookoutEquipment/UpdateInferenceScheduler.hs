{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LookoutEquipment.UpdateInferenceScheduler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an inference scheduler.
module Network.AWS.LookoutEquipment.UpdateInferenceScheduler
  ( -- * Creating a Request
    UpdateInferenceScheduler (..),
    newUpdateInferenceScheduler,

    -- * Request Lenses
    updateInferenceScheduler_dataUploadFrequency,
    updateInferenceScheduler_dataDelayOffsetInMinutes,
    updateInferenceScheduler_dataOutputConfiguration,
    updateInferenceScheduler_dataInputConfiguration,
    updateInferenceScheduler_roleArn,
    updateInferenceScheduler_inferenceSchedulerName,

    -- * Destructuring the Response
    UpdateInferenceSchedulerResponse (..),
    newUpdateInferenceSchedulerResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutEquipment.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateInferenceScheduler' smart constructor.
data UpdateInferenceScheduler = UpdateInferenceScheduler'
  { -- | How often data is uploaded to the source S3 bucket for the input data.
    -- The value chosen is the length of time between data uploads. For
    -- instance, if you select 5 minutes, Amazon Lookout for Equipment will
    -- upload the real-time data to the source bucket once every 5 minutes.
    -- This frequency also determines how often Amazon Lookout for Equipment
    -- starts a scheduled inference on your data. In this example, it starts
    -- once every 5 minutes.
    dataUploadFrequency :: Prelude.Maybe DataUploadFrequency,
    -- | A period of time (in minutes) by which inference on the data is delayed
    -- after the data starts. For instance, if you select an offset delay time
    -- of five minutes, inference will not begin on the data until the first
    -- data measurement after the five minute mark. For example, if five
    -- minutes is selected, the inference scheduler will wake up at the
    -- configured frequency with the additional five minute delay time to check
    -- the customer S3 bucket. The customer can upload data at the same
    -- frequency and they don\'t need to stop and restart the scheduler when
    -- uploading new data.
    dataDelayOffsetInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Specifies information for the output results from the inference
    -- scheduler, including the output S3 location.
    dataOutputConfiguration :: Prelude.Maybe InferenceOutputConfiguration,
    -- | Specifies information for the input data for the inference scheduler,
    -- including delimiter, format, and dataset location.
    dataInputConfiguration :: Prelude.Maybe InferenceInputConfiguration,
    -- | The Amazon Resource Name (ARN) of a role with permission to access the
    -- data source for the inference scheduler.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the inference scheduler to be updated.
    inferenceSchedulerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInferenceScheduler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataUploadFrequency', 'updateInferenceScheduler_dataUploadFrequency' - How often data is uploaded to the source S3 bucket for the input data.
-- The value chosen is the length of time between data uploads. For
-- instance, if you select 5 minutes, Amazon Lookout for Equipment will
-- upload the real-time data to the source bucket once every 5 minutes.
-- This frequency also determines how often Amazon Lookout for Equipment
-- starts a scheduled inference on your data. In this example, it starts
-- once every 5 minutes.
--
-- 'dataDelayOffsetInMinutes', 'updateInferenceScheduler_dataDelayOffsetInMinutes' - A period of time (in minutes) by which inference on the data is delayed
-- after the data starts. For instance, if you select an offset delay time
-- of five minutes, inference will not begin on the data until the first
-- data measurement after the five minute mark. For example, if five
-- minutes is selected, the inference scheduler will wake up at the
-- configured frequency with the additional five minute delay time to check
-- the customer S3 bucket. The customer can upload data at the same
-- frequency and they don\'t need to stop and restart the scheduler when
-- uploading new data.
--
-- 'dataOutputConfiguration', 'updateInferenceScheduler_dataOutputConfiguration' - Specifies information for the output results from the inference
-- scheduler, including the output S3 location.
--
-- 'dataInputConfiguration', 'updateInferenceScheduler_dataInputConfiguration' - Specifies information for the input data for the inference scheduler,
-- including delimiter, format, and dataset location.
--
-- 'roleArn', 'updateInferenceScheduler_roleArn' - The Amazon Resource Name (ARN) of a role with permission to access the
-- data source for the inference scheduler.
--
-- 'inferenceSchedulerName', 'updateInferenceScheduler_inferenceSchedulerName' - The name of the inference scheduler to be updated.
newUpdateInferenceScheduler ::
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  UpdateInferenceScheduler
newUpdateInferenceScheduler pInferenceSchedulerName_ =
  UpdateInferenceScheduler'
    { dataUploadFrequency =
        Prelude.Nothing,
      dataDelayOffsetInMinutes = Prelude.Nothing,
      dataOutputConfiguration = Prelude.Nothing,
      dataInputConfiguration = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      inferenceSchedulerName = pInferenceSchedulerName_
    }

-- | How often data is uploaded to the source S3 bucket for the input data.
-- The value chosen is the length of time between data uploads. For
-- instance, if you select 5 minutes, Amazon Lookout for Equipment will
-- upload the real-time data to the source bucket once every 5 minutes.
-- This frequency also determines how often Amazon Lookout for Equipment
-- starts a scheduled inference on your data. In this example, it starts
-- once every 5 minutes.
updateInferenceScheduler_dataUploadFrequency :: Lens.Lens' UpdateInferenceScheduler (Prelude.Maybe DataUploadFrequency)
updateInferenceScheduler_dataUploadFrequency = Lens.lens (\UpdateInferenceScheduler' {dataUploadFrequency} -> dataUploadFrequency) (\s@UpdateInferenceScheduler' {} a -> s {dataUploadFrequency = a} :: UpdateInferenceScheduler)

-- | A period of time (in minutes) by which inference on the data is delayed
-- after the data starts. For instance, if you select an offset delay time
-- of five minutes, inference will not begin on the data until the first
-- data measurement after the five minute mark. For example, if five
-- minutes is selected, the inference scheduler will wake up at the
-- configured frequency with the additional five minute delay time to check
-- the customer S3 bucket. The customer can upload data at the same
-- frequency and they don\'t need to stop and restart the scheduler when
-- uploading new data.
updateInferenceScheduler_dataDelayOffsetInMinutes :: Lens.Lens' UpdateInferenceScheduler (Prelude.Maybe Prelude.Natural)
updateInferenceScheduler_dataDelayOffsetInMinutes = Lens.lens (\UpdateInferenceScheduler' {dataDelayOffsetInMinutes} -> dataDelayOffsetInMinutes) (\s@UpdateInferenceScheduler' {} a -> s {dataDelayOffsetInMinutes = a} :: UpdateInferenceScheduler)

-- | Specifies information for the output results from the inference
-- scheduler, including the output S3 location.
updateInferenceScheduler_dataOutputConfiguration :: Lens.Lens' UpdateInferenceScheduler (Prelude.Maybe InferenceOutputConfiguration)
updateInferenceScheduler_dataOutputConfiguration = Lens.lens (\UpdateInferenceScheduler' {dataOutputConfiguration} -> dataOutputConfiguration) (\s@UpdateInferenceScheduler' {} a -> s {dataOutputConfiguration = a} :: UpdateInferenceScheduler)

-- | Specifies information for the input data for the inference scheduler,
-- including delimiter, format, and dataset location.
updateInferenceScheduler_dataInputConfiguration :: Lens.Lens' UpdateInferenceScheduler (Prelude.Maybe InferenceInputConfiguration)
updateInferenceScheduler_dataInputConfiguration = Lens.lens (\UpdateInferenceScheduler' {dataInputConfiguration} -> dataInputConfiguration) (\s@UpdateInferenceScheduler' {} a -> s {dataInputConfiguration = a} :: UpdateInferenceScheduler)

-- | The Amazon Resource Name (ARN) of a role with permission to access the
-- data source for the inference scheduler.
updateInferenceScheduler_roleArn :: Lens.Lens' UpdateInferenceScheduler (Prelude.Maybe Prelude.Text)
updateInferenceScheduler_roleArn = Lens.lens (\UpdateInferenceScheduler' {roleArn} -> roleArn) (\s@UpdateInferenceScheduler' {} a -> s {roleArn = a} :: UpdateInferenceScheduler)

-- | The name of the inference scheduler to be updated.
updateInferenceScheduler_inferenceSchedulerName :: Lens.Lens' UpdateInferenceScheduler Prelude.Text
updateInferenceScheduler_inferenceSchedulerName = Lens.lens (\UpdateInferenceScheduler' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@UpdateInferenceScheduler' {} a -> s {inferenceSchedulerName = a} :: UpdateInferenceScheduler)

instance Core.AWSRequest UpdateInferenceScheduler where
  type
    AWSResponse UpdateInferenceScheduler =
      UpdateInferenceSchedulerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateInferenceSchedulerResponse'

instance Prelude.Hashable UpdateInferenceScheduler

instance Prelude.NFData UpdateInferenceScheduler

instance Core.ToHeaders UpdateInferenceScheduler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLookoutEquipmentFrontendService.UpdateInferenceScheduler" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateInferenceScheduler where
  toJSON UpdateInferenceScheduler' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataUploadFrequency" Core..=)
              Prelude.<$> dataUploadFrequency,
            ("DataDelayOffsetInMinutes" Core..=)
              Prelude.<$> dataDelayOffsetInMinutes,
            ("DataOutputConfiguration" Core..=)
              Prelude.<$> dataOutputConfiguration,
            ("DataInputConfiguration" Core..=)
              Prelude.<$> dataInputConfiguration,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            Prelude.Just
              ( "InferenceSchedulerName"
                  Core..= inferenceSchedulerName
              )
          ]
      )

instance Core.ToPath UpdateInferenceScheduler where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateInferenceScheduler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInferenceSchedulerResponse' smart constructor.
data UpdateInferenceSchedulerResponse = UpdateInferenceSchedulerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInferenceSchedulerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateInferenceSchedulerResponse ::
  UpdateInferenceSchedulerResponse
newUpdateInferenceSchedulerResponse =
  UpdateInferenceSchedulerResponse'

instance
  Prelude.NFData
    UpdateInferenceSchedulerResponse
