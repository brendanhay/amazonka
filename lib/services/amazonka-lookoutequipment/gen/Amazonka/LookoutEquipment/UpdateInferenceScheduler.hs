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
-- Module      : Amazonka.LookoutEquipment.UpdateInferenceScheduler
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an inference scheduler.
module Amazonka.LookoutEquipment.UpdateInferenceScheduler
  ( -- * Creating a Request
    UpdateInferenceScheduler (..),
    newUpdateInferenceScheduler,

    -- * Request Lenses
    updateInferenceScheduler_roleArn,
    updateInferenceScheduler_dataDelayOffsetInMinutes,
    updateInferenceScheduler_dataOutputConfiguration,
    updateInferenceScheduler_dataUploadFrequency,
    updateInferenceScheduler_dataInputConfiguration,
    updateInferenceScheduler_inferenceSchedulerName,

    -- * Destructuring the Response
    UpdateInferenceSchedulerResponse (..),
    newUpdateInferenceSchedulerResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateInferenceScheduler' smart constructor.
data UpdateInferenceScheduler = UpdateInferenceScheduler'
  { -- | The Amazon Resource Name (ARN) of a role with permission to access the
    -- data source for the inference scheduler.
    roleArn :: Prelude.Maybe Prelude.Text,
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
    -- | How often data is uploaded to the source S3 bucket for the input data.
    -- The value chosen is the length of time between data uploads. For
    -- instance, if you select 5 minutes, Amazon Lookout for Equipment will
    -- upload the real-time data to the source bucket once every 5 minutes.
    -- This frequency also determines how often Amazon Lookout for Equipment
    -- starts a scheduled inference on your data. In this example, it starts
    -- once every 5 minutes.
    dataUploadFrequency :: Prelude.Maybe DataUploadFrequency,
    -- | Specifies information for the input data for the inference scheduler,
    -- including delimiter, format, and dataset location.
    dataInputConfiguration :: Prelude.Maybe InferenceInputConfiguration,
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
-- 'roleArn', 'updateInferenceScheduler_roleArn' - The Amazon Resource Name (ARN) of a role with permission to access the
-- data source for the inference scheduler.
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
-- 'dataUploadFrequency', 'updateInferenceScheduler_dataUploadFrequency' - How often data is uploaded to the source S3 bucket for the input data.
-- The value chosen is the length of time between data uploads. For
-- instance, if you select 5 minutes, Amazon Lookout for Equipment will
-- upload the real-time data to the source bucket once every 5 minutes.
-- This frequency also determines how often Amazon Lookout for Equipment
-- starts a scheduled inference on your data. In this example, it starts
-- once every 5 minutes.
--
-- 'dataInputConfiguration', 'updateInferenceScheduler_dataInputConfiguration' - Specifies information for the input data for the inference scheduler,
-- including delimiter, format, and dataset location.
--
-- 'inferenceSchedulerName', 'updateInferenceScheduler_inferenceSchedulerName' - The name of the inference scheduler to be updated.
newUpdateInferenceScheduler ::
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  UpdateInferenceScheduler
newUpdateInferenceScheduler pInferenceSchedulerName_ =
  UpdateInferenceScheduler'
    { roleArn =
        Prelude.Nothing,
      dataDelayOffsetInMinutes = Prelude.Nothing,
      dataOutputConfiguration = Prelude.Nothing,
      dataUploadFrequency = Prelude.Nothing,
      dataInputConfiguration = Prelude.Nothing,
      inferenceSchedulerName = pInferenceSchedulerName_
    }

-- | The Amazon Resource Name (ARN) of a role with permission to access the
-- data source for the inference scheduler.
updateInferenceScheduler_roleArn :: Lens.Lens' UpdateInferenceScheduler (Prelude.Maybe Prelude.Text)
updateInferenceScheduler_roleArn = Lens.lens (\UpdateInferenceScheduler' {roleArn} -> roleArn) (\s@UpdateInferenceScheduler' {} a -> s {roleArn = a} :: UpdateInferenceScheduler)

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

-- | How often data is uploaded to the source S3 bucket for the input data.
-- The value chosen is the length of time between data uploads. For
-- instance, if you select 5 minutes, Amazon Lookout for Equipment will
-- upload the real-time data to the source bucket once every 5 minutes.
-- This frequency also determines how often Amazon Lookout for Equipment
-- starts a scheduled inference on your data. In this example, it starts
-- once every 5 minutes.
updateInferenceScheduler_dataUploadFrequency :: Lens.Lens' UpdateInferenceScheduler (Prelude.Maybe DataUploadFrequency)
updateInferenceScheduler_dataUploadFrequency = Lens.lens (\UpdateInferenceScheduler' {dataUploadFrequency} -> dataUploadFrequency) (\s@UpdateInferenceScheduler' {} a -> s {dataUploadFrequency = a} :: UpdateInferenceScheduler)

-- | Specifies information for the input data for the inference scheduler,
-- including delimiter, format, and dataset location.
updateInferenceScheduler_dataInputConfiguration :: Lens.Lens' UpdateInferenceScheduler (Prelude.Maybe InferenceInputConfiguration)
updateInferenceScheduler_dataInputConfiguration = Lens.lens (\UpdateInferenceScheduler' {dataInputConfiguration} -> dataInputConfiguration) (\s@UpdateInferenceScheduler' {} a -> s {dataInputConfiguration = a} :: UpdateInferenceScheduler)

-- | The name of the inference scheduler to be updated.
updateInferenceScheduler_inferenceSchedulerName :: Lens.Lens' UpdateInferenceScheduler Prelude.Text
updateInferenceScheduler_inferenceSchedulerName = Lens.lens (\UpdateInferenceScheduler' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@UpdateInferenceScheduler' {} a -> s {inferenceSchedulerName = a} :: UpdateInferenceScheduler)

instance Core.AWSRequest UpdateInferenceScheduler where
  type
    AWSResponse UpdateInferenceScheduler =
      UpdateInferenceSchedulerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateInferenceSchedulerResponse'

instance Prelude.Hashable UpdateInferenceScheduler where
  hashWithSalt _salt UpdateInferenceScheduler' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` dataDelayOffsetInMinutes
      `Prelude.hashWithSalt` dataOutputConfiguration
      `Prelude.hashWithSalt` dataUploadFrequency
      `Prelude.hashWithSalt` dataInputConfiguration
      `Prelude.hashWithSalt` inferenceSchedulerName

instance Prelude.NFData UpdateInferenceScheduler where
  rnf UpdateInferenceScheduler' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf dataDelayOffsetInMinutes
      `Prelude.seq` Prelude.rnf dataOutputConfiguration
      `Prelude.seq` Prelude.rnf dataUploadFrequency
      `Prelude.seq` Prelude.rnf dataInputConfiguration
      `Prelude.seq` Prelude.rnf inferenceSchedulerName

instance Data.ToHeaders UpdateInferenceScheduler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.UpdateInferenceScheduler" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateInferenceScheduler where
  toJSON UpdateInferenceScheduler' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("DataDelayOffsetInMinutes" Data..=)
              Prelude.<$> dataDelayOffsetInMinutes,
            ("DataOutputConfiguration" Data..=)
              Prelude.<$> dataOutputConfiguration,
            ("DataUploadFrequency" Data..=)
              Prelude.<$> dataUploadFrequency,
            ("DataInputConfiguration" Data..=)
              Prelude.<$> dataInputConfiguration,
            Prelude.Just
              ( "InferenceSchedulerName"
                  Data..= inferenceSchedulerName
              )
          ]
      )

instance Data.ToPath UpdateInferenceScheduler where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateInferenceScheduler where
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
  where
  rnf _ = ()
