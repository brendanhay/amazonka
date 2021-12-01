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
-- Module      : Amazonka.Forecast.DescribeForecast
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a forecast created using the CreateForecast operation.
--
-- In addition to listing the properties provided in the @CreateForecast@
-- request, this operation lists the following properties:
--
-- -   @DatasetGroupArn@ - The dataset group that provided the training
--     data.
--
-- -   @CreationTime@
--
-- -   @LastModificationTime@
--
-- -   @Status@
--
-- -   @Message@ - If an error occurred, information about the error.
module Amazonka.Forecast.DescribeForecast
  ( -- * Creating a Request
    DescribeForecast (..),
    newDescribeForecast,

    -- * Request Lenses
    describeForecast_forecastArn,

    -- * Destructuring the Response
    DescribeForecastResponse (..),
    newDescribeForecastResponse,

    -- * Response Lenses
    describeForecastResponse_creationTime,
    describeForecastResponse_status,
    describeForecastResponse_predictorArn,
    describeForecastResponse_forecastArn,
    describeForecastResponse_estimatedTimeRemainingInMinutes,
    describeForecastResponse_forecastName,
    describeForecastResponse_forecastTypes,
    describeForecastResponse_datasetGroupArn,
    describeForecastResponse_message,
    describeForecastResponse_lastModificationTime,
    describeForecastResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Forecast.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeForecast' smart constructor.
data DescribeForecast = DescribeForecast'
  { -- | The Amazon Resource Name (ARN) of the forecast.
    forecastArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastArn', 'describeForecast_forecastArn' - The Amazon Resource Name (ARN) of the forecast.
newDescribeForecast ::
  -- | 'forecastArn'
  Prelude.Text ->
  DescribeForecast
newDescribeForecast pForecastArn_ =
  DescribeForecast' {forecastArn = pForecastArn_}

-- | The Amazon Resource Name (ARN) of the forecast.
describeForecast_forecastArn :: Lens.Lens' DescribeForecast Prelude.Text
describeForecast_forecastArn = Lens.lens (\DescribeForecast' {forecastArn} -> forecastArn) (\s@DescribeForecast' {} a -> s {forecastArn = a} :: DescribeForecast)

instance Core.AWSRequest DescribeForecast where
  type
    AWSResponse DescribeForecast =
      DescribeForecastResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeForecastResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "PredictorArn")
            Prelude.<*> (x Core..?> "ForecastArn")
            Prelude.<*> (x Core..?> "EstimatedTimeRemainingInMinutes")
            Prelude.<*> (x Core..?> "ForecastName")
            Prelude.<*> (x Core..?> "ForecastTypes")
            Prelude.<*> (x Core..?> "DatasetGroupArn")
            Prelude.<*> (x Core..?> "Message")
            Prelude.<*> (x Core..?> "LastModificationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeForecast where
  hashWithSalt salt' DescribeForecast' {..} =
    salt' `Prelude.hashWithSalt` forecastArn

instance Prelude.NFData DescribeForecast where
  rnf DescribeForecast' {..} = Prelude.rnf forecastArn

instance Core.ToHeaders DescribeForecast where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.DescribeForecast" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeForecast where
  toJSON DescribeForecast' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ForecastArn" Core..= forecastArn)]
      )

instance Core.ToPath DescribeForecast where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeForecast where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeForecastResponse' smart constructor.
data DescribeForecastResponse = DescribeForecastResponse'
  { -- | When the forecast creation task was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the forecast. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- The @Status@ of the forecast must be @ACTIVE@ before you can query or
    -- export the forecast.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the predictor used to generate the forecast.
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | The forecast ARN as specified in the request.
    forecastArn :: Prelude.Maybe Prelude.Text,
    -- | The estimated time remaining in minutes for the forecast job to
    -- complete.
    estimatedTimeRemainingInMinutes :: Prelude.Maybe Prelude.Integer,
    -- | The name of the forecast.
    forecastName :: Prelude.Maybe Prelude.Text,
    -- | The quantiles at which probabilistic forecasts were generated.
    forecastTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ARN of the dataset group that provided the data used to train the
    -- predictor.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The last time the resource was modified. The timestamp depends on the
    -- status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @CREATE_STOPPING@ - The current timestamp.
    --
    -- -   @CREATE_STOPPED@ - When the job stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeForecastResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeForecastResponse_creationTime' - When the forecast creation task was created.
--
-- 'status', 'describeForecastResponse_status' - The status of the forecast. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the forecast must be @ACTIVE@ before you can query or
-- export the forecast.
--
-- 'predictorArn', 'describeForecastResponse_predictorArn' - The ARN of the predictor used to generate the forecast.
--
-- 'forecastArn', 'describeForecastResponse_forecastArn' - The forecast ARN as specified in the request.
--
-- 'estimatedTimeRemainingInMinutes', 'describeForecastResponse_estimatedTimeRemainingInMinutes' - The estimated time remaining in minutes for the forecast job to
-- complete.
--
-- 'forecastName', 'describeForecastResponse_forecastName' - The name of the forecast.
--
-- 'forecastTypes', 'describeForecastResponse_forecastTypes' - The quantiles at which probabilistic forecasts were generated.
--
-- 'datasetGroupArn', 'describeForecastResponse_datasetGroupArn' - The ARN of the dataset group that provided the data used to train the
-- predictor.
--
-- 'message', 'describeForecastResponse_message' - If an error occurred, an informational message about the error.
--
-- 'lastModificationTime', 'describeForecastResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
--
-- 'httpStatus', 'describeForecastResponse_httpStatus' - The response's http status code.
newDescribeForecastResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeForecastResponse
newDescribeForecastResponse pHttpStatus_ =
  DescribeForecastResponse'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      predictorArn = Prelude.Nothing,
      forecastArn = Prelude.Nothing,
      estimatedTimeRemainingInMinutes = Prelude.Nothing,
      forecastName = Prelude.Nothing,
      forecastTypes = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      message = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the forecast creation task was created.
describeForecastResponse_creationTime :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.UTCTime)
describeForecastResponse_creationTime = Lens.lens (\DescribeForecastResponse' {creationTime} -> creationTime) (\s@DescribeForecastResponse' {} a -> s {creationTime = a} :: DescribeForecastResponse) Prelude.. Lens.mapping Core._Time

-- | The status of the forecast. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the forecast must be @ACTIVE@ before you can query or
-- export the forecast.
describeForecastResponse_status :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.Text)
describeForecastResponse_status = Lens.lens (\DescribeForecastResponse' {status} -> status) (\s@DescribeForecastResponse' {} a -> s {status = a} :: DescribeForecastResponse)

-- | The ARN of the predictor used to generate the forecast.
describeForecastResponse_predictorArn :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.Text)
describeForecastResponse_predictorArn = Lens.lens (\DescribeForecastResponse' {predictorArn} -> predictorArn) (\s@DescribeForecastResponse' {} a -> s {predictorArn = a} :: DescribeForecastResponse)

-- | The forecast ARN as specified in the request.
describeForecastResponse_forecastArn :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.Text)
describeForecastResponse_forecastArn = Lens.lens (\DescribeForecastResponse' {forecastArn} -> forecastArn) (\s@DescribeForecastResponse' {} a -> s {forecastArn = a} :: DescribeForecastResponse)

-- | The estimated time remaining in minutes for the forecast job to
-- complete.
describeForecastResponse_estimatedTimeRemainingInMinutes :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.Integer)
describeForecastResponse_estimatedTimeRemainingInMinutes = Lens.lens (\DescribeForecastResponse' {estimatedTimeRemainingInMinutes} -> estimatedTimeRemainingInMinutes) (\s@DescribeForecastResponse' {} a -> s {estimatedTimeRemainingInMinutes = a} :: DescribeForecastResponse)

-- | The name of the forecast.
describeForecastResponse_forecastName :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.Text)
describeForecastResponse_forecastName = Lens.lens (\DescribeForecastResponse' {forecastName} -> forecastName) (\s@DescribeForecastResponse' {} a -> s {forecastName = a} :: DescribeForecastResponse)

-- | The quantiles at which probabilistic forecasts were generated.
describeForecastResponse_forecastTypes :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeForecastResponse_forecastTypes = Lens.lens (\DescribeForecastResponse' {forecastTypes} -> forecastTypes) (\s@DescribeForecastResponse' {} a -> s {forecastTypes = a} :: DescribeForecastResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the dataset group that provided the data used to train the
-- predictor.
describeForecastResponse_datasetGroupArn :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.Text)
describeForecastResponse_datasetGroupArn = Lens.lens (\DescribeForecastResponse' {datasetGroupArn} -> datasetGroupArn) (\s@DescribeForecastResponse' {} a -> s {datasetGroupArn = a} :: DescribeForecastResponse)

-- | If an error occurred, an informational message about the error.
describeForecastResponse_message :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.Text)
describeForecastResponse_message = Lens.lens (\DescribeForecastResponse' {message} -> message) (\s@DescribeForecastResponse' {} a -> s {message = a} :: DescribeForecastResponse)

-- | The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
describeForecastResponse_lastModificationTime :: Lens.Lens' DescribeForecastResponse (Prelude.Maybe Prelude.UTCTime)
describeForecastResponse_lastModificationTime = Lens.lens (\DescribeForecastResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeForecastResponse' {} a -> s {lastModificationTime = a} :: DescribeForecastResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeForecastResponse_httpStatus :: Lens.Lens' DescribeForecastResponse Prelude.Int
describeForecastResponse_httpStatus = Lens.lens (\DescribeForecastResponse' {httpStatus} -> httpStatus) (\s@DescribeForecastResponse' {} a -> s {httpStatus = a} :: DescribeForecastResponse)

instance Prelude.NFData DescribeForecastResponse where
  rnf DescribeForecastResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf forecastTypes
      `Prelude.seq` Prelude.rnf forecastName
      `Prelude.seq` Prelude.rnf estimatedTimeRemainingInMinutes
      `Prelude.seq` Prelude.rnf forecastArn
      `Prelude.seq` Prelude.rnf predictorArn
      `Prelude.seq` Prelude.rnf status
