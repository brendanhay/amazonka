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
-- Module      : Amazonka.Forecast.DescribeWhatIfForecast
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the what-if forecast created using the CreateWhatIfForecast
-- operation.
--
-- In addition to listing the properties provided in the
-- @CreateWhatIfForecast@ request, this operation lists the following
-- properties:
--
-- -   @CreationTime@
--
-- -   @LastModificationTime@
--
-- -   @Message@ - If an error occurred, information about the error.
--
-- -   @Status@
module Amazonka.Forecast.DescribeWhatIfForecast
  ( -- * Creating a Request
    DescribeWhatIfForecast (..),
    newDescribeWhatIfForecast,

    -- * Request Lenses
    describeWhatIfForecast_whatIfForecastArn,

    -- * Destructuring the Response
    DescribeWhatIfForecastResponse (..),
    newDescribeWhatIfForecastResponse,

    -- * Response Lenses
    describeWhatIfForecastResponse_creationTime,
    describeWhatIfForecastResponse_estimatedTimeRemainingInMinutes,
    describeWhatIfForecastResponse_forecastTypes,
    describeWhatIfForecastResponse_lastModificationTime,
    describeWhatIfForecastResponse_message,
    describeWhatIfForecastResponse_status,
    describeWhatIfForecastResponse_timeSeriesReplacementsDataSource,
    describeWhatIfForecastResponse_timeSeriesTransformations,
    describeWhatIfForecastResponse_whatIfAnalysisArn,
    describeWhatIfForecastResponse_whatIfForecastArn,
    describeWhatIfForecastResponse_whatIfForecastName,
    describeWhatIfForecastResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeWhatIfForecast' smart constructor.
data DescribeWhatIfForecast = DescribeWhatIfForecast'
  { -- | The Amazon Resource Name (ARN) of the what-if forecast that you are
    -- interested in.
    whatIfForecastArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWhatIfForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whatIfForecastArn', 'describeWhatIfForecast_whatIfForecastArn' - The Amazon Resource Name (ARN) of the what-if forecast that you are
-- interested in.
newDescribeWhatIfForecast ::
  -- | 'whatIfForecastArn'
  Prelude.Text ->
  DescribeWhatIfForecast
newDescribeWhatIfForecast pWhatIfForecastArn_ =
  DescribeWhatIfForecast'
    { whatIfForecastArn =
        pWhatIfForecastArn_
    }

-- | The Amazon Resource Name (ARN) of the what-if forecast that you are
-- interested in.
describeWhatIfForecast_whatIfForecastArn :: Lens.Lens' DescribeWhatIfForecast Prelude.Text
describeWhatIfForecast_whatIfForecastArn = Lens.lens (\DescribeWhatIfForecast' {whatIfForecastArn} -> whatIfForecastArn) (\s@DescribeWhatIfForecast' {} a -> s {whatIfForecastArn = a} :: DescribeWhatIfForecast)

instance Core.AWSRequest DescribeWhatIfForecast where
  type
    AWSResponse DescribeWhatIfForecast =
      DescribeWhatIfForecastResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWhatIfForecastResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "EstimatedTimeRemainingInMinutes")
            Prelude.<*> (x Data..?> "ForecastTypes")
            Prelude.<*> (x Data..?> "LastModificationTime")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TimeSeriesReplacementsDataSource")
            Prelude.<*> ( x
                            Data..?> "TimeSeriesTransformations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "WhatIfAnalysisArn")
            Prelude.<*> (x Data..?> "WhatIfForecastArn")
            Prelude.<*> (x Data..?> "WhatIfForecastName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWhatIfForecast where
  hashWithSalt _salt DescribeWhatIfForecast' {..} =
    _salt `Prelude.hashWithSalt` whatIfForecastArn

instance Prelude.NFData DescribeWhatIfForecast where
  rnf DescribeWhatIfForecast' {..} =
    Prelude.rnf whatIfForecastArn

instance Data.ToHeaders DescribeWhatIfForecast where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribeWhatIfForecast" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeWhatIfForecast where
  toJSON DescribeWhatIfForecast' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WhatIfForecastArn" Data..= whatIfForecastArn)
          ]
      )

instance Data.ToPath DescribeWhatIfForecast where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWhatIfForecast where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWhatIfForecastResponse' smart constructor.
data DescribeWhatIfForecastResponse = DescribeWhatIfForecastResponse'
  { -- | When the what-if forecast was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The approximate time remaining to complete the what-if forecast, in
    -- minutes.
    estimatedTimeRemainingInMinutes :: Prelude.Maybe Prelude.Integer,
    -- | The quantiles at which probabilistic forecasts are generated. You can
    -- specify up to five quantiles per what-if forecast in the
    -- CreateWhatIfForecast operation. If you didn\'t specify quantiles, the
    -- default values are @[\"0.1\", \"0.5\", \"0.9\"]@.
    forecastTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the what-if forecast. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- The @Status@ of the what-if forecast must be @ACTIVE@ before you can
    -- access the forecast.
    status :: Prelude.Maybe Prelude.Text,
    -- | An array of @S3Config@, @Schema@, and @Format@ elements that describe
    -- the replacement time series.
    timeSeriesReplacementsDataSource :: Prelude.Maybe TimeSeriesReplacementsDataSource,
    -- | An array of @Action@ and @TimeSeriesConditions@ elements that describe
    -- what transformations were applied to which time series.
    timeSeriesTransformations :: Prelude.Maybe [TimeSeriesTransformation],
    -- | The Amazon Resource Name (ARN) of the what-if analysis that contains
    -- this forecast.
    whatIfAnalysisArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the what-if forecast.
    whatIfForecastArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the what-if forecast.
    whatIfForecastName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWhatIfForecastResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeWhatIfForecastResponse_creationTime' - When the what-if forecast was created.
--
-- 'estimatedTimeRemainingInMinutes', 'describeWhatIfForecastResponse_estimatedTimeRemainingInMinutes' - The approximate time remaining to complete the what-if forecast, in
-- minutes.
--
-- 'forecastTypes', 'describeWhatIfForecastResponse_forecastTypes' - The quantiles at which probabilistic forecasts are generated. You can
-- specify up to five quantiles per what-if forecast in the
-- CreateWhatIfForecast operation. If you didn\'t specify quantiles, the
-- default values are @[\"0.1\", \"0.5\", \"0.9\"]@.
--
-- 'lastModificationTime', 'describeWhatIfForecastResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'message', 'describeWhatIfForecastResponse_message' - If an error occurred, an informational message about the error.
--
-- 'status', 'describeWhatIfForecastResponse_status' - The status of the what-if forecast. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the what-if forecast must be @ACTIVE@ before you can
-- access the forecast.
--
-- 'timeSeriesReplacementsDataSource', 'describeWhatIfForecastResponse_timeSeriesReplacementsDataSource' - An array of @S3Config@, @Schema@, and @Format@ elements that describe
-- the replacement time series.
--
-- 'timeSeriesTransformations', 'describeWhatIfForecastResponse_timeSeriesTransformations' - An array of @Action@ and @TimeSeriesConditions@ elements that describe
-- what transformations were applied to which time series.
--
-- 'whatIfAnalysisArn', 'describeWhatIfForecastResponse_whatIfAnalysisArn' - The Amazon Resource Name (ARN) of the what-if analysis that contains
-- this forecast.
--
-- 'whatIfForecastArn', 'describeWhatIfForecastResponse_whatIfForecastArn' - The Amazon Resource Name (ARN) of the what-if forecast.
--
-- 'whatIfForecastName', 'describeWhatIfForecastResponse_whatIfForecastName' - The name of the what-if forecast.
--
-- 'httpStatus', 'describeWhatIfForecastResponse_httpStatus' - The response's http status code.
newDescribeWhatIfForecastResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWhatIfForecastResponse
newDescribeWhatIfForecastResponse pHttpStatus_ =
  DescribeWhatIfForecastResponse'
    { creationTime =
        Prelude.Nothing,
      estimatedTimeRemainingInMinutes =
        Prelude.Nothing,
      forecastTypes = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing,
      timeSeriesReplacementsDataSource =
        Prelude.Nothing,
      timeSeriesTransformations = Prelude.Nothing,
      whatIfAnalysisArn = Prelude.Nothing,
      whatIfForecastArn = Prelude.Nothing,
      whatIfForecastName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the what-if forecast was created.
describeWhatIfForecastResponse_creationTime :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe Prelude.UTCTime)
describeWhatIfForecastResponse_creationTime = Lens.lens (\DescribeWhatIfForecastResponse' {creationTime} -> creationTime) (\s@DescribeWhatIfForecastResponse' {} a -> s {creationTime = a} :: DescribeWhatIfForecastResponse) Prelude.. Lens.mapping Data._Time

-- | The approximate time remaining to complete the what-if forecast, in
-- minutes.
describeWhatIfForecastResponse_estimatedTimeRemainingInMinutes :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe Prelude.Integer)
describeWhatIfForecastResponse_estimatedTimeRemainingInMinutes = Lens.lens (\DescribeWhatIfForecastResponse' {estimatedTimeRemainingInMinutes} -> estimatedTimeRemainingInMinutes) (\s@DescribeWhatIfForecastResponse' {} a -> s {estimatedTimeRemainingInMinutes = a} :: DescribeWhatIfForecastResponse)

-- | The quantiles at which probabilistic forecasts are generated. You can
-- specify up to five quantiles per what-if forecast in the
-- CreateWhatIfForecast operation. If you didn\'t specify quantiles, the
-- default values are @[\"0.1\", \"0.5\", \"0.9\"]@.
describeWhatIfForecastResponse_forecastTypes :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeWhatIfForecastResponse_forecastTypes = Lens.lens (\DescribeWhatIfForecastResponse' {forecastTypes} -> forecastTypes) (\s@DescribeWhatIfForecastResponse' {} a -> s {forecastTypes = a} :: DescribeWhatIfForecastResponse) Prelude.. Lens.mapping Lens.coerced

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
describeWhatIfForecastResponse_lastModificationTime :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe Prelude.UTCTime)
describeWhatIfForecastResponse_lastModificationTime = Lens.lens (\DescribeWhatIfForecastResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeWhatIfForecastResponse' {} a -> s {lastModificationTime = a} :: DescribeWhatIfForecastResponse) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, an informational message about the error.
describeWhatIfForecastResponse_message :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe Prelude.Text)
describeWhatIfForecastResponse_message = Lens.lens (\DescribeWhatIfForecastResponse' {message} -> message) (\s@DescribeWhatIfForecastResponse' {} a -> s {message = a} :: DescribeWhatIfForecastResponse)

-- | The status of the what-if forecast. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the what-if forecast must be @ACTIVE@ before you can
-- access the forecast.
describeWhatIfForecastResponse_status :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe Prelude.Text)
describeWhatIfForecastResponse_status = Lens.lens (\DescribeWhatIfForecastResponse' {status} -> status) (\s@DescribeWhatIfForecastResponse' {} a -> s {status = a} :: DescribeWhatIfForecastResponse)

-- | An array of @S3Config@, @Schema@, and @Format@ elements that describe
-- the replacement time series.
describeWhatIfForecastResponse_timeSeriesReplacementsDataSource :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe TimeSeriesReplacementsDataSource)
describeWhatIfForecastResponse_timeSeriesReplacementsDataSource = Lens.lens (\DescribeWhatIfForecastResponse' {timeSeriesReplacementsDataSource} -> timeSeriesReplacementsDataSource) (\s@DescribeWhatIfForecastResponse' {} a -> s {timeSeriesReplacementsDataSource = a} :: DescribeWhatIfForecastResponse)

-- | An array of @Action@ and @TimeSeriesConditions@ elements that describe
-- what transformations were applied to which time series.
describeWhatIfForecastResponse_timeSeriesTransformations :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe [TimeSeriesTransformation])
describeWhatIfForecastResponse_timeSeriesTransformations = Lens.lens (\DescribeWhatIfForecastResponse' {timeSeriesTransformations} -> timeSeriesTransformations) (\s@DescribeWhatIfForecastResponse' {} a -> s {timeSeriesTransformations = a} :: DescribeWhatIfForecastResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the what-if analysis that contains
-- this forecast.
describeWhatIfForecastResponse_whatIfAnalysisArn :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe Prelude.Text)
describeWhatIfForecastResponse_whatIfAnalysisArn = Lens.lens (\DescribeWhatIfForecastResponse' {whatIfAnalysisArn} -> whatIfAnalysisArn) (\s@DescribeWhatIfForecastResponse' {} a -> s {whatIfAnalysisArn = a} :: DescribeWhatIfForecastResponse)

-- | The Amazon Resource Name (ARN) of the what-if forecast.
describeWhatIfForecastResponse_whatIfForecastArn :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe Prelude.Text)
describeWhatIfForecastResponse_whatIfForecastArn = Lens.lens (\DescribeWhatIfForecastResponse' {whatIfForecastArn} -> whatIfForecastArn) (\s@DescribeWhatIfForecastResponse' {} a -> s {whatIfForecastArn = a} :: DescribeWhatIfForecastResponse)

-- | The name of the what-if forecast.
describeWhatIfForecastResponse_whatIfForecastName :: Lens.Lens' DescribeWhatIfForecastResponse (Prelude.Maybe Prelude.Text)
describeWhatIfForecastResponse_whatIfForecastName = Lens.lens (\DescribeWhatIfForecastResponse' {whatIfForecastName} -> whatIfForecastName) (\s@DescribeWhatIfForecastResponse' {} a -> s {whatIfForecastName = a} :: DescribeWhatIfForecastResponse)

-- | The response's http status code.
describeWhatIfForecastResponse_httpStatus :: Lens.Lens' DescribeWhatIfForecastResponse Prelude.Int
describeWhatIfForecastResponse_httpStatus = Lens.lens (\DescribeWhatIfForecastResponse' {httpStatus} -> httpStatus) (\s@DescribeWhatIfForecastResponse' {} a -> s {httpStatus = a} :: DescribeWhatIfForecastResponse)

instance
  Prelude.NFData
    DescribeWhatIfForecastResponse
  where
  rnf DescribeWhatIfForecastResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf estimatedTimeRemainingInMinutes
      `Prelude.seq` Prelude.rnf forecastTypes
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf timeSeriesReplacementsDataSource
      `Prelude.seq` Prelude.rnf timeSeriesTransformations
      `Prelude.seq` Prelude.rnf whatIfAnalysisArn
      `Prelude.seq` Prelude.rnf whatIfForecastArn
      `Prelude.seq` Prelude.rnf whatIfForecastName
      `Prelude.seq` Prelude.rnf httpStatus
