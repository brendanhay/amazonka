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
-- Module      : Amazonka.Forecast.DescribeForecastExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a forecast export job created using the
-- CreateForecastExportJob operation.
--
-- In addition to listing the properties provided by the user in the
-- @CreateForecastExportJob@ request, this operation lists the following
-- properties:
--
-- -   @CreationTime@
--
-- -   @LastModificationTime@
--
-- -   @Status@
--
-- -   @Message@ - If an error occurred, information about the error.
module Amazonka.Forecast.DescribeForecastExportJob
  ( -- * Creating a Request
    DescribeForecastExportJob (..),
    newDescribeForecastExportJob,

    -- * Request Lenses
    describeForecastExportJob_forecastExportJobArn,

    -- * Destructuring the Response
    DescribeForecastExportJobResponse (..),
    newDescribeForecastExportJobResponse,

    -- * Response Lenses
    describeForecastExportJobResponse_lastModificationTime,
    describeForecastExportJobResponse_destination,
    describeForecastExportJobResponse_message,
    describeForecastExportJobResponse_forecastExportJobName,
    describeForecastExportJobResponse_format,
    describeForecastExportJobResponse_forecastExportJobArn,
    describeForecastExportJobResponse_status,
    describeForecastExportJobResponse_forecastArn,
    describeForecastExportJobResponse_creationTime,
    describeForecastExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeForecastExportJob' smart constructor.
data DescribeForecastExportJob = DescribeForecastExportJob'
  { -- | The Amazon Resource Name (ARN) of the forecast export job.
    forecastExportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeForecastExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastExportJobArn', 'describeForecastExportJob_forecastExportJobArn' - The Amazon Resource Name (ARN) of the forecast export job.
newDescribeForecastExportJob ::
  -- | 'forecastExportJobArn'
  Prelude.Text ->
  DescribeForecastExportJob
newDescribeForecastExportJob pForecastExportJobArn_ =
  DescribeForecastExportJob'
    { forecastExportJobArn =
        pForecastExportJobArn_
    }

-- | The Amazon Resource Name (ARN) of the forecast export job.
describeForecastExportJob_forecastExportJobArn :: Lens.Lens' DescribeForecastExportJob Prelude.Text
describeForecastExportJob_forecastExportJobArn = Lens.lens (\DescribeForecastExportJob' {forecastExportJobArn} -> forecastExportJobArn) (\s@DescribeForecastExportJob' {} a -> s {forecastExportJobArn = a} :: DescribeForecastExportJob)

instance Core.AWSRequest DescribeForecastExportJob where
  type
    AWSResponse DescribeForecastExportJob =
      DescribeForecastExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeForecastExportJobResponse'
            Prelude.<$> (x Data..?> "LastModificationTime")
            Prelude.<*> (x Data..?> "Destination")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "ForecastExportJobName")
            Prelude.<*> (x Data..?> "Format")
            Prelude.<*> (x Data..?> "ForecastExportJobArn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "ForecastArn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeForecastExportJob where
  hashWithSalt _salt DescribeForecastExportJob' {..} =
    _salt `Prelude.hashWithSalt` forecastExportJobArn

instance Prelude.NFData DescribeForecastExportJob where
  rnf DescribeForecastExportJob' {..} =
    Prelude.rnf forecastExportJobArn

instance Data.ToHeaders DescribeForecastExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribeForecastExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeForecastExportJob where
  toJSON DescribeForecastExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ForecastExportJobArn"
                  Data..= forecastExportJobArn
              )
          ]
      )

instance Data.ToPath DescribeForecastExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeForecastExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeForecastExportJobResponse' smart constructor.
data DescribeForecastExportJobResponse = DescribeForecastExportJobResponse'
  { -- | The last time the resource was modified. The timestamp depends on the
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
    -- | The path to the Amazon Simple Storage Service (Amazon S3) bucket where
    -- the forecast is exported.
    destination :: Prelude.Maybe DataDestination,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the forecast export job.
    forecastExportJobName :: Prelude.Maybe Prelude.Text,
    -- | The format of the exported data, CSV or PARQUET.
    format :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the forecast export job.
    forecastExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the forecast export job. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- The @Status@ of the forecast export job must be @ACTIVE@ before you can
    -- access the forecast in your S3 bucket.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the exported forecast.
    forecastArn :: Prelude.Maybe Prelude.Text,
    -- | When the forecast export job was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeForecastExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'describeForecastExportJobResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'destination', 'describeForecastExportJobResponse_destination' - The path to the Amazon Simple Storage Service (Amazon S3) bucket where
-- the forecast is exported.
--
-- 'message', 'describeForecastExportJobResponse_message' - If an error occurred, an informational message about the error.
--
-- 'forecastExportJobName', 'describeForecastExportJobResponse_forecastExportJobName' - The name of the forecast export job.
--
-- 'format', 'describeForecastExportJobResponse_format' - The format of the exported data, CSV or PARQUET.
--
-- 'forecastExportJobArn', 'describeForecastExportJobResponse_forecastExportJobArn' - The ARN of the forecast export job.
--
-- 'status', 'describeForecastExportJobResponse_status' - The status of the forecast export job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the forecast export job must be @ACTIVE@ before you can
-- access the forecast in your S3 bucket.
--
-- 'forecastArn', 'describeForecastExportJobResponse_forecastArn' - The Amazon Resource Name (ARN) of the exported forecast.
--
-- 'creationTime', 'describeForecastExportJobResponse_creationTime' - When the forecast export job was created.
--
-- 'httpStatus', 'describeForecastExportJobResponse_httpStatus' - The response's http status code.
newDescribeForecastExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeForecastExportJobResponse
newDescribeForecastExportJobResponse pHttpStatus_ =
  DescribeForecastExportJobResponse'
    { lastModificationTime =
        Prelude.Nothing,
      destination = Prelude.Nothing,
      message = Prelude.Nothing,
      forecastExportJobName = Prelude.Nothing,
      format = Prelude.Nothing,
      forecastExportJobArn = Prelude.Nothing,
      status = Prelude.Nothing,
      forecastArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

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
describeForecastExportJobResponse_lastModificationTime :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeForecastExportJobResponse_lastModificationTime = Lens.lens (\DescribeForecastExportJobResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeForecastExportJobResponse' {} a -> s {lastModificationTime = a} :: DescribeForecastExportJobResponse) Prelude.. Lens.mapping Data._Time

-- | The path to the Amazon Simple Storage Service (Amazon S3) bucket where
-- the forecast is exported.
describeForecastExportJobResponse_destination :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe DataDestination)
describeForecastExportJobResponse_destination = Lens.lens (\DescribeForecastExportJobResponse' {destination} -> destination) (\s@DescribeForecastExportJobResponse' {} a -> s {destination = a} :: DescribeForecastExportJobResponse)

-- | If an error occurred, an informational message about the error.
describeForecastExportJobResponse_message :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe Prelude.Text)
describeForecastExportJobResponse_message = Lens.lens (\DescribeForecastExportJobResponse' {message} -> message) (\s@DescribeForecastExportJobResponse' {} a -> s {message = a} :: DescribeForecastExportJobResponse)

-- | The name of the forecast export job.
describeForecastExportJobResponse_forecastExportJobName :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe Prelude.Text)
describeForecastExportJobResponse_forecastExportJobName = Lens.lens (\DescribeForecastExportJobResponse' {forecastExportJobName} -> forecastExportJobName) (\s@DescribeForecastExportJobResponse' {} a -> s {forecastExportJobName = a} :: DescribeForecastExportJobResponse)

-- | The format of the exported data, CSV or PARQUET.
describeForecastExportJobResponse_format :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe Prelude.Text)
describeForecastExportJobResponse_format = Lens.lens (\DescribeForecastExportJobResponse' {format} -> format) (\s@DescribeForecastExportJobResponse' {} a -> s {format = a} :: DescribeForecastExportJobResponse)

-- | The ARN of the forecast export job.
describeForecastExportJobResponse_forecastExportJobArn :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe Prelude.Text)
describeForecastExportJobResponse_forecastExportJobArn = Lens.lens (\DescribeForecastExportJobResponse' {forecastExportJobArn} -> forecastExportJobArn) (\s@DescribeForecastExportJobResponse' {} a -> s {forecastExportJobArn = a} :: DescribeForecastExportJobResponse)

-- | The status of the forecast export job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the forecast export job must be @ACTIVE@ before you can
-- access the forecast in your S3 bucket.
describeForecastExportJobResponse_status :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe Prelude.Text)
describeForecastExportJobResponse_status = Lens.lens (\DescribeForecastExportJobResponse' {status} -> status) (\s@DescribeForecastExportJobResponse' {} a -> s {status = a} :: DescribeForecastExportJobResponse)

-- | The Amazon Resource Name (ARN) of the exported forecast.
describeForecastExportJobResponse_forecastArn :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe Prelude.Text)
describeForecastExportJobResponse_forecastArn = Lens.lens (\DescribeForecastExportJobResponse' {forecastArn} -> forecastArn) (\s@DescribeForecastExportJobResponse' {} a -> s {forecastArn = a} :: DescribeForecastExportJobResponse)

-- | When the forecast export job was created.
describeForecastExportJobResponse_creationTime :: Lens.Lens' DescribeForecastExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeForecastExportJobResponse_creationTime = Lens.lens (\DescribeForecastExportJobResponse' {creationTime} -> creationTime) (\s@DescribeForecastExportJobResponse' {} a -> s {creationTime = a} :: DescribeForecastExportJobResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeForecastExportJobResponse_httpStatus :: Lens.Lens' DescribeForecastExportJobResponse Prelude.Int
describeForecastExportJobResponse_httpStatus = Lens.lens (\DescribeForecastExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeForecastExportJobResponse' {} a -> s {httpStatus = a} :: DescribeForecastExportJobResponse)

instance
  Prelude.NFData
    DescribeForecastExportJobResponse
  where
  rnf DescribeForecastExportJobResponse' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf forecastExportJobName
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf forecastExportJobArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf forecastArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
