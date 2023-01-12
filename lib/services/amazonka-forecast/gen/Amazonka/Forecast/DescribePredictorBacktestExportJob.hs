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
-- Module      : Amazonka.Forecast.DescribePredictorBacktestExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a predictor backtest export job created using the
-- CreatePredictorBacktestExportJob operation.
--
-- In addition to listing the properties provided by the user in the
-- @CreatePredictorBacktestExportJob@ request, this operation lists the
-- following properties:
--
-- -   @CreationTime@
--
-- -   @LastModificationTime@
--
-- -   @Status@
--
-- -   @Message@ (if an error occurred)
module Amazonka.Forecast.DescribePredictorBacktestExportJob
  ( -- * Creating a Request
    DescribePredictorBacktestExportJob (..),
    newDescribePredictorBacktestExportJob,

    -- * Request Lenses
    describePredictorBacktestExportJob_predictorBacktestExportJobArn,

    -- * Destructuring the Response
    DescribePredictorBacktestExportJobResponse (..),
    newDescribePredictorBacktestExportJobResponse,

    -- * Response Lenses
    describePredictorBacktestExportJobResponse_creationTime,
    describePredictorBacktestExportJobResponse_destination,
    describePredictorBacktestExportJobResponse_format,
    describePredictorBacktestExportJobResponse_lastModificationTime,
    describePredictorBacktestExportJobResponse_message,
    describePredictorBacktestExportJobResponse_predictorArn,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobName,
    describePredictorBacktestExportJobResponse_status,
    describePredictorBacktestExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePredictorBacktestExportJob' smart constructor.
data DescribePredictorBacktestExportJob = DescribePredictorBacktestExportJob'
  { -- | The Amazon Resource Name (ARN) of the predictor backtest export job.
    predictorBacktestExportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePredictorBacktestExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorBacktestExportJobArn', 'describePredictorBacktestExportJob_predictorBacktestExportJobArn' - The Amazon Resource Name (ARN) of the predictor backtest export job.
newDescribePredictorBacktestExportJob ::
  -- | 'predictorBacktestExportJobArn'
  Prelude.Text ->
  DescribePredictorBacktestExportJob
newDescribePredictorBacktestExportJob
  pPredictorBacktestExportJobArn_ =
    DescribePredictorBacktestExportJob'
      { predictorBacktestExportJobArn =
          pPredictorBacktestExportJobArn_
      }

-- | The Amazon Resource Name (ARN) of the predictor backtest export job.
describePredictorBacktestExportJob_predictorBacktestExportJobArn :: Lens.Lens' DescribePredictorBacktestExportJob Prelude.Text
describePredictorBacktestExportJob_predictorBacktestExportJobArn = Lens.lens (\DescribePredictorBacktestExportJob' {predictorBacktestExportJobArn} -> predictorBacktestExportJobArn) (\s@DescribePredictorBacktestExportJob' {} a -> s {predictorBacktestExportJobArn = a} :: DescribePredictorBacktestExportJob)

instance
  Core.AWSRequest
    DescribePredictorBacktestExportJob
  where
  type
    AWSResponse DescribePredictorBacktestExportJob =
      DescribePredictorBacktestExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePredictorBacktestExportJobResponse'
            Prelude.<$> (x Data..?> "CreationTime")
              Prelude.<*> (x Data..?> "Destination")
              Prelude.<*> (x Data..?> "Format")
              Prelude.<*> (x Data..?> "LastModificationTime")
              Prelude.<*> (x Data..?> "Message")
              Prelude.<*> (x Data..?> "PredictorArn")
              Prelude.<*> (x Data..?> "PredictorBacktestExportJobArn")
              Prelude.<*> (x Data..?> "PredictorBacktestExportJobName")
              Prelude.<*> (x Data..?> "Status")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePredictorBacktestExportJob
  where
  hashWithSalt
    _salt
    DescribePredictorBacktestExportJob' {..} =
      _salt
        `Prelude.hashWithSalt` predictorBacktestExportJobArn

instance
  Prelude.NFData
    DescribePredictorBacktestExportJob
  where
  rnf DescribePredictorBacktestExportJob' {..} =
    Prelude.rnf predictorBacktestExportJobArn

instance
  Data.ToHeaders
    DescribePredictorBacktestExportJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribePredictorBacktestExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribePredictorBacktestExportJob
  where
  toJSON DescribePredictorBacktestExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PredictorBacktestExportJobArn"
                  Data..= predictorBacktestExportJobArn
              )
          ]
      )

instance
  Data.ToPath
    DescribePredictorBacktestExportJob
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribePredictorBacktestExportJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePredictorBacktestExportJobResponse' smart constructor.
data DescribePredictorBacktestExportJobResponse = DescribePredictorBacktestExportJobResponse'
  { -- | When the predictor backtest export job was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    destination :: Prelude.Maybe DataDestination,
    -- | The format of the exported data, CSV or PARQUET.
    format :: Prelude.Maybe Prelude.Text,
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
    -- | Information about any errors that may have occurred during the backtest
    -- export.
    message :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the predictor.
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the predictor backtest export job.
    predictorBacktestExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the predictor backtest export job.
    predictorBacktestExportJobName :: Prelude.Maybe Prelude.Text,
    -- | The status of the predictor backtest export job. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePredictorBacktestExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describePredictorBacktestExportJobResponse_creationTime' - When the predictor backtest export job was created.
--
-- 'destination', 'describePredictorBacktestExportJobResponse_destination' - Undocumented member.
--
-- 'format', 'describePredictorBacktestExportJobResponse_format' - The format of the exported data, CSV or PARQUET.
--
-- 'lastModificationTime', 'describePredictorBacktestExportJobResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'message', 'describePredictorBacktestExportJobResponse_message' - Information about any errors that may have occurred during the backtest
-- export.
--
-- 'predictorArn', 'describePredictorBacktestExportJobResponse_predictorArn' - The Amazon Resource Name (ARN) of the predictor.
--
-- 'predictorBacktestExportJobArn', 'describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn' - The Amazon Resource Name (ARN) of the predictor backtest export job.
--
-- 'predictorBacktestExportJobName', 'describePredictorBacktestExportJobResponse_predictorBacktestExportJobName' - The name of the predictor backtest export job.
--
-- 'status', 'describePredictorBacktestExportJobResponse_status' - The status of the predictor backtest export job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- 'httpStatus', 'describePredictorBacktestExportJobResponse_httpStatus' - The response's http status code.
newDescribePredictorBacktestExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePredictorBacktestExportJobResponse
newDescribePredictorBacktestExportJobResponse
  pHttpStatus_ =
    DescribePredictorBacktestExportJobResponse'
      { creationTime =
          Prelude.Nothing,
        destination = Prelude.Nothing,
        format = Prelude.Nothing,
        lastModificationTime =
          Prelude.Nothing,
        message = Prelude.Nothing,
        predictorArn = Prelude.Nothing,
        predictorBacktestExportJobArn =
          Prelude.Nothing,
        predictorBacktestExportJobName =
          Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | When the predictor backtest export job was created.
describePredictorBacktestExportJobResponse_creationTime :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describePredictorBacktestExportJobResponse_creationTime = Lens.lens (\DescribePredictorBacktestExportJobResponse' {creationTime} -> creationTime) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {creationTime = a} :: DescribePredictorBacktestExportJobResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
describePredictorBacktestExportJobResponse_destination :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe DataDestination)
describePredictorBacktestExportJobResponse_destination = Lens.lens (\DescribePredictorBacktestExportJobResponse' {destination} -> destination) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {destination = a} :: DescribePredictorBacktestExportJobResponse)

-- | The format of the exported data, CSV or PARQUET.
describePredictorBacktestExportJobResponse_format :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_format = Lens.lens (\DescribePredictorBacktestExportJobResponse' {format} -> format) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {format = a} :: DescribePredictorBacktestExportJobResponse)

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
describePredictorBacktestExportJobResponse_lastModificationTime :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describePredictorBacktestExportJobResponse_lastModificationTime = Lens.lens (\DescribePredictorBacktestExportJobResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {lastModificationTime = a} :: DescribePredictorBacktestExportJobResponse) Prelude.. Lens.mapping Data._Time

-- | Information about any errors that may have occurred during the backtest
-- export.
describePredictorBacktestExportJobResponse_message :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_message = Lens.lens (\DescribePredictorBacktestExportJobResponse' {message} -> message) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {message = a} :: DescribePredictorBacktestExportJobResponse)

-- | The Amazon Resource Name (ARN) of the predictor.
describePredictorBacktestExportJobResponse_predictorArn :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_predictorArn = Lens.lens (\DescribePredictorBacktestExportJobResponse' {predictorArn} -> predictorArn) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {predictorArn = a} :: DescribePredictorBacktestExportJobResponse)

-- | The Amazon Resource Name (ARN) of the predictor backtest export job.
describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn = Lens.lens (\DescribePredictorBacktestExportJobResponse' {predictorBacktestExportJobArn} -> predictorBacktestExportJobArn) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {predictorBacktestExportJobArn = a} :: DescribePredictorBacktestExportJobResponse)

-- | The name of the predictor backtest export job.
describePredictorBacktestExportJobResponse_predictorBacktestExportJobName :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_predictorBacktestExportJobName = Lens.lens (\DescribePredictorBacktestExportJobResponse' {predictorBacktestExportJobName} -> predictorBacktestExportJobName) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {predictorBacktestExportJobName = a} :: DescribePredictorBacktestExportJobResponse)

-- | The status of the predictor backtest export job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
describePredictorBacktestExportJobResponse_status :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_status = Lens.lens (\DescribePredictorBacktestExportJobResponse' {status} -> status) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {status = a} :: DescribePredictorBacktestExportJobResponse)

-- | The response's http status code.
describePredictorBacktestExportJobResponse_httpStatus :: Lens.Lens' DescribePredictorBacktestExportJobResponse Prelude.Int
describePredictorBacktestExportJobResponse_httpStatus = Lens.lens (\DescribePredictorBacktestExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {httpStatus = a} :: DescribePredictorBacktestExportJobResponse)

instance
  Prelude.NFData
    DescribePredictorBacktestExportJobResponse
  where
  rnf DescribePredictorBacktestExportJobResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf predictorArn
      `Prelude.seq` Prelude.rnf predictorBacktestExportJobArn
      `Prelude.seq` Prelude.rnf predictorBacktestExportJobName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
