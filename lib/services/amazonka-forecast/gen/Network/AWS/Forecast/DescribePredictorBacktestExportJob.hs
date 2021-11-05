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
-- Module      : Network.AWS.Forecast.DescribePredictorBacktestExportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Forecast.DescribePredictorBacktestExportJob
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
    describePredictorBacktestExportJobResponse_status,
    describePredictorBacktestExportJobResponse_destination,
    describePredictorBacktestExportJobResponse_predictorArn,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_message,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobName,
    describePredictorBacktestExportJobResponse_lastModificationTime,
    describePredictorBacktestExportJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Forecast.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePredictorBacktestExportJobResponse'
            Prelude.<$> (x Core..?> "CreationTime")
              Prelude.<*> (x Core..?> "Status")
              Prelude.<*> (x Core..?> "Destination")
              Prelude.<*> (x Core..?> "PredictorArn")
              Prelude.<*> (x Core..?> "PredictorBacktestExportJobArn")
              Prelude.<*> (x Core..?> "Message")
              Prelude.<*> (x Core..?> "PredictorBacktestExportJobName")
              Prelude.<*> (x Core..?> "LastModificationTime")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePredictorBacktestExportJob

instance
  Prelude.NFData
    DescribePredictorBacktestExportJob

instance
  Core.ToHeaders
    DescribePredictorBacktestExportJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.DescribePredictorBacktestExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribePredictorBacktestExportJob
  where
  toJSON DescribePredictorBacktestExportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PredictorBacktestExportJobArn"
                  Core..= predictorBacktestExportJobArn
              )
          ]
      )

instance
  Core.ToPath
    DescribePredictorBacktestExportJob
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribePredictorBacktestExportJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePredictorBacktestExportJobResponse' smart constructor.
data DescribePredictorBacktestExportJobResponse = DescribePredictorBacktestExportJobResponse'
  { -- | When the predictor backtest export job was created.
    creationTime :: Prelude.Maybe Core.POSIX,
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
    destination :: Prelude.Maybe DataDestination,
    -- | The Amazon Resource Name (ARN) of the predictor.
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the predictor backtest export job.
    predictorBacktestExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | Information about any errors that may have occurred during the backtest
    -- export.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the predictor backtest export job.
    predictorBacktestExportJobName :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'DescribePredictorBacktestExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describePredictorBacktestExportJobResponse_creationTime' - When the predictor backtest export job was created.
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
-- 'destination', 'describePredictorBacktestExportJobResponse_destination' - Undocumented member.
--
-- 'predictorArn', 'describePredictorBacktestExportJobResponse_predictorArn' - The Amazon Resource Name (ARN) of the predictor.
--
-- 'predictorBacktestExportJobArn', 'describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn' - The Amazon Resource Name (ARN) of the predictor backtest export job.
--
-- 'message', 'describePredictorBacktestExportJobResponse_message' - Information about any errors that may have occurred during the backtest
-- export.
--
-- 'predictorBacktestExportJobName', 'describePredictorBacktestExportJobResponse_predictorBacktestExportJobName' - The name of the predictor backtest export job.
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
        status = Prelude.Nothing,
        destination = Prelude.Nothing,
        predictorArn = Prelude.Nothing,
        predictorBacktestExportJobArn =
          Prelude.Nothing,
        message = Prelude.Nothing,
        predictorBacktestExportJobName =
          Prelude.Nothing,
        lastModificationTime =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | When the predictor backtest export job was created.
describePredictorBacktestExportJobResponse_creationTime :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describePredictorBacktestExportJobResponse_creationTime = Lens.lens (\DescribePredictorBacktestExportJobResponse' {creationTime} -> creationTime) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {creationTime = a} :: DescribePredictorBacktestExportJobResponse) Prelude.. Lens.mapping Core._Time

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

-- | Undocumented member.
describePredictorBacktestExportJobResponse_destination :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe DataDestination)
describePredictorBacktestExportJobResponse_destination = Lens.lens (\DescribePredictorBacktestExportJobResponse' {destination} -> destination) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {destination = a} :: DescribePredictorBacktestExportJobResponse)

-- | The Amazon Resource Name (ARN) of the predictor.
describePredictorBacktestExportJobResponse_predictorArn :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_predictorArn = Lens.lens (\DescribePredictorBacktestExportJobResponse' {predictorArn} -> predictorArn) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {predictorArn = a} :: DescribePredictorBacktestExportJobResponse)

-- | The Amazon Resource Name (ARN) of the predictor backtest export job.
describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn = Lens.lens (\DescribePredictorBacktestExportJobResponse' {predictorBacktestExportJobArn} -> predictorBacktestExportJobArn) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {predictorBacktestExportJobArn = a} :: DescribePredictorBacktestExportJobResponse)

-- | Information about any errors that may have occurred during the backtest
-- export.
describePredictorBacktestExportJobResponse_message :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_message = Lens.lens (\DescribePredictorBacktestExportJobResponse' {message} -> message) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {message = a} :: DescribePredictorBacktestExportJobResponse)

-- | The name of the predictor backtest export job.
describePredictorBacktestExportJobResponse_predictorBacktestExportJobName :: Lens.Lens' DescribePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
describePredictorBacktestExportJobResponse_predictorBacktestExportJobName = Lens.lens (\DescribePredictorBacktestExportJobResponse' {predictorBacktestExportJobName} -> predictorBacktestExportJobName) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {predictorBacktestExportJobName = a} :: DescribePredictorBacktestExportJobResponse)

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
describePredictorBacktestExportJobResponse_lastModificationTime = Lens.lens (\DescribePredictorBacktestExportJobResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {lastModificationTime = a} :: DescribePredictorBacktestExportJobResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describePredictorBacktestExportJobResponse_httpStatus :: Lens.Lens' DescribePredictorBacktestExportJobResponse Prelude.Int
describePredictorBacktestExportJobResponse_httpStatus = Lens.lens (\DescribePredictorBacktestExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribePredictorBacktestExportJobResponse' {} a -> s {httpStatus = a} :: DescribePredictorBacktestExportJobResponse)

instance
  Prelude.NFData
    DescribePredictorBacktestExportJobResponse
