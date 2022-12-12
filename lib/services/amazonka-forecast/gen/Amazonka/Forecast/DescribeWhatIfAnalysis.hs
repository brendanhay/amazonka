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
-- Module      : Amazonka.Forecast.DescribeWhatIfAnalysis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the what-if analysis created using the CreateWhatIfAnalysis
-- operation.
--
-- In addition to listing the properties provided in the
-- @CreateWhatIfAnalysis@ request, this operation lists the following
-- properties:
--
-- -   @CreationTime@
--
-- -   @LastModificationTime@
--
-- -   @Message@ - If an error occurred, information about the error.
--
-- -   @Status@
module Amazonka.Forecast.DescribeWhatIfAnalysis
  ( -- * Creating a Request
    DescribeWhatIfAnalysis (..),
    newDescribeWhatIfAnalysis,

    -- * Request Lenses
    describeWhatIfAnalysis_whatIfAnalysisArn,

    -- * Destructuring the Response
    DescribeWhatIfAnalysisResponse (..),
    newDescribeWhatIfAnalysisResponse,

    -- * Response Lenses
    describeWhatIfAnalysisResponse_creationTime,
    describeWhatIfAnalysisResponse_estimatedTimeRemainingInMinutes,
    describeWhatIfAnalysisResponse_forecastArn,
    describeWhatIfAnalysisResponse_lastModificationTime,
    describeWhatIfAnalysisResponse_message,
    describeWhatIfAnalysisResponse_status,
    describeWhatIfAnalysisResponse_timeSeriesSelector,
    describeWhatIfAnalysisResponse_whatIfAnalysisArn,
    describeWhatIfAnalysisResponse_whatIfAnalysisName,
    describeWhatIfAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeWhatIfAnalysis' smart constructor.
data DescribeWhatIfAnalysis = DescribeWhatIfAnalysis'
  { -- | The Amazon Resource Name (ARN) of the what-if analysis that you are
    -- interested in.
    whatIfAnalysisArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWhatIfAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whatIfAnalysisArn', 'describeWhatIfAnalysis_whatIfAnalysisArn' - The Amazon Resource Name (ARN) of the what-if analysis that you are
-- interested in.
newDescribeWhatIfAnalysis ::
  -- | 'whatIfAnalysisArn'
  Prelude.Text ->
  DescribeWhatIfAnalysis
newDescribeWhatIfAnalysis pWhatIfAnalysisArn_ =
  DescribeWhatIfAnalysis'
    { whatIfAnalysisArn =
        pWhatIfAnalysisArn_
    }

-- | The Amazon Resource Name (ARN) of the what-if analysis that you are
-- interested in.
describeWhatIfAnalysis_whatIfAnalysisArn :: Lens.Lens' DescribeWhatIfAnalysis Prelude.Text
describeWhatIfAnalysis_whatIfAnalysisArn = Lens.lens (\DescribeWhatIfAnalysis' {whatIfAnalysisArn} -> whatIfAnalysisArn) (\s@DescribeWhatIfAnalysis' {} a -> s {whatIfAnalysisArn = a} :: DescribeWhatIfAnalysis)

instance Core.AWSRequest DescribeWhatIfAnalysis where
  type
    AWSResponse DescribeWhatIfAnalysis =
      DescribeWhatIfAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWhatIfAnalysisResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "EstimatedTimeRemainingInMinutes")
            Prelude.<*> (x Data..?> "ForecastArn")
            Prelude.<*> (x Data..?> "LastModificationTime")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TimeSeriesSelector")
            Prelude.<*> (x Data..?> "WhatIfAnalysisArn")
            Prelude.<*> (x Data..?> "WhatIfAnalysisName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWhatIfAnalysis where
  hashWithSalt _salt DescribeWhatIfAnalysis' {..} =
    _salt `Prelude.hashWithSalt` whatIfAnalysisArn

instance Prelude.NFData DescribeWhatIfAnalysis where
  rnf DescribeWhatIfAnalysis' {..} =
    Prelude.rnf whatIfAnalysisArn

instance Data.ToHeaders DescribeWhatIfAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribeWhatIfAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeWhatIfAnalysis where
  toJSON DescribeWhatIfAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WhatIfAnalysisArn" Data..= whatIfAnalysisArn)
          ]
      )

instance Data.ToPath DescribeWhatIfAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWhatIfAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWhatIfAnalysisResponse' smart constructor.
data DescribeWhatIfAnalysisResponse = DescribeWhatIfAnalysisResponse'
  { -- | When the what-if analysis was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The approximate time remaining to complete the what-if analysis, in
    -- minutes.
    estimatedTimeRemainingInMinutes :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the what-if forecast.
    forecastArn :: Prelude.Maybe Prelude.Text,
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
    -- | The status of the what-if analysis. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
    -- access the analysis.
    status :: Prelude.Maybe Prelude.Text,
    timeSeriesSelector :: Prelude.Maybe TimeSeriesSelector,
    -- | The Amazon Resource Name (ARN) of the what-if analysis.
    whatIfAnalysisArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the what-if analysis.
    whatIfAnalysisName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWhatIfAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeWhatIfAnalysisResponse_creationTime' - When the what-if analysis was created.
--
-- 'estimatedTimeRemainingInMinutes', 'describeWhatIfAnalysisResponse_estimatedTimeRemainingInMinutes' - The approximate time remaining to complete the what-if analysis, in
-- minutes.
--
-- 'forecastArn', 'describeWhatIfAnalysisResponse_forecastArn' - The Amazon Resource Name (ARN) of the what-if forecast.
--
-- 'lastModificationTime', 'describeWhatIfAnalysisResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'message', 'describeWhatIfAnalysisResponse_message' - If an error occurred, an informational message about the error.
--
-- 'status', 'describeWhatIfAnalysisResponse_status' - The status of the what-if analysis. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
-- access the analysis.
--
-- 'timeSeriesSelector', 'describeWhatIfAnalysisResponse_timeSeriesSelector' - Undocumented member.
--
-- 'whatIfAnalysisArn', 'describeWhatIfAnalysisResponse_whatIfAnalysisArn' - The Amazon Resource Name (ARN) of the what-if analysis.
--
-- 'whatIfAnalysisName', 'describeWhatIfAnalysisResponse_whatIfAnalysisName' - The name of the what-if analysis.
--
-- 'httpStatus', 'describeWhatIfAnalysisResponse_httpStatus' - The response's http status code.
newDescribeWhatIfAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWhatIfAnalysisResponse
newDescribeWhatIfAnalysisResponse pHttpStatus_ =
  DescribeWhatIfAnalysisResponse'
    { creationTime =
        Prelude.Nothing,
      estimatedTimeRemainingInMinutes =
        Prelude.Nothing,
      forecastArn = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing,
      timeSeriesSelector = Prelude.Nothing,
      whatIfAnalysisArn = Prelude.Nothing,
      whatIfAnalysisName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the what-if analysis was created.
describeWhatIfAnalysisResponse_creationTime :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe Prelude.UTCTime)
describeWhatIfAnalysisResponse_creationTime = Lens.lens (\DescribeWhatIfAnalysisResponse' {creationTime} -> creationTime) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {creationTime = a} :: DescribeWhatIfAnalysisResponse) Prelude.. Lens.mapping Data._Time

-- | The approximate time remaining to complete the what-if analysis, in
-- minutes.
describeWhatIfAnalysisResponse_estimatedTimeRemainingInMinutes :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe Prelude.Integer)
describeWhatIfAnalysisResponse_estimatedTimeRemainingInMinutes = Lens.lens (\DescribeWhatIfAnalysisResponse' {estimatedTimeRemainingInMinutes} -> estimatedTimeRemainingInMinutes) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {estimatedTimeRemainingInMinutes = a} :: DescribeWhatIfAnalysisResponse)

-- | The Amazon Resource Name (ARN) of the what-if forecast.
describeWhatIfAnalysisResponse_forecastArn :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe Prelude.Text)
describeWhatIfAnalysisResponse_forecastArn = Lens.lens (\DescribeWhatIfAnalysisResponse' {forecastArn} -> forecastArn) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {forecastArn = a} :: DescribeWhatIfAnalysisResponse)

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
describeWhatIfAnalysisResponse_lastModificationTime :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe Prelude.UTCTime)
describeWhatIfAnalysisResponse_lastModificationTime = Lens.lens (\DescribeWhatIfAnalysisResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {lastModificationTime = a} :: DescribeWhatIfAnalysisResponse) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, an informational message about the error.
describeWhatIfAnalysisResponse_message :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe Prelude.Text)
describeWhatIfAnalysisResponse_message = Lens.lens (\DescribeWhatIfAnalysisResponse' {message} -> message) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {message = a} :: DescribeWhatIfAnalysisResponse)

-- | The status of the what-if analysis. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
-- access the analysis.
describeWhatIfAnalysisResponse_status :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe Prelude.Text)
describeWhatIfAnalysisResponse_status = Lens.lens (\DescribeWhatIfAnalysisResponse' {status} -> status) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {status = a} :: DescribeWhatIfAnalysisResponse)

-- | Undocumented member.
describeWhatIfAnalysisResponse_timeSeriesSelector :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe TimeSeriesSelector)
describeWhatIfAnalysisResponse_timeSeriesSelector = Lens.lens (\DescribeWhatIfAnalysisResponse' {timeSeriesSelector} -> timeSeriesSelector) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {timeSeriesSelector = a} :: DescribeWhatIfAnalysisResponse)

-- | The Amazon Resource Name (ARN) of the what-if analysis.
describeWhatIfAnalysisResponse_whatIfAnalysisArn :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe Prelude.Text)
describeWhatIfAnalysisResponse_whatIfAnalysisArn = Lens.lens (\DescribeWhatIfAnalysisResponse' {whatIfAnalysisArn} -> whatIfAnalysisArn) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {whatIfAnalysisArn = a} :: DescribeWhatIfAnalysisResponse)

-- | The name of the what-if analysis.
describeWhatIfAnalysisResponse_whatIfAnalysisName :: Lens.Lens' DescribeWhatIfAnalysisResponse (Prelude.Maybe Prelude.Text)
describeWhatIfAnalysisResponse_whatIfAnalysisName = Lens.lens (\DescribeWhatIfAnalysisResponse' {whatIfAnalysisName} -> whatIfAnalysisName) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {whatIfAnalysisName = a} :: DescribeWhatIfAnalysisResponse)

-- | The response's http status code.
describeWhatIfAnalysisResponse_httpStatus :: Lens.Lens' DescribeWhatIfAnalysisResponse Prelude.Int
describeWhatIfAnalysisResponse_httpStatus = Lens.lens (\DescribeWhatIfAnalysisResponse' {httpStatus} -> httpStatus) (\s@DescribeWhatIfAnalysisResponse' {} a -> s {httpStatus = a} :: DescribeWhatIfAnalysisResponse)

instance
  Prelude.NFData
    DescribeWhatIfAnalysisResponse
  where
  rnf DescribeWhatIfAnalysisResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf estimatedTimeRemainingInMinutes
      `Prelude.seq` Prelude.rnf forecastArn
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf timeSeriesSelector
      `Prelude.seq` Prelude.rnf whatIfAnalysisArn
      `Prelude.seq` Prelude.rnf whatIfAnalysisName
      `Prelude.seq` Prelude.rnf httpStatus
