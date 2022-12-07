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
-- Module      : Amazonka.Forecast.DescribeExplainability
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Explainability resource created using the
-- CreateExplainability operation.
module Amazonka.Forecast.DescribeExplainability
  ( -- * Creating a Request
    DescribeExplainability (..),
    newDescribeExplainability,

    -- * Request Lenses
    describeExplainability_explainabilityArn,

    -- * Destructuring the Response
    DescribeExplainabilityResponse (..),
    newDescribeExplainabilityResponse,

    -- * Response Lenses
    describeExplainabilityResponse_lastModificationTime,
    describeExplainabilityResponse_message,
    describeExplainabilityResponse_explainabilityConfig,
    describeExplainabilityResponse_enableVisualization,
    describeExplainabilityResponse_startDateTime,
    describeExplainabilityResponse_status,
    describeExplainabilityResponse_explainabilityArn,
    describeExplainabilityResponse_explainabilityName,
    describeExplainabilityResponse_estimatedTimeRemainingInMinutes,
    describeExplainabilityResponse_schema,
    describeExplainabilityResponse_dataSource,
    describeExplainabilityResponse_creationTime,
    describeExplainabilityResponse_resourceArn,
    describeExplainabilityResponse_endDateTime,
    describeExplainabilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExplainability' smart constructor.
data DescribeExplainability = DescribeExplainability'
  { -- | The Amazon Resource Name (ARN) of the Explaianability to describe.
    explainabilityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExplainability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explainabilityArn', 'describeExplainability_explainabilityArn' - The Amazon Resource Name (ARN) of the Explaianability to describe.
newDescribeExplainability ::
  -- | 'explainabilityArn'
  Prelude.Text ->
  DescribeExplainability
newDescribeExplainability pExplainabilityArn_ =
  DescribeExplainability'
    { explainabilityArn =
        pExplainabilityArn_
    }

-- | The Amazon Resource Name (ARN) of the Explaianability to describe.
describeExplainability_explainabilityArn :: Lens.Lens' DescribeExplainability Prelude.Text
describeExplainability_explainabilityArn = Lens.lens (\DescribeExplainability' {explainabilityArn} -> explainabilityArn) (\s@DescribeExplainability' {} a -> s {explainabilityArn = a} :: DescribeExplainability)

instance Core.AWSRequest DescribeExplainability where
  type
    AWSResponse DescribeExplainability =
      DescribeExplainabilityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExplainabilityResponse'
            Prelude.<$> (x Data..?> "LastModificationTime")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "ExplainabilityConfig")
            Prelude.<*> (x Data..?> "EnableVisualization")
            Prelude.<*> (x Data..?> "StartDateTime")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "ExplainabilityArn")
            Prelude.<*> (x Data..?> "ExplainabilityName")
            Prelude.<*> (x Data..?> "EstimatedTimeRemainingInMinutes")
            Prelude.<*> (x Data..?> "Schema")
            Prelude.<*> (x Data..?> "DataSource")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "EndDateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExplainability where
  hashWithSalt _salt DescribeExplainability' {..} =
    _salt `Prelude.hashWithSalt` explainabilityArn

instance Prelude.NFData DescribeExplainability where
  rnf DescribeExplainability' {..} =
    Prelude.rnf explainabilityArn

instance Data.ToHeaders DescribeExplainability where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribeExplainability" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeExplainability where
  toJSON DescribeExplainability' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ExplainabilityArn" Data..= explainabilityArn)
          ]
      )

instance Data.ToPath DescribeExplainability where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeExplainability where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExplainabilityResponse' smart constructor.
data DescribeExplainabilityResponse = DescribeExplainabilityResponse'
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
    -- | If an error occurred, a message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The configuration settings that define the granularity of time series
    -- and time points for the Explainability.
    explainabilityConfig :: Prelude.Maybe ExplainabilityConfig,
    -- | Whether the visualization was enabled for the Explainability resource.
    enableVisualization :: Prelude.Maybe Prelude.Bool,
    -- | If @TimePointGranularity@ is set to @SPECIFIC@, the first time point in
    -- the Explainability.
    startDateTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the Explainability resource. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Explainability.
    explainabilityArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Explainability.
    explainabilityName :: Prelude.Maybe Prelude.Text,
    -- | The estimated time remaining in minutes for the CreateExplainability job
    -- to complete.
    estimatedTimeRemainingInMinutes :: Prelude.Maybe Prelude.Integer,
    schema :: Prelude.Maybe Schema,
    dataSource :: Prelude.Maybe DataSource,
    -- | When the Explainability resource was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the Predictor or Forecast used to
    -- create the Explainability resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | If @TimePointGranularity@ is set to @SPECIFIC@, the last time point in
    -- the Explainability.
    endDateTime :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExplainabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'describeExplainabilityResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'message', 'describeExplainabilityResponse_message' - If an error occurred, a message about the error.
--
-- 'explainabilityConfig', 'describeExplainabilityResponse_explainabilityConfig' - The configuration settings that define the granularity of time series
-- and time points for the Explainability.
--
-- 'enableVisualization', 'describeExplainabilityResponse_enableVisualization' - Whether the visualization was enabled for the Explainability resource.
--
-- 'startDateTime', 'describeExplainabilityResponse_startDateTime' - If @TimePointGranularity@ is set to @SPECIFIC@, the first time point in
-- the Explainability.
--
-- 'status', 'describeExplainabilityResponse_status' - The status of the Explainability resource. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- 'explainabilityArn', 'describeExplainabilityResponse_explainabilityArn' - The Amazon Resource Name (ARN) of the Explainability.
--
-- 'explainabilityName', 'describeExplainabilityResponse_explainabilityName' - The name of the Explainability.
--
-- 'estimatedTimeRemainingInMinutes', 'describeExplainabilityResponse_estimatedTimeRemainingInMinutes' - The estimated time remaining in minutes for the CreateExplainability job
-- to complete.
--
-- 'schema', 'describeExplainabilityResponse_schema' - Undocumented member.
--
-- 'dataSource', 'describeExplainabilityResponse_dataSource' - Undocumented member.
--
-- 'creationTime', 'describeExplainabilityResponse_creationTime' - When the Explainability resource was created.
--
-- 'resourceArn', 'describeExplainabilityResponse_resourceArn' - The Amazon Resource Name (ARN) of the Predictor or Forecast used to
-- create the Explainability resource.
--
-- 'endDateTime', 'describeExplainabilityResponse_endDateTime' - If @TimePointGranularity@ is set to @SPECIFIC@, the last time point in
-- the Explainability.
--
-- 'httpStatus', 'describeExplainabilityResponse_httpStatus' - The response's http status code.
newDescribeExplainabilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExplainabilityResponse
newDescribeExplainabilityResponse pHttpStatus_ =
  DescribeExplainabilityResponse'
    { lastModificationTime =
        Prelude.Nothing,
      message = Prelude.Nothing,
      explainabilityConfig = Prelude.Nothing,
      enableVisualization = Prelude.Nothing,
      startDateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      explainabilityArn = Prelude.Nothing,
      explainabilityName = Prelude.Nothing,
      estimatedTimeRemainingInMinutes =
        Prelude.Nothing,
      schema = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
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
describeExplainabilityResponse_lastModificationTime :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.UTCTime)
describeExplainabilityResponse_lastModificationTime = Lens.lens (\DescribeExplainabilityResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeExplainabilityResponse' {} a -> s {lastModificationTime = a} :: DescribeExplainabilityResponse) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, a message about the error.
describeExplainabilityResponse_message :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityResponse_message = Lens.lens (\DescribeExplainabilityResponse' {message} -> message) (\s@DescribeExplainabilityResponse' {} a -> s {message = a} :: DescribeExplainabilityResponse)

-- | The configuration settings that define the granularity of time series
-- and time points for the Explainability.
describeExplainabilityResponse_explainabilityConfig :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe ExplainabilityConfig)
describeExplainabilityResponse_explainabilityConfig = Lens.lens (\DescribeExplainabilityResponse' {explainabilityConfig} -> explainabilityConfig) (\s@DescribeExplainabilityResponse' {} a -> s {explainabilityConfig = a} :: DescribeExplainabilityResponse)

-- | Whether the visualization was enabled for the Explainability resource.
describeExplainabilityResponse_enableVisualization :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Bool)
describeExplainabilityResponse_enableVisualization = Lens.lens (\DescribeExplainabilityResponse' {enableVisualization} -> enableVisualization) (\s@DescribeExplainabilityResponse' {} a -> s {enableVisualization = a} :: DescribeExplainabilityResponse)

-- | If @TimePointGranularity@ is set to @SPECIFIC@, the first time point in
-- the Explainability.
describeExplainabilityResponse_startDateTime :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityResponse_startDateTime = Lens.lens (\DescribeExplainabilityResponse' {startDateTime} -> startDateTime) (\s@DescribeExplainabilityResponse' {} a -> s {startDateTime = a} :: DescribeExplainabilityResponse)

-- | The status of the Explainability resource. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
describeExplainabilityResponse_status :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityResponse_status = Lens.lens (\DescribeExplainabilityResponse' {status} -> status) (\s@DescribeExplainabilityResponse' {} a -> s {status = a} :: DescribeExplainabilityResponse)

-- | The Amazon Resource Name (ARN) of the Explainability.
describeExplainabilityResponse_explainabilityArn :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityResponse_explainabilityArn = Lens.lens (\DescribeExplainabilityResponse' {explainabilityArn} -> explainabilityArn) (\s@DescribeExplainabilityResponse' {} a -> s {explainabilityArn = a} :: DescribeExplainabilityResponse)

-- | The name of the Explainability.
describeExplainabilityResponse_explainabilityName :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityResponse_explainabilityName = Lens.lens (\DescribeExplainabilityResponse' {explainabilityName} -> explainabilityName) (\s@DescribeExplainabilityResponse' {} a -> s {explainabilityName = a} :: DescribeExplainabilityResponse)

-- | The estimated time remaining in minutes for the CreateExplainability job
-- to complete.
describeExplainabilityResponse_estimatedTimeRemainingInMinutes :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Integer)
describeExplainabilityResponse_estimatedTimeRemainingInMinutes = Lens.lens (\DescribeExplainabilityResponse' {estimatedTimeRemainingInMinutes} -> estimatedTimeRemainingInMinutes) (\s@DescribeExplainabilityResponse' {} a -> s {estimatedTimeRemainingInMinutes = a} :: DescribeExplainabilityResponse)

-- | Undocumented member.
describeExplainabilityResponse_schema :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Schema)
describeExplainabilityResponse_schema = Lens.lens (\DescribeExplainabilityResponse' {schema} -> schema) (\s@DescribeExplainabilityResponse' {} a -> s {schema = a} :: DescribeExplainabilityResponse)

-- | Undocumented member.
describeExplainabilityResponse_dataSource :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe DataSource)
describeExplainabilityResponse_dataSource = Lens.lens (\DescribeExplainabilityResponse' {dataSource} -> dataSource) (\s@DescribeExplainabilityResponse' {} a -> s {dataSource = a} :: DescribeExplainabilityResponse)

-- | When the Explainability resource was created.
describeExplainabilityResponse_creationTime :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.UTCTime)
describeExplainabilityResponse_creationTime = Lens.lens (\DescribeExplainabilityResponse' {creationTime} -> creationTime) (\s@DescribeExplainabilityResponse' {} a -> s {creationTime = a} :: DescribeExplainabilityResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the Predictor or Forecast used to
-- create the Explainability resource.
describeExplainabilityResponse_resourceArn :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityResponse_resourceArn = Lens.lens (\DescribeExplainabilityResponse' {resourceArn} -> resourceArn) (\s@DescribeExplainabilityResponse' {} a -> s {resourceArn = a} :: DescribeExplainabilityResponse)

-- | If @TimePointGranularity@ is set to @SPECIFIC@, the last time point in
-- the Explainability.
describeExplainabilityResponse_endDateTime :: Lens.Lens' DescribeExplainabilityResponse (Prelude.Maybe Prelude.Text)
describeExplainabilityResponse_endDateTime = Lens.lens (\DescribeExplainabilityResponse' {endDateTime} -> endDateTime) (\s@DescribeExplainabilityResponse' {} a -> s {endDateTime = a} :: DescribeExplainabilityResponse)

-- | The response's http status code.
describeExplainabilityResponse_httpStatus :: Lens.Lens' DescribeExplainabilityResponse Prelude.Int
describeExplainabilityResponse_httpStatus = Lens.lens (\DescribeExplainabilityResponse' {httpStatus} -> httpStatus) (\s@DescribeExplainabilityResponse' {} a -> s {httpStatus = a} :: DescribeExplainabilityResponse)

instance
  Prelude.NFData
    DescribeExplainabilityResponse
  where
  rnf DescribeExplainabilityResponse' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf explainabilityConfig
      `Prelude.seq` Prelude.rnf enableVisualization
      `Prelude.seq` Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf explainabilityArn
      `Prelude.seq` Prelude.rnf explainabilityName
      `Prelude.seq` Prelude.rnf estimatedTimeRemainingInMinutes
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf httpStatus
