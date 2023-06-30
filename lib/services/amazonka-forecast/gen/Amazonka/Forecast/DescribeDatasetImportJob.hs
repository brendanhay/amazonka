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
-- Module      : Amazonka.Forecast.DescribeDatasetImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a dataset import job created using the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>
-- operation.
--
-- In addition to listing the parameters provided in the
-- @CreateDatasetImportJob@ request, this operation includes the following
-- properties:
--
-- -   @CreationTime@
--
-- -   @LastModificationTime@
--
-- -   @DataSize@
--
-- -   @FieldStatistics@
--
-- -   @Status@
--
-- -   @Message@ - If an error occurred, information about the error.
module Amazonka.Forecast.DescribeDatasetImportJob
  ( -- * Creating a Request
    DescribeDatasetImportJob (..),
    newDescribeDatasetImportJob,

    -- * Request Lenses
    describeDatasetImportJob_datasetImportJobArn,

    -- * Destructuring the Response
    DescribeDatasetImportJobResponse (..),
    newDescribeDatasetImportJobResponse,

    -- * Response Lenses
    describeDatasetImportJobResponse_creationTime,
    describeDatasetImportJobResponse_dataSize,
    describeDatasetImportJobResponse_dataSource,
    describeDatasetImportJobResponse_datasetArn,
    describeDatasetImportJobResponse_datasetImportJobArn,
    describeDatasetImportJobResponse_datasetImportJobName,
    describeDatasetImportJobResponse_estimatedTimeRemainingInMinutes,
    describeDatasetImportJobResponse_fieldStatistics,
    describeDatasetImportJobResponse_format,
    describeDatasetImportJobResponse_geolocationFormat,
    describeDatasetImportJobResponse_lastModificationTime,
    describeDatasetImportJobResponse_message,
    describeDatasetImportJobResponse_status,
    describeDatasetImportJobResponse_timeZone,
    describeDatasetImportJobResponse_timestampFormat,
    describeDatasetImportJobResponse_useGeolocationForTimeZone,
    describeDatasetImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDatasetImportJob' smart constructor.
data DescribeDatasetImportJob = DescribeDatasetImportJob'
  { -- | The Amazon Resource Name (ARN) of the dataset import job.
    datasetImportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetImportJobArn', 'describeDatasetImportJob_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job.
newDescribeDatasetImportJob ::
  -- | 'datasetImportJobArn'
  Prelude.Text ->
  DescribeDatasetImportJob
newDescribeDatasetImportJob pDatasetImportJobArn_ =
  DescribeDatasetImportJob'
    { datasetImportJobArn =
        pDatasetImportJobArn_
    }

-- | The Amazon Resource Name (ARN) of the dataset import job.
describeDatasetImportJob_datasetImportJobArn :: Lens.Lens' DescribeDatasetImportJob Prelude.Text
describeDatasetImportJob_datasetImportJobArn = Lens.lens (\DescribeDatasetImportJob' {datasetImportJobArn} -> datasetImportJobArn) (\s@DescribeDatasetImportJob' {} a -> s {datasetImportJobArn = a} :: DescribeDatasetImportJob)

instance Core.AWSRequest DescribeDatasetImportJob where
  type
    AWSResponse DescribeDatasetImportJob =
      DescribeDatasetImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetImportJobResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DataSize")
            Prelude.<*> (x Data..?> "DataSource")
            Prelude.<*> (x Data..?> "DatasetArn")
            Prelude.<*> (x Data..?> "DatasetImportJobArn")
            Prelude.<*> (x Data..?> "DatasetImportJobName")
            Prelude.<*> (x Data..?> "EstimatedTimeRemainingInMinutes")
            Prelude.<*> ( x
                            Data..?> "FieldStatistics"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Format")
            Prelude.<*> (x Data..?> "GeolocationFormat")
            Prelude.<*> (x Data..?> "LastModificationTime")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TimeZone")
            Prelude.<*> (x Data..?> "TimestampFormat")
            Prelude.<*> (x Data..?> "UseGeolocationForTimeZone")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDatasetImportJob where
  hashWithSalt _salt DescribeDatasetImportJob' {..} =
    _salt `Prelude.hashWithSalt` datasetImportJobArn

instance Prelude.NFData DescribeDatasetImportJob where
  rnf DescribeDatasetImportJob' {..} =
    Prelude.rnf datasetImportJobArn

instance Data.ToHeaders DescribeDatasetImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribeDatasetImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDatasetImportJob where
  toJSON DescribeDatasetImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DatasetImportJobArn" Data..= datasetImportJobArn)
          ]
      )

instance Data.ToPath DescribeDatasetImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDatasetImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetImportJobResponse' smart constructor.
data DescribeDatasetImportJobResponse = DescribeDatasetImportJobResponse'
  { -- | When the dataset import job was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The size of the dataset in gigabytes (GB) after the import job has
    -- finished.
    dataSize :: Prelude.Maybe Prelude.Double,
    -- | The location of the training data to import and an AWS Identity and
    -- Access Management (IAM) role that Amazon Forecast can assume to access
    -- the data.
    --
    -- If encryption is used, @DataSource@ includes an AWS Key Management
    -- Service (KMS) key.
    dataSource :: Prelude.Maybe DataSource,
    -- | The Amazon Resource Name (ARN) of the dataset that the training data was
    -- imported to.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset import job.
    datasetImportJobName :: Prelude.Maybe Prelude.Text,
    -- | The estimated time remaining in minutes for the dataset import job to
    -- complete.
    estimatedTimeRemainingInMinutes :: Prelude.Maybe Prelude.Integer,
    -- | Statistical information about each field in the input data.
    fieldStatistics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Statistics),
    -- | The format of the imported data, CSV or PARQUET.
    format :: Prelude.Maybe Prelude.Text,
    -- | The format of the geolocation attribute. Valid Values:@\"LAT_LONG\"@ and
    -- @\"CC_POSTALCODE\"@.
    geolocationFormat :: Prelude.Maybe Prelude.Text,
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
    -- | The status of the dataset import job. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The single time zone applied to every item in the dataset
    timeZone :: Prelude.Maybe Prelude.Text,
    -- | The format of timestamps in the dataset. The format that you specify
    -- depends on the @DataFrequency@ specified when the dataset was created.
    -- The following formats are supported
    --
    -- -   \"yyyy-MM-dd\"
    --
    --     For the following data frequencies: Y, M, W, and D
    --
    -- -   \"yyyy-MM-dd HH:mm:ss\"
    --
    --     For the following data frequencies: H, 30min, 15min, and 1min; and
    --     optionally, for: Y, M, W, and D
    timestampFormat :: Prelude.Maybe Prelude.Text,
    -- | Whether @TimeZone@ is automatically derived from the geolocation
    -- attribute.
    useGeolocationForTimeZone :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeDatasetImportJobResponse_creationTime' - When the dataset import job was created.
--
-- 'dataSize', 'describeDatasetImportJobResponse_dataSize' - The size of the dataset in gigabytes (GB) after the import job has
-- finished.
--
-- 'dataSource', 'describeDatasetImportJobResponse_dataSource' - The location of the training data to import and an AWS Identity and
-- Access Management (IAM) role that Amazon Forecast can assume to access
-- the data.
--
-- If encryption is used, @DataSource@ includes an AWS Key Management
-- Service (KMS) key.
--
-- 'datasetArn', 'describeDatasetImportJobResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset that the training data was
-- imported to.
--
-- 'datasetImportJobArn', 'describeDatasetImportJobResponse_datasetImportJobArn' - The ARN of the dataset import job.
--
-- 'datasetImportJobName', 'describeDatasetImportJobResponse_datasetImportJobName' - The name of the dataset import job.
--
-- 'estimatedTimeRemainingInMinutes', 'describeDatasetImportJobResponse_estimatedTimeRemainingInMinutes' - The estimated time remaining in minutes for the dataset import job to
-- complete.
--
-- 'fieldStatistics', 'describeDatasetImportJobResponse_fieldStatistics' - Statistical information about each field in the input data.
--
-- 'format', 'describeDatasetImportJobResponse_format' - The format of the imported data, CSV or PARQUET.
--
-- 'geolocationFormat', 'describeDatasetImportJobResponse_geolocationFormat' - The format of the geolocation attribute. Valid Values:@\"LAT_LONG\"@ and
-- @\"CC_POSTALCODE\"@.
--
-- 'lastModificationTime', 'describeDatasetImportJobResponse_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'message', 'describeDatasetImportJobResponse_message' - If an error occurred, an informational message about the error.
--
-- 'status', 'describeDatasetImportJobResponse_status' - The status of the dataset import job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- 'timeZone', 'describeDatasetImportJobResponse_timeZone' - The single time zone applied to every item in the dataset
--
-- 'timestampFormat', 'describeDatasetImportJobResponse_timestampFormat' - The format of timestamps in the dataset. The format that you specify
-- depends on the @DataFrequency@ specified when the dataset was created.
-- The following formats are supported
--
-- -   \"yyyy-MM-dd\"
--
--     For the following data frequencies: Y, M, W, and D
--
-- -   \"yyyy-MM-dd HH:mm:ss\"
--
--     For the following data frequencies: H, 30min, 15min, and 1min; and
--     optionally, for: Y, M, W, and D
--
-- 'useGeolocationForTimeZone', 'describeDatasetImportJobResponse_useGeolocationForTimeZone' - Whether @TimeZone@ is automatically derived from the geolocation
-- attribute.
--
-- 'httpStatus', 'describeDatasetImportJobResponse_httpStatus' - The response's http status code.
newDescribeDatasetImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatasetImportJobResponse
newDescribeDatasetImportJobResponse pHttpStatus_ =
  DescribeDatasetImportJobResponse'
    { creationTime =
        Prelude.Nothing,
      dataSize = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      datasetImportJobArn = Prelude.Nothing,
      datasetImportJobName = Prelude.Nothing,
      estimatedTimeRemainingInMinutes =
        Prelude.Nothing,
      fieldStatistics = Prelude.Nothing,
      format = Prelude.Nothing,
      geolocationFormat = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing,
      timeZone = Prelude.Nothing,
      timestampFormat = Prelude.Nothing,
      useGeolocationForTimeZone =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the dataset import job was created.
describeDatasetImportJobResponse_creationTime :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetImportJobResponse_creationTime = Lens.lens (\DescribeDatasetImportJobResponse' {creationTime} -> creationTime) (\s@DescribeDatasetImportJobResponse' {} a -> s {creationTime = a} :: DescribeDatasetImportJobResponse) Prelude.. Lens.mapping Data._Time

-- | The size of the dataset in gigabytes (GB) after the import job has
-- finished.
describeDatasetImportJobResponse_dataSize :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Double)
describeDatasetImportJobResponse_dataSize = Lens.lens (\DescribeDatasetImportJobResponse' {dataSize} -> dataSize) (\s@DescribeDatasetImportJobResponse' {} a -> s {dataSize = a} :: DescribeDatasetImportJobResponse)

-- | The location of the training data to import and an AWS Identity and
-- Access Management (IAM) role that Amazon Forecast can assume to access
-- the data.
--
-- If encryption is used, @DataSource@ includes an AWS Key Management
-- Service (KMS) key.
describeDatasetImportJobResponse_dataSource :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe DataSource)
describeDatasetImportJobResponse_dataSource = Lens.lens (\DescribeDatasetImportJobResponse' {dataSource} -> dataSource) (\s@DescribeDatasetImportJobResponse' {} a -> s {dataSource = a} :: DescribeDatasetImportJobResponse)

-- | The Amazon Resource Name (ARN) of the dataset that the training data was
-- imported to.
describeDatasetImportJobResponse_datasetArn :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_datasetArn = Lens.lens (\DescribeDatasetImportJobResponse' {datasetArn} -> datasetArn) (\s@DescribeDatasetImportJobResponse' {} a -> s {datasetArn = a} :: DescribeDatasetImportJobResponse)

-- | The ARN of the dataset import job.
describeDatasetImportJobResponse_datasetImportJobArn :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_datasetImportJobArn = Lens.lens (\DescribeDatasetImportJobResponse' {datasetImportJobArn} -> datasetImportJobArn) (\s@DescribeDatasetImportJobResponse' {} a -> s {datasetImportJobArn = a} :: DescribeDatasetImportJobResponse)

-- | The name of the dataset import job.
describeDatasetImportJobResponse_datasetImportJobName :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_datasetImportJobName = Lens.lens (\DescribeDatasetImportJobResponse' {datasetImportJobName} -> datasetImportJobName) (\s@DescribeDatasetImportJobResponse' {} a -> s {datasetImportJobName = a} :: DescribeDatasetImportJobResponse)

-- | The estimated time remaining in minutes for the dataset import job to
-- complete.
describeDatasetImportJobResponse_estimatedTimeRemainingInMinutes :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Integer)
describeDatasetImportJobResponse_estimatedTimeRemainingInMinutes = Lens.lens (\DescribeDatasetImportJobResponse' {estimatedTimeRemainingInMinutes} -> estimatedTimeRemainingInMinutes) (\s@DescribeDatasetImportJobResponse' {} a -> s {estimatedTimeRemainingInMinutes = a} :: DescribeDatasetImportJobResponse)

-- | Statistical information about each field in the input data.
describeDatasetImportJobResponse_fieldStatistics :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Statistics))
describeDatasetImportJobResponse_fieldStatistics = Lens.lens (\DescribeDatasetImportJobResponse' {fieldStatistics} -> fieldStatistics) (\s@DescribeDatasetImportJobResponse' {} a -> s {fieldStatistics = a} :: DescribeDatasetImportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The format of the imported data, CSV or PARQUET.
describeDatasetImportJobResponse_format :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_format = Lens.lens (\DescribeDatasetImportJobResponse' {format} -> format) (\s@DescribeDatasetImportJobResponse' {} a -> s {format = a} :: DescribeDatasetImportJobResponse)

-- | The format of the geolocation attribute. Valid Values:@\"LAT_LONG\"@ and
-- @\"CC_POSTALCODE\"@.
describeDatasetImportJobResponse_geolocationFormat :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_geolocationFormat = Lens.lens (\DescribeDatasetImportJobResponse' {geolocationFormat} -> geolocationFormat) (\s@DescribeDatasetImportJobResponse' {} a -> s {geolocationFormat = a} :: DescribeDatasetImportJobResponse)

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
describeDatasetImportJobResponse_lastModificationTime :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetImportJobResponse_lastModificationTime = Lens.lens (\DescribeDatasetImportJobResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeDatasetImportJobResponse' {} a -> s {lastModificationTime = a} :: DescribeDatasetImportJobResponse) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, an informational message about the error.
describeDatasetImportJobResponse_message :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_message = Lens.lens (\DescribeDatasetImportJobResponse' {message} -> message) (\s@DescribeDatasetImportJobResponse' {} a -> s {message = a} :: DescribeDatasetImportJobResponse)

-- | The status of the dataset import job. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
describeDatasetImportJobResponse_status :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_status = Lens.lens (\DescribeDatasetImportJobResponse' {status} -> status) (\s@DescribeDatasetImportJobResponse' {} a -> s {status = a} :: DescribeDatasetImportJobResponse)

-- | The single time zone applied to every item in the dataset
describeDatasetImportJobResponse_timeZone :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_timeZone = Lens.lens (\DescribeDatasetImportJobResponse' {timeZone} -> timeZone) (\s@DescribeDatasetImportJobResponse' {} a -> s {timeZone = a} :: DescribeDatasetImportJobResponse)

-- | The format of timestamps in the dataset. The format that you specify
-- depends on the @DataFrequency@ specified when the dataset was created.
-- The following formats are supported
--
-- -   \"yyyy-MM-dd\"
--
--     For the following data frequencies: Y, M, W, and D
--
-- -   \"yyyy-MM-dd HH:mm:ss\"
--
--     For the following data frequencies: H, 30min, 15min, and 1min; and
--     optionally, for: Y, M, W, and D
describeDatasetImportJobResponse_timestampFormat :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
describeDatasetImportJobResponse_timestampFormat = Lens.lens (\DescribeDatasetImportJobResponse' {timestampFormat} -> timestampFormat) (\s@DescribeDatasetImportJobResponse' {} a -> s {timestampFormat = a} :: DescribeDatasetImportJobResponse)

-- | Whether @TimeZone@ is automatically derived from the geolocation
-- attribute.
describeDatasetImportJobResponse_useGeolocationForTimeZone :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe Prelude.Bool)
describeDatasetImportJobResponse_useGeolocationForTimeZone = Lens.lens (\DescribeDatasetImportJobResponse' {useGeolocationForTimeZone} -> useGeolocationForTimeZone) (\s@DescribeDatasetImportJobResponse' {} a -> s {useGeolocationForTimeZone = a} :: DescribeDatasetImportJobResponse)

-- | The response's http status code.
describeDatasetImportJobResponse_httpStatus :: Lens.Lens' DescribeDatasetImportJobResponse Prelude.Int
describeDatasetImportJobResponse_httpStatus = Lens.lens (\DescribeDatasetImportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetImportJobResponse' {} a -> s {httpStatus = a} :: DescribeDatasetImportJobResponse)

instance
  Prelude.NFData
    DescribeDatasetImportJobResponse
  where
  rnf DescribeDatasetImportJobResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataSize
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf datasetImportJobName
      `Prelude.seq` Prelude.rnf estimatedTimeRemainingInMinutes
      `Prelude.seq` Prelude.rnf fieldStatistics
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf geolocationFormat
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf timeZone
      `Prelude.seq` Prelude.rnf timestampFormat
      `Prelude.seq` Prelude.rnf useGeolocationForTimeZone
      `Prelude.seq` Prelude.rnf httpStatus
