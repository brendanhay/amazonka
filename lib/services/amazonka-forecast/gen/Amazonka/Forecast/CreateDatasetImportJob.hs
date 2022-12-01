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
-- Module      : Amazonka.Forecast.CreateDatasetImportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports your training data to an Amazon Forecast dataset. You provide
-- the location of your training data in an Amazon Simple Storage Service
-- (Amazon S3) bucket and the Amazon Resource Name (ARN) of the dataset
-- that you want to import the data to.
--
-- You must specify a
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DataSource.html DataSource>
-- object that includes an AWS Identity and Access Management (IAM) role
-- that Amazon Forecast can assume to access the data, as Amazon Forecast
-- makes a copy of your data and processes it in an internal AWS system.
-- For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/aws-forecast-iam-roles.html Set up permissions>.
--
-- The training data must be in CSV or Parquet format. The delimiter must
-- be a comma (,).
--
-- You can specify the path to a specific file, the S3 bucket, or to a
-- folder in the S3 bucket. For the latter two cases, Amazon Forecast
-- imports all files up to the limit of 10,000 files.
--
-- Because dataset imports are not aggregated, your most recent dataset
-- import is the one that is used when training a predictor or generating a
-- forecast. Make sure that your most recent dataset import contains all of
-- the data you want to model off of, and not just the new data collected
-- since the previous import.
--
-- To get a list of all your dataset import jobs, filtered by specified
-- criteria, use the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_ListDatasetImportJobs.html ListDatasetImportJobs>
-- operation.
module Amazonka.Forecast.CreateDatasetImportJob
  ( -- * Creating a Request
    CreateDatasetImportJob (..),
    newCreateDatasetImportJob,

    -- * Request Lenses
    createDatasetImportJob_tags,
    createDatasetImportJob_format,
    createDatasetImportJob_timeZone,
    createDatasetImportJob_useGeolocationForTimeZone,
    createDatasetImportJob_timestampFormat,
    createDatasetImportJob_geolocationFormat,
    createDatasetImportJob_datasetImportJobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,

    -- * Destructuring the Response
    CreateDatasetImportJobResponse (..),
    newCreateDatasetImportJobResponse,

    -- * Response Lenses
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDatasetImportJob' smart constructor.
data CreateDatasetImportJob = CreateDatasetImportJob'
  { -- | The optional metadata that you apply to the dataset import job to help
    -- you categorize and organize them. Each tag consists of a key and an
    -- optional value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50.
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8.
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for keys as it is reserved for AWS use. You cannot
    --     edit or delete tag keys with this prefix. Values can have this
    --     prefix. If a tag value has @aws@ as its prefix but the key does not,
    --     then Forecast considers it to be a user tag and will count against
    --     the limit of 50 tags. Tags with only the key prefix of @aws@ do not
    --     count against your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | The format of the imported data, CSV or PARQUET. The default value is
    -- CSV.
    format :: Prelude.Maybe Prelude.Text,
    -- | A single time zone for every item in your dataset. This option is ideal
    -- for datasets with all timestamps within a single time zone, or if all
    -- timestamps are normalized to a single time zone.
    --
    -- Refer to the
    -- <http://joda-time.sourceforge.net/timezones.html Joda-Time API> for a
    -- complete list of valid time zone names.
    timeZone :: Prelude.Maybe Prelude.Text,
    -- | Automatically derive time zone information from the geolocation
    -- attribute. This option is ideal for datasets that contain timestamps in
    -- multiple time zones and those timestamps are expressed in local time.
    useGeolocationForTimeZone :: Prelude.Maybe Prelude.Bool,
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
    --
    -- If the format isn\'t specified, Amazon Forecast expects the format to be
    -- \"yyyy-MM-dd HH:mm:ss\".
    timestampFormat :: Prelude.Maybe Prelude.Text,
    -- | The format of the geolocation attribute. The geolocation attribute can
    -- be formatted in one of two ways:
    --
    -- -   @LAT_LONG@ - the latitude and longitude in decimal format (Example:
    --     47.61_-122.33).
    --
    -- -   @CC_POSTALCODE@ (US Only) - the country code (US), followed by the
    --     5-digit ZIP code (Example: US_98121).
    geolocationFormat :: Prelude.Maybe Prelude.Text,
    -- | The name for the dataset import job. We recommend including the current
    -- timestamp in the name, for example, @20190721DatasetImport@. This can
    -- help you avoid getting a @ResourceAlreadyExistsException@ exception.
    datasetImportJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Forecast dataset that you
    -- want to import data to.
    datasetArn :: Prelude.Text,
    -- | The location of the training data to import and an AWS Identity and
    -- Access Management (IAM) role that Amazon Forecast can assume to access
    -- the data. The training data must be stored in an Amazon S3 bucket.
    --
    -- If encryption is used, @DataSource@ must include an AWS Key Management
    -- Service (KMS) key and the IAM role must allow Amazon Forecast permission
    -- to access the key. The KMS key and IAM role must match those specified
    -- in the @EncryptionConfig@ parameter of the
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDataset.html CreateDataset>
    -- operation.
    dataSource :: DataSource
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDatasetImportJob_tags' - The optional metadata that you apply to the dataset import job to help
-- you categorize and organize them. Each tag consists of a key and an
-- optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50.
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8.
--
-- -   Maximum value length - 256 Unicode characters in UTF-8.
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for keys as it is reserved for AWS use. You cannot
--     edit or delete tag keys with this prefix. Values can have this
--     prefix. If a tag value has @aws@ as its prefix but the key does not,
--     then Forecast considers it to be a user tag and will count against
--     the limit of 50 tags. Tags with only the key prefix of @aws@ do not
--     count against your tags per resource limit.
--
-- 'format', 'createDatasetImportJob_format' - The format of the imported data, CSV or PARQUET. The default value is
-- CSV.
--
-- 'timeZone', 'createDatasetImportJob_timeZone' - A single time zone for every item in your dataset. This option is ideal
-- for datasets with all timestamps within a single time zone, or if all
-- timestamps are normalized to a single time zone.
--
-- Refer to the
-- <http://joda-time.sourceforge.net/timezones.html Joda-Time API> for a
-- complete list of valid time zone names.
--
-- 'useGeolocationForTimeZone', 'createDatasetImportJob_useGeolocationForTimeZone' - Automatically derive time zone information from the geolocation
-- attribute. This option is ideal for datasets that contain timestamps in
-- multiple time zones and those timestamps are expressed in local time.
--
-- 'timestampFormat', 'createDatasetImportJob_timestampFormat' - The format of timestamps in the dataset. The format that you specify
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
-- If the format isn\'t specified, Amazon Forecast expects the format to be
-- \"yyyy-MM-dd HH:mm:ss\".
--
-- 'geolocationFormat', 'createDatasetImportJob_geolocationFormat' - The format of the geolocation attribute. The geolocation attribute can
-- be formatted in one of two ways:
--
-- -   @LAT_LONG@ - the latitude and longitude in decimal format (Example:
--     47.61_-122.33).
--
-- -   @CC_POSTALCODE@ (US Only) - the country code (US), followed by the
--     5-digit ZIP code (Example: US_98121).
--
-- 'datasetImportJobName', 'createDatasetImportJob_datasetImportJobName' - The name for the dataset import job. We recommend including the current
-- timestamp in the name, for example, @20190721DatasetImport@. This can
-- help you avoid getting a @ResourceAlreadyExistsException@ exception.
--
-- 'datasetArn', 'createDatasetImportJob_datasetArn' - The Amazon Resource Name (ARN) of the Amazon Forecast dataset that you
-- want to import data to.
--
-- 'dataSource', 'createDatasetImportJob_dataSource' - The location of the training data to import and an AWS Identity and
-- Access Management (IAM) role that Amazon Forecast can assume to access
-- the data. The training data must be stored in an Amazon S3 bucket.
--
-- If encryption is used, @DataSource@ must include an AWS Key Management
-- Service (KMS) key and the IAM role must allow Amazon Forecast permission
-- to access the key. The KMS key and IAM role must match those specified
-- in the @EncryptionConfig@ parameter of the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDataset.html CreateDataset>
-- operation.
newCreateDatasetImportJob ::
  -- | 'datasetImportJobName'
  Prelude.Text ->
  -- | 'datasetArn'
  Prelude.Text ->
  -- | 'dataSource'
  DataSource ->
  CreateDatasetImportJob
newCreateDatasetImportJob
  pDatasetImportJobName_
  pDatasetArn_
  pDataSource_ =
    CreateDatasetImportJob'
      { tags = Prelude.Nothing,
        format = Prelude.Nothing,
        timeZone = Prelude.Nothing,
        useGeolocationForTimeZone = Prelude.Nothing,
        timestampFormat = Prelude.Nothing,
        geolocationFormat = Prelude.Nothing,
        datasetImportJobName = pDatasetImportJobName_,
        datasetArn = pDatasetArn_,
        dataSource = pDataSource_
      }

-- | The optional metadata that you apply to the dataset import job to help
-- you categorize and organize them. Each tag consists of a key and an
-- optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50.
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8.
--
-- -   Maximum value length - 256 Unicode characters in UTF-8.
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for keys as it is reserved for AWS use. You cannot
--     edit or delete tag keys with this prefix. Values can have this
--     prefix. If a tag value has @aws@ as its prefix but the key does not,
--     then Forecast considers it to be a user tag and will count against
--     the limit of 50 tags. Tags with only the key prefix of @aws@ do not
--     count against your tags per resource limit.
createDatasetImportJob_tags :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe [Tag])
createDatasetImportJob_tags = Lens.lens (\CreateDatasetImportJob' {tags} -> tags) (\s@CreateDatasetImportJob' {} a -> s {tags = a} :: CreateDatasetImportJob) Prelude.. Lens.mapping Lens.coerced

-- | The format of the imported data, CSV or PARQUET. The default value is
-- CSV.
createDatasetImportJob_format :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe Prelude.Text)
createDatasetImportJob_format = Lens.lens (\CreateDatasetImportJob' {format} -> format) (\s@CreateDatasetImportJob' {} a -> s {format = a} :: CreateDatasetImportJob)

-- | A single time zone for every item in your dataset. This option is ideal
-- for datasets with all timestamps within a single time zone, or if all
-- timestamps are normalized to a single time zone.
--
-- Refer to the
-- <http://joda-time.sourceforge.net/timezones.html Joda-Time API> for a
-- complete list of valid time zone names.
createDatasetImportJob_timeZone :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe Prelude.Text)
createDatasetImportJob_timeZone = Lens.lens (\CreateDatasetImportJob' {timeZone} -> timeZone) (\s@CreateDatasetImportJob' {} a -> s {timeZone = a} :: CreateDatasetImportJob)

-- | Automatically derive time zone information from the geolocation
-- attribute. This option is ideal for datasets that contain timestamps in
-- multiple time zones and those timestamps are expressed in local time.
createDatasetImportJob_useGeolocationForTimeZone :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe Prelude.Bool)
createDatasetImportJob_useGeolocationForTimeZone = Lens.lens (\CreateDatasetImportJob' {useGeolocationForTimeZone} -> useGeolocationForTimeZone) (\s@CreateDatasetImportJob' {} a -> s {useGeolocationForTimeZone = a} :: CreateDatasetImportJob)

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
--
-- If the format isn\'t specified, Amazon Forecast expects the format to be
-- \"yyyy-MM-dd HH:mm:ss\".
createDatasetImportJob_timestampFormat :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe Prelude.Text)
createDatasetImportJob_timestampFormat = Lens.lens (\CreateDatasetImportJob' {timestampFormat} -> timestampFormat) (\s@CreateDatasetImportJob' {} a -> s {timestampFormat = a} :: CreateDatasetImportJob)

-- | The format of the geolocation attribute. The geolocation attribute can
-- be formatted in one of two ways:
--
-- -   @LAT_LONG@ - the latitude and longitude in decimal format (Example:
--     47.61_-122.33).
--
-- -   @CC_POSTALCODE@ (US Only) - the country code (US), followed by the
--     5-digit ZIP code (Example: US_98121).
createDatasetImportJob_geolocationFormat :: Lens.Lens' CreateDatasetImportJob (Prelude.Maybe Prelude.Text)
createDatasetImportJob_geolocationFormat = Lens.lens (\CreateDatasetImportJob' {geolocationFormat} -> geolocationFormat) (\s@CreateDatasetImportJob' {} a -> s {geolocationFormat = a} :: CreateDatasetImportJob)

-- | The name for the dataset import job. We recommend including the current
-- timestamp in the name, for example, @20190721DatasetImport@. This can
-- help you avoid getting a @ResourceAlreadyExistsException@ exception.
createDatasetImportJob_datasetImportJobName :: Lens.Lens' CreateDatasetImportJob Prelude.Text
createDatasetImportJob_datasetImportJobName = Lens.lens (\CreateDatasetImportJob' {datasetImportJobName} -> datasetImportJobName) (\s@CreateDatasetImportJob' {} a -> s {datasetImportJobName = a} :: CreateDatasetImportJob)

-- | The Amazon Resource Name (ARN) of the Amazon Forecast dataset that you
-- want to import data to.
createDatasetImportJob_datasetArn :: Lens.Lens' CreateDatasetImportJob Prelude.Text
createDatasetImportJob_datasetArn = Lens.lens (\CreateDatasetImportJob' {datasetArn} -> datasetArn) (\s@CreateDatasetImportJob' {} a -> s {datasetArn = a} :: CreateDatasetImportJob)

-- | The location of the training data to import and an AWS Identity and
-- Access Management (IAM) role that Amazon Forecast can assume to access
-- the data. The training data must be stored in an Amazon S3 bucket.
--
-- If encryption is used, @DataSource@ must include an AWS Key Management
-- Service (KMS) key and the IAM role must allow Amazon Forecast permission
-- to access the key. The KMS key and IAM role must match those specified
-- in the @EncryptionConfig@ parameter of the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDataset.html CreateDataset>
-- operation.
createDatasetImportJob_dataSource :: Lens.Lens' CreateDatasetImportJob DataSource
createDatasetImportJob_dataSource = Lens.lens (\CreateDatasetImportJob' {dataSource} -> dataSource) (\s@CreateDatasetImportJob' {} a -> s {dataSource = a} :: CreateDatasetImportJob)

instance Core.AWSRequest CreateDatasetImportJob where
  type
    AWSResponse CreateDatasetImportJob =
      CreateDatasetImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetImportJobResponse'
            Prelude.<$> (x Core..?> "DatasetImportJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetImportJob where
  hashWithSalt _salt CreateDatasetImportJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` timeZone
      `Prelude.hashWithSalt` useGeolocationForTimeZone
      `Prelude.hashWithSalt` timestampFormat
      `Prelude.hashWithSalt` geolocationFormat
      `Prelude.hashWithSalt` datasetImportJobName
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` dataSource

instance Prelude.NFData CreateDatasetImportJob where
  rnf CreateDatasetImportJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf timeZone
      `Prelude.seq` Prelude.rnf useGeolocationForTimeZone
      `Prelude.seq` Prelude.rnf timestampFormat
      `Prelude.seq` Prelude.rnf geolocationFormat
      `Prelude.seq` Prelude.rnf datasetImportJobName
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf dataSource

instance Core.ToHeaders CreateDatasetImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.CreateDatasetImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDatasetImportJob where
  toJSON CreateDatasetImportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Format" Core..=) Prelude.<$> format,
            ("TimeZone" Core..=) Prelude.<$> timeZone,
            ("UseGeolocationForTimeZone" Core..=)
              Prelude.<$> useGeolocationForTimeZone,
            ("TimestampFormat" Core..=)
              Prelude.<$> timestampFormat,
            ("GeolocationFormat" Core..=)
              Prelude.<$> geolocationFormat,
            Prelude.Just
              ( "DatasetImportJobName"
                  Core..= datasetImportJobName
              ),
            Prelude.Just ("DatasetArn" Core..= datasetArn),
            Prelude.Just ("DataSource" Core..= dataSource)
          ]
      )

instance Core.ToPath CreateDatasetImportJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDatasetImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetImportJobResponse' smart constructor.
data CreateDatasetImportJobResponse = CreateDatasetImportJobResponse'
  { -- | The Amazon Resource Name (ARN) of the dataset import job.
    datasetImportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetImportJobArn', 'createDatasetImportJobResponse_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job.
--
-- 'httpStatus', 'createDatasetImportJobResponse_httpStatus' - The response's http status code.
newCreateDatasetImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetImportJobResponse
newCreateDatasetImportJobResponse pHttpStatus_ =
  CreateDatasetImportJobResponse'
    { datasetImportJobArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset import job.
createDatasetImportJobResponse_datasetImportJobArn :: Lens.Lens' CreateDatasetImportJobResponse (Prelude.Maybe Prelude.Text)
createDatasetImportJobResponse_datasetImportJobArn = Lens.lens (\CreateDatasetImportJobResponse' {datasetImportJobArn} -> datasetImportJobArn) (\s@CreateDatasetImportJobResponse' {} a -> s {datasetImportJobArn = a} :: CreateDatasetImportJobResponse)

-- | The response's http status code.
createDatasetImportJobResponse_httpStatus :: Lens.Lens' CreateDatasetImportJobResponse Prelude.Int
createDatasetImportJobResponse_httpStatus = Lens.lens (\CreateDatasetImportJobResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetImportJobResponse' {} a -> s {httpStatus = a} :: CreateDatasetImportJobResponse)

instance
  Prelude.NFData
    CreateDatasetImportJobResponse
  where
  rnf CreateDatasetImportJobResponse' {..} =
    Prelude.rnf datasetImportJobArn
      `Prelude.seq` Prelude.rnf httpStatus
