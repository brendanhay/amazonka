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
-- Module      : Amazonka.Forecast.CreateForecastExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a forecast created by the CreateForecast operation to your
-- Amazon Simple Storage Service (Amazon S3) bucket. The forecast file name
-- will match the following conventions:
--
-- \<ForecastExportJobName>_\<ExportTimestamp>_\<PartNumber>
--
-- where the \<ExportTimestamp> component is in Java SimpleDateFormat
-- (yyyy-MM-ddTHH-mm-ssZ).
--
-- You must specify a DataDestination object that includes an AWS Identity
-- and Access Management (IAM) role that Amazon Forecast can assume to
-- access the Amazon S3 bucket. For more information, see
-- aws-forecast-iam-roles.
--
-- For more information, see howitworks-forecast.
--
-- To get a list of all your forecast export jobs, use the
-- ListForecastExportJobs operation.
--
-- The @Status@ of the forecast export job must be @ACTIVE@ before you can
-- access the forecast in your Amazon S3 bucket. To get the status, use the
-- DescribeForecastExportJob operation.
module Amazonka.Forecast.CreateForecastExportJob
  ( -- * Creating a Request
    CreateForecastExportJob (..),
    newCreateForecastExportJob,

    -- * Request Lenses
    createForecastExportJob_format,
    createForecastExportJob_tags,
    createForecastExportJob_forecastExportJobName,
    createForecastExportJob_forecastArn,
    createForecastExportJob_destination,

    -- * Destructuring the Response
    CreateForecastExportJobResponse (..),
    newCreateForecastExportJobResponse,

    -- * Response Lenses
    createForecastExportJobResponse_forecastExportJobArn,
    createForecastExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateForecastExportJob' smart constructor.
data CreateForecastExportJob = CreateForecastExportJob'
  { -- | The format of the exported data, CSV or PARQUET. The default value is
    -- CSV.
    format :: Prelude.Maybe Prelude.Text,
    -- | The optional metadata that you apply to the forecast export job to help
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
    -- | The name for the forecast export job.
    forecastExportJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the forecast that you want to export.
    forecastArn :: Prelude.Text,
    -- | The location where you want to save the forecast and an AWS Identity and
    -- Access Management (IAM) role that Amazon Forecast can assume to access
    -- the location. The forecast must be exported to an Amazon S3 bucket.
    --
    -- If encryption is used, @Destination@ must include an AWS Key Management
    -- Service (KMS) key. The IAM role must allow Amazon Forecast permission to
    -- access the key.
    destination :: DataDestination
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateForecastExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'createForecastExportJob_format' - The format of the exported data, CSV or PARQUET. The default value is
-- CSV.
--
-- 'tags', 'createForecastExportJob_tags' - The optional metadata that you apply to the forecast export job to help
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
-- 'forecastExportJobName', 'createForecastExportJob_forecastExportJobName' - The name for the forecast export job.
--
-- 'forecastArn', 'createForecastExportJob_forecastArn' - The Amazon Resource Name (ARN) of the forecast that you want to export.
--
-- 'destination', 'createForecastExportJob_destination' - The location where you want to save the forecast and an AWS Identity and
-- Access Management (IAM) role that Amazon Forecast can assume to access
-- the location. The forecast must be exported to an Amazon S3 bucket.
--
-- If encryption is used, @Destination@ must include an AWS Key Management
-- Service (KMS) key. The IAM role must allow Amazon Forecast permission to
-- access the key.
newCreateForecastExportJob ::
  -- | 'forecastExportJobName'
  Prelude.Text ->
  -- | 'forecastArn'
  Prelude.Text ->
  -- | 'destination'
  DataDestination ->
  CreateForecastExportJob
newCreateForecastExportJob
  pForecastExportJobName_
  pForecastArn_
  pDestination_ =
    CreateForecastExportJob'
      { format = Prelude.Nothing,
        tags = Prelude.Nothing,
        forecastExportJobName = pForecastExportJobName_,
        forecastArn = pForecastArn_,
        destination = pDestination_
      }

-- | The format of the exported data, CSV or PARQUET. The default value is
-- CSV.
createForecastExportJob_format :: Lens.Lens' CreateForecastExportJob (Prelude.Maybe Prelude.Text)
createForecastExportJob_format = Lens.lens (\CreateForecastExportJob' {format} -> format) (\s@CreateForecastExportJob' {} a -> s {format = a} :: CreateForecastExportJob)

-- | The optional metadata that you apply to the forecast export job to help
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
createForecastExportJob_tags :: Lens.Lens' CreateForecastExportJob (Prelude.Maybe [Tag])
createForecastExportJob_tags = Lens.lens (\CreateForecastExportJob' {tags} -> tags) (\s@CreateForecastExportJob' {} a -> s {tags = a} :: CreateForecastExportJob) Prelude.. Lens.mapping Lens.coerced

-- | The name for the forecast export job.
createForecastExportJob_forecastExportJobName :: Lens.Lens' CreateForecastExportJob Prelude.Text
createForecastExportJob_forecastExportJobName = Lens.lens (\CreateForecastExportJob' {forecastExportJobName} -> forecastExportJobName) (\s@CreateForecastExportJob' {} a -> s {forecastExportJobName = a} :: CreateForecastExportJob)

-- | The Amazon Resource Name (ARN) of the forecast that you want to export.
createForecastExportJob_forecastArn :: Lens.Lens' CreateForecastExportJob Prelude.Text
createForecastExportJob_forecastArn = Lens.lens (\CreateForecastExportJob' {forecastArn} -> forecastArn) (\s@CreateForecastExportJob' {} a -> s {forecastArn = a} :: CreateForecastExportJob)

-- | The location where you want to save the forecast and an AWS Identity and
-- Access Management (IAM) role that Amazon Forecast can assume to access
-- the location. The forecast must be exported to an Amazon S3 bucket.
--
-- If encryption is used, @Destination@ must include an AWS Key Management
-- Service (KMS) key. The IAM role must allow Amazon Forecast permission to
-- access the key.
createForecastExportJob_destination :: Lens.Lens' CreateForecastExportJob DataDestination
createForecastExportJob_destination = Lens.lens (\CreateForecastExportJob' {destination} -> destination) (\s@CreateForecastExportJob' {} a -> s {destination = a} :: CreateForecastExportJob)

instance Core.AWSRequest CreateForecastExportJob where
  type
    AWSResponse CreateForecastExportJob =
      CreateForecastExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateForecastExportJobResponse'
            Prelude.<$> (x Data..?> "ForecastExportJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateForecastExportJob where
  hashWithSalt _salt CreateForecastExportJob' {..} =
    _salt `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` forecastExportJobName
      `Prelude.hashWithSalt` forecastArn
      `Prelude.hashWithSalt` destination

instance Prelude.NFData CreateForecastExportJob where
  rnf CreateForecastExportJob' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf forecastExportJobName
      `Prelude.seq` Prelude.rnf forecastArn
      `Prelude.seq` Prelude.rnf destination

instance Data.ToHeaders CreateForecastExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreateForecastExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateForecastExportJob where
  toJSON CreateForecastExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Format" Data..=) Prelude.<$> format,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "ForecastExportJobName"
                  Data..= forecastExportJobName
              ),
            Prelude.Just ("ForecastArn" Data..= forecastArn),
            Prelude.Just ("Destination" Data..= destination)
          ]
      )

instance Data.ToPath CreateForecastExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateForecastExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateForecastExportJobResponse' smart constructor.
data CreateForecastExportJobResponse = CreateForecastExportJobResponse'
  { -- | The Amazon Resource Name (ARN) of the export job.
    forecastExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateForecastExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastExportJobArn', 'createForecastExportJobResponse_forecastExportJobArn' - The Amazon Resource Name (ARN) of the export job.
--
-- 'httpStatus', 'createForecastExportJobResponse_httpStatus' - The response's http status code.
newCreateForecastExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateForecastExportJobResponse
newCreateForecastExportJobResponse pHttpStatus_ =
  CreateForecastExportJobResponse'
    { forecastExportJobArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the export job.
createForecastExportJobResponse_forecastExportJobArn :: Lens.Lens' CreateForecastExportJobResponse (Prelude.Maybe Prelude.Text)
createForecastExportJobResponse_forecastExportJobArn = Lens.lens (\CreateForecastExportJobResponse' {forecastExportJobArn} -> forecastExportJobArn) (\s@CreateForecastExportJobResponse' {} a -> s {forecastExportJobArn = a} :: CreateForecastExportJobResponse)

-- | The response's http status code.
createForecastExportJobResponse_httpStatus :: Lens.Lens' CreateForecastExportJobResponse Prelude.Int
createForecastExportJobResponse_httpStatus = Lens.lens (\CreateForecastExportJobResponse' {httpStatus} -> httpStatus) (\s@CreateForecastExportJobResponse' {} a -> s {httpStatus = a} :: CreateForecastExportJobResponse)

instance
  Prelude.NFData
    CreateForecastExportJobResponse
  where
  rnf CreateForecastExportJobResponse' {..} =
    Prelude.rnf forecastExportJobArn
      `Prelude.seq` Prelude.rnf httpStatus
