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
-- Module      : Amazonka.Forecast.CreatePredictorBacktestExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports backtest forecasts and accuracy metrics generated by the
-- CreateAutoPredictor or CreatePredictor operations. Two folders
-- containing CSV or Parquet files are exported to your specified S3
-- bucket.
--
-- The export file names will match the following conventions:
--
-- @\<ExportJobName>_\<ExportTimestamp>_\<PartNumber>.csv@
--
-- The \<ExportTimestamp> component is in Java SimpleDate format
-- (yyyy-MM-ddTHH-mm-ssZ).
--
-- You must specify a DataDestination object that includes an Amazon S3
-- bucket and an AWS Identity and Access Management (IAM) role that Amazon
-- Forecast can assume to access the Amazon S3 bucket. For more
-- information, see aws-forecast-iam-roles.
--
-- The @Status@ of the export job must be @ACTIVE@ before you can access
-- the export in your Amazon S3 bucket. To get the status, use the
-- DescribePredictorBacktestExportJob operation.
module Amazonka.Forecast.CreatePredictorBacktestExportJob
  ( -- * Creating a Request
    CreatePredictorBacktestExportJob (..),
    newCreatePredictorBacktestExportJob,

    -- * Request Lenses
    createPredictorBacktestExportJob_format,
    createPredictorBacktestExportJob_tags,
    createPredictorBacktestExportJob_predictorBacktestExportJobName,
    createPredictorBacktestExportJob_predictorArn,
    createPredictorBacktestExportJob_destination,

    -- * Destructuring the Response
    CreatePredictorBacktestExportJobResponse (..),
    newCreatePredictorBacktestExportJobResponse,

    -- * Response Lenses
    createPredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    createPredictorBacktestExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePredictorBacktestExportJob' smart constructor.
data CreatePredictorBacktestExportJob = CreatePredictorBacktestExportJob'
  { -- | The format of the exported data, CSV or PARQUET. The default value is
    -- CSV.
    format :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata to help you categorize and organize your backtests.
    -- Each tag consists of a key and an optional value, both of which you
    -- define. Tag keys and values are case sensitive.
    --
    -- The following restrictions apply to tags:
    --
    -- -   For each resource, each tag key must be unique and each tag key must
    --     have one value.
    --
    -- -   Maximum number of tags per resource: 50.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8.
    --
    -- -   Accepted characters: all letters and numbers, spaces representable
    --     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
    --     across other services and resources, the character restrictions of
    --     those services also apply.
    --
    -- -   Key prefixes cannot include any upper or lowercase combination of
    --     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
    --     @aws@ as its prefix but the key does not, Forecast considers it to
    --     be a user tag and will count against the limit of 50 tags. Tags with
    --     only the key prefix of @aws@ do not count against your tags per
    --     resource limit. You cannot edit or delete tag keys with this prefix.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the backtest export job.
    predictorBacktestExportJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the predictor that you want to export.
    predictorArn :: Prelude.Text,
    destination :: DataDestination
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePredictorBacktestExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'createPredictorBacktestExportJob_format' - The format of the exported data, CSV or PARQUET. The default value is
-- CSV.
--
-- 'tags', 'createPredictorBacktestExportJob_tags' - Optional metadata to help you categorize and organize your backtests.
-- Each tag consists of a key and an optional value, both of which you
-- define. Tag keys and values are case sensitive.
--
-- The following restrictions apply to tags:
--
-- -   For each resource, each tag key must be unique and each tag key must
--     have one value.
--
-- -   Maximum number of tags per resource: 50.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Accepted characters: all letters and numbers, spaces representable
--     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
--     across other services and resources, the character restrictions of
--     those services also apply.
--
-- -   Key prefixes cannot include any upper or lowercase combination of
--     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
--     @aws@ as its prefix but the key does not, Forecast considers it to
--     be a user tag and will count against the limit of 50 tags. Tags with
--     only the key prefix of @aws@ do not count against your tags per
--     resource limit. You cannot edit or delete tag keys with this prefix.
--
-- 'predictorBacktestExportJobName', 'createPredictorBacktestExportJob_predictorBacktestExportJobName' - The name for the backtest export job.
--
-- 'predictorArn', 'createPredictorBacktestExportJob_predictorArn' - The Amazon Resource Name (ARN) of the predictor that you want to export.
--
-- 'destination', 'createPredictorBacktestExportJob_destination' - Undocumented member.
newCreatePredictorBacktestExportJob ::
  -- | 'predictorBacktestExportJobName'
  Prelude.Text ->
  -- | 'predictorArn'
  Prelude.Text ->
  -- | 'destination'
  DataDestination ->
  CreatePredictorBacktestExportJob
newCreatePredictorBacktestExportJob
  pPredictorBacktestExportJobName_
  pPredictorArn_
  pDestination_ =
    CreatePredictorBacktestExportJob'
      { format =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        predictorBacktestExportJobName =
          pPredictorBacktestExportJobName_,
        predictorArn = pPredictorArn_,
        destination = pDestination_
      }

-- | The format of the exported data, CSV or PARQUET. The default value is
-- CSV.
createPredictorBacktestExportJob_format :: Lens.Lens' CreatePredictorBacktestExportJob (Prelude.Maybe Prelude.Text)
createPredictorBacktestExportJob_format = Lens.lens (\CreatePredictorBacktestExportJob' {format} -> format) (\s@CreatePredictorBacktestExportJob' {} a -> s {format = a} :: CreatePredictorBacktestExportJob)

-- | Optional metadata to help you categorize and organize your backtests.
-- Each tag consists of a key and an optional value, both of which you
-- define. Tag keys and values are case sensitive.
--
-- The following restrictions apply to tags:
--
-- -   For each resource, each tag key must be unique and each tag key must
--     have one value.
--
-- -   Maximum number of tags per resource: 50.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Accepted characters: all letters and numbers, spaces representable
--     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
--     across other services and resources, the character restrictions of
--     those services also apply.
--
-- -   Key prefixes cannot include any upper or lowercase combination of
--     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
--     @aws@ as its prefix but the key does not, Forecast considers it to
--     be a user tag and will count against the limit of 50 tags. Tags with
--     only the key prefix of @aws@ do not count against your tags per
--     resource limit. You cannot edit or delete tag keys with this prefix.
createPredictorBacktestExportJob_tags :: Lens.Lens' CreatePredictorBacktestExportJob (Prelude.Maybe [Tag])
createPredictorBacktestExportJob_tags = Lens.lens (\CreatePredictorBacktestExportJob' {tags} -> tags) (\s@CreatePredictorBacktestExportJob' {} a -> s {tags = a} :: CreatePredictorBacktestExportJob) Prelude.. Lens.mapping Lens.coerced

-- | The name for the backtest export job.
createPredictorBacktestExportJob_predictorBacktestExportJobName :: Lens.Lens' CreatePredictorBacktestExportJob Prelude.Text
createPredictorBacktestExportJob_predictorBacktestExportJobName = Lens.lens (\CreatePredictorBacktestExportJob' {predictorBacktestExportJobName} -> predictorBacktestExportJobName) (\s@CreatePredictorBacktestExportJob' {} a -> s {predictorBacktestExportJobName = a} :: CreatePredictorBacktestExportJob)

-- | The Amazon Resource Name (ARN) of the predictor that you want to export.
createPredictorBacktestExportJob_predictorArn :: Lens.Lens' CreatePredictorBacktestExportJob Prelude.Text
createPredictorBacktestExportJob_predictorArn = Lens.lens (\CreatePredictorBacktestExportJob' {predictorArn} -> predictorArn) (\s@CreatePredictorBacktestExportJob' {} a -> s {predictorArn = a} :: CreatePredictorBacktestExportJob)

-- | Undocumented member.
createPredictorBacktestExportJob_destination :: Lens.Lens' CreatePredictorBacktestExportJob DataDestination
createPredictorBacktestExportJob_destination = Lens.lens (\CreatePredictorBacktestExportJob' {destination} -> destination) (\s@CreatePredictorBacktestExportJob' {} a -> s {destination = a} :: CreatePredictorBacktestExportJob)

instance
  Core.AWSRequest
    CreatePredictorBacktestExportJob
  where
  type
    AWSResponse CreatePredictorBacktestExportJob =
      CreatePredictorBacktestExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePredictorBacktestExportJobResponse'
            Prelude.<$> (x Data..?> "PredictorBacktestExportJobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreatePredictorBacktestExportJob
  where
  hashWithSalt
    _salt
    CreatePredictorBacktestExportJob' {..} =
      _salt
        `Prelude.hashWithSalt` format
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` predictorBacktestExportJobName
        `Prelude.hashWithSalt` predictorArn
        `Prelude.hashWithSalt` destination

instance
  Prelude.NFData
    CreatePredictorBacktestExportJob
  where
  rnf CreatePredictorBacktestExportJob' {..} =
    Prelude.rnf format `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf predictorBacktestExportJobName `Prelude.seq`
          Prelude.rnf predictorArn `Prelude.seq`
            Prelude.rnf destination

instance
  Data.ToHeaders
    CreatePredictorBacktestExportJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreatePredictorBacktestExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePredictorBacktestExportJob where
  toJSON CreatePredictorBacktestExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Format" Data..=) Prelude.<$> format,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "PredictorBacktestExportJobName"
                  Data..= predictorBacktestExportJobName
              ),
            Prelude.Just ("PredictorArn" Data..= predictorArn),
            Prelude.Just ("Destination" Data..= destination)
          ]
      )

instance Data.ToPath CreatePredictorBacktestExportJob where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreatePredictorBacktestExportJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePredictorBacktestExportJobResponse' smart constructor.
data CreatePredictorBacktestExportJobResponse = CreatePredictorBacktestExportJobResponse'
  { -- | The Amazon Resource Name (ARN) of the predictor backtest export job that
    -- you want to export.
    predictorBacktestExportJobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePredictorBacktestExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorBacktestExportJobArn', 'createPredictorBacktestExportJobResponse_predictorBacktestExportJobArn' - The Amazon Resource Name (ARN) of the predictor backtest export job that
-- you want to export.
--
-- 'httpStatus', 'createPredictorBacktestExportJobResponse_httpStatus' - The response's http status code.
newCreatePredictorBacktestExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePredictorBacktestExportJobResponse
newCreatePredictorBacktestExportJobResponse
  pHttpStatus_ =
    CreatePredictorBacktestExportJobResponse'
      { predictorBacktestExportJobArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the predictor backtest export job that
-- you want to export.
createPredictorBacktestExportJobResponse_predictorBacktestExportJobArn :: Lens.Lens' CreatePredictorBacktestExportJobResponse (Prelude.Maybe Prelude.Text)
createPredictorBacktestExportJobResponse_predictorBacktestExportJobArn = Lens.lens (\CreatePredictorBacktestExportJobResponse' {predictorBacktestExportJobArn} -> predictorBacktestExportJobArn) (\s@CreatePredictorBacktestExportJobResponse' {} a -> s {predictorBacktestExportJobArn = a} :: CreatePredictorBacktestExportJobResponse)

-- | The response's http status code.
createPredictorBacktestExportJobResponse_httpStatus :: Lens.Lens' CreatePredictorBacktestExportJobResponse Prelude.Int
createPredictorBacktestExportJobResponse_httpStatus = Lens.lens (\CreatePredictorBacktestExportJobResponse' {httpStatus} -> httpStatus) (\s@CreatePredictorBacktestExportJobResponse' {} a -> s {httpStatus = a} :: CreatePredictorBacktestExportJobResponse)

instance
  Prelude.NFData
    CreatePredictorBacktestExportJobResponse
  where
  rnf CreatePredictorBacktestExportJobResponse' {..} =
    Prelude.rnf predictorBacktestExportJobArn `Prelude.seq`
      Prelude.rnf httpStatus
