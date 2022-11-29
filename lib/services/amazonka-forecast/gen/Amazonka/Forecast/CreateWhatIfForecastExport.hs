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
-- Module      : Amazonka.Forecast.CreateWhatIfForecastExport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a forecast created by the CreateWhatIfForecast operation to your
-- Amazon Simple Storage Service (Amazon S3) bucket. The forecast file name
-- will match the following conventions:
--
-- @≈\<ForecastExportJobName>_\<ExportTimestamp>_\<PartNumber>@
--
-- The \<ExportTimestamp> component is in Java SimpleDateFormat
-- (yyyy-MM-ddTHH-mm-ssZ).
--
-- You must specify a DataDestination object that includes an AWS Identity
-- and Access Management (IAM) role that Amazon Forecast can assume to
-- access the Amazon S3 bucket. For more information, see
-- aws-forecast-iam-roles.
--
-- For more information, see howitworks-forecast.
--
-- To get a list of all your what-if forecast export jobs, use the
-- ListWhatIfForecastExports operation.
--
-- The @Status@ of the forecast export job must be @ACTIVE@ before you can
-- access the forecast in your Amazon S3 bucket. To get the status, use the
-- DescribeWhatIfForecastExport operation.
module Amazonka.Forecast.CreateWhatIfForecastExport
  ( -- * Creating a Request
    CreateWhatIfForecastExport (..),
    newCreateWhatIfForecastExport,

    -- * Request Lenses
    createWhatIfForecastExport_tags,
    createWhatIfForecastExport_format,
    createWhatIfForecastExport_whatIfForecastExportName,
    createWhatIfForecastExport_whatIfForecastArns,
    createWhatIfForecastExport_destination,

    -- * Destructuring the Response
    CreateWhatIfForecastExportResponse (..),
    newCreateWhatIfForecastExportResponse,

    -- * Response Lenses
    createWhatIfForecastExportResponse_whatIfForecastExportArn,
    createWhatIfForecastExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWhatIfForecastExport' smart constructor.
data CreateWhatIfForecastExport = CreateWhatIfForecastExport'
  { -- | A list of
    -- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
    -- to apply to the what if forecast.
    tags :: Prelude.Maybe [Tag],
    -- | The format of the exported data, CSV or PARQUET.
    format :: Prelude.Maybe Prelude.Text,
    -- | The name of the what-if forecast to export.
    whatIfForecastExportName :: Prelude.Text,
    -- | The list of what-if forecast Amazon Resource Names (ARNs) to export.
    whatIfForecastArns :: Prelude.NonEmpty Prelude.Text,
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
-- Create a value of 'CreateWhatIfForecastExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWhatIfForecastExport_tags' - A list of
-- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
-- to apply to the what if forecast.
--
-- 'format', 'createWhatIfForecastExport_format' - The format of the exported data, CSV or PARQUET.
--
-- 'whatIfForecastExportName', 'createWhatIfForecastExport_whatIfForecastExportName' - The name of the what-if forecast to export.
--
-- 'whatIfForecastArns', 'createWhatIfForecastExport_whatIfForecastArns' - The list of what-if forecast Amazon Resource Names (ARNs) to export.
--
-- 'destination', 'createWhatIfForecastExport_destination' - The location where you want to save the forecast and an AWS Identity and
-- Access Management (IAM) role that Amazon Forecast can assume to access
-- the location. The forecast must be exported to an Amazon S3 bucket.
--
-- If encryption is used, @Destination@ must include an AWS Key Management
-- Service (KMS) key. The IAM role must allow Amazon Forecast permission to
-- access the key.
newCreateWhatIfForecastExport ::
  -- | 'whatIfForecastExportName'
  Prelude.Text ->
  -- | 'whatIfForecastArns'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'destination'
  DataDestination ->
  CreateWhatIfForecastExport
newCreateWhatIfForecastExport
  pWhatIfForecastExportName_
  pWhatIfForecastArns_
  pDestination_ =
    CreateWhatIfForecastExport'
      { tags = Prelude.Nothing,
        format = Prelude.Nothing,
        whatIfForecastExportName =
          pWhatIfForecastExportName_,
        whatIfForecastArns =
          Lens.coerced Lens.# pWhatIfForecastArns_,
        destination = pDestination_
      }

-- | A list of
-- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
-- to apply to the what if forecast.
createWhatIfForecastExport_tags :: Lens.Lens' CreateWhatIfForecastExport (Prelude.Maybe [Tag])
createWhatIfForecastExport_tags = Lens.lens (\CreateWhatIfForecastExport' {tags} -> tags) (\s@CreateWhatIfForecastExport' {} a -> s {tags = a} :: CreateWhatIfForecastExport) Prelude.. Lens.mapping Lens.coerced

-- | The format of the exported data, CSV or PARQUET.
createWhatIfForecastExport_format :: Lens.Lens' CreateWhatIfForecastExport (Prelude.Maybe Prelude.Text)
createWhatIfForecastExport_format = Lens.lens (\CreateWhatIfForecastExport' {format} -> format) (\s@CreateWhatIfForecastExport' {} a -> s {format = a} :: CreateWhatIfForecastExport)

-- | The name of the what-if forecast to export.
createWhatIfForecastExport_whatIfForecastExportName :: Lens.Lens' CreateWhatIfForecastExport Prelude.Text
createWhatIfForecastExport_whatIfForecastExportName = Lens.lens (\CreateWhatIfForecastExport' {whatIfForecastExportName} -> whatIfForecastExportName) (\s@CreateWhatIfForecastExport' {} a -> s {whatIfForecastExportName = a} :: CreateWhatIfForecastExport)

-- | The list of what-if forecast Amazon Resource Names (ARNs) to export.
createWhatIfForecastExport_whatIfForecastArns :: Lens.Lens' CreateWhatIfForecastExport (Prelude.NonEmpty Prelude.Text)
createWhatIfForecastExport_whatIfForecastArns = Lens.lens (\CreateWhatIfForecastExport' {whatIfForecastArns} -> whatIfForecastArns) (\s@CreateWhatIfForecastExport' {} a -> s {whatIfForecastArns = a} :: CreateWhatIfForecastExport) Prelude.. Lens.coerced

-- | The location where you want to save the forecast and an AWS Identity and
-- Access Management (IAM) role that Amazon Forecast can assume to access
-- the location. The forecast must be exported to an Amazon S3 bucket.
--
-- If encryption is used, @Destination@ must include an AWS Key Management
-- Service (KMS) key. The IAM role must allow Amazon Forecast permission to
-- access the key.
createWhatIfForecastExport_destination :: Lens.Lens' CreateWhatIfForecastExport DataDestination
createWhatIfForecastExport_destination = Lens.lens (\CreateWhatIfForecastExport' {destination} -> destination) (\s@CreateWhatIfForecastExport' {} a -> s {destination = a} :: CreateWhatIfForecastExport)

instance Core.AWSRequest CreateWhatIfForecastExport where
  type
    AWSResponse CreateWhatIfForecastExport =
      CreateWhatIfForecastExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWhatIfForecastExportResponse'
            Prelude.<$> (x Core..?> "WhatIfForecastExportArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWhatIfForecastExport where
  hashWithSalt _salt CreateWhatIfForecastExport' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` whatIfForecastExportName
      `Prelude.hashWithSalt` whatIfForecastArns
      `Prelude.hashWithSalt` destination

instance Prelude.NFData CreateWhatIfForecastExport where
  rnf CreateWhatIfForecastExport' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf whatIfForecastExportName
      `Prelude.seq` Prelude.rnf whatIfForecastArns
      `Prelude.seq` Prelude.rnf destination

instance Core.ToHeaders CreateWhatIfForecastExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.CreateWhatIfForecastExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWhatIfForecastExport where
  toJSON CreateWhatIfForecastExport' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Format" Core..=) Prelude.<$> format,
            Prelude.Just
              ( "WhatIfForecastExportName"
                  Core..= whatIfForecastExportName
              ),
            Prelude.Just
              ("WhatIfForecastArns" Core..= whatIfForecastArns),
            Prelude.Just ("Destination" Core..= destination)
          ]
      )

instance Core.ToPath CreateWhatIfForecastExport where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateWhatIfForecastExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWhatIfForecastExportResponse' smart constructor.
data CreateWhatIfForecastExportResponse = CreateWhatIfForecastExportResponse'
  { -- | The Amazon Resource Name (ARN) of the what-if forecast.
    whatIfForecastExportArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWhatIfForecastExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whatIfForecastExportArn', 'createWhatIfForecastExportResponse_whatIfForecastExportArn' - The Amazon Resource Name (ARN) of the what-if forecast.
--
-- 'httpStatus', 'createWhatIfForecastExportResponse_httpStatus' - The response's http status code.
newCreateWhatIfForecastExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWhatIfForecastExportResponse
newCreateWhatIfForecastExportResponse pHttpStatus_ =
  CreateWhatIfForecastExportResponse'
    { whatIfForecastExportArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the what-if forecast.
createWhatIfForecastExportResponse_whatIfForecastExportArn :: Lens.Lens' CreateWhatIfForecastExportResponse (Prelude.Maybe Prelude.Text)
createWhatIfForecastExportResponse_whatIfForecastExportArn = Lens.lens (\CreateWhatIfForecastExportResponse' {whatIfForecastExportArn} -> whatIfForecastExportArn) (\s@CreateWhatIfForecastExportResponse' {} a -> s {whatIfForecastExportArn = a} :: CreateWhatIfForecastExportResponse)

-- | The response's http status code.
createWhatIfForecastExportResponse_httpStatus :: Lens.Lens' CreateWhatIfForecastExportResponse Prelude.Int
createWhatIfForecastExportResponse_httpStatus = Lens.lens (\CreateWhatIfForecastExportResponse' {httpStatus} -> httpStatus) (\s@CreateWhatIfForecastExportResponse' {} a -> s {httpStatus = a} :: CreateWhatIfForecastExportResponse)

instance
  Prelude.NFData
    CreateWhatIfForecastExportResponse
  where
  rnf CreateWhatIfForecastExportResponse' {..} =
    Prelude.rnf whatIfForecastExportArn
      `Prelude.seq` Prelude.rnf httpStatus
