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
-- Module      : Amazonka.LicenseManager.CreateLicenseManagerReportGenerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a report generator.
module Amazonka.LicenseManager.CreateLicenseManagerReportGenerator
  ( -- * Creating a Request
    CreateLicenseManagerReportGenerator (..),
    newCreateLicenseManagerReportGenerator,

    -- * Request Lenses
    createLicenseManagerReportGenerator_description,
    createLicenseManagerReportGenerator_tags,
    createLicenseManagerReportGenerator_reportGeneratorName,
    createLicenseManagerReportGenerator_type,
    createLicenseManagerReportGenerator_reportContext,
    createLicenseManagerReportGenerator_reportFrequency,
    createLicenseManagerReportGenerator_clientToken,

    -- * Destructuring the Response
    CreateLicenseManagerReportGeneratorResponse (..),
    newCreateLicenseManagerReportGeneratorResponse,

    -- * Response Lenses
    createLicenseManagerReportGeneratorResponse_licenseManagerReportGeneratorArn,
    createLicenseManagerReportGeneratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLicenseManagerReportGenerator' smart constructor.
data CreateLicenseManagerReportGenerator = CreateLicenseManagerReportGenerator'
  { -- | Description of the report generator.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags to add to the report generator.
    tags :: Prelude.Maybe [Tag],
    -- | Name of the report generator.
    reportGeneratorName :: Prelude.Text,
    -- | Type of reports to generate. The following report types an be generated:
    --
    -- -   License configuration report - Reports the number and details of
    --     consumed licenses for a license configuration.
    --
    -- -   Resource report - Reports the tracked licenses and resource
    --     consumption for a license configuration.
    type' :: [ReportType],
    -- | Defines the type of license configuration the report generator tracks.
    reportContext :: ReportContext,
    -- | Frequency by which reports are generated. Reports can be generated
    -- daily, monthly, or weekly.
    reportFrequency :: ReportFrequency,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLicenseManagerReportGenerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createLicenseManagerReportGenerator_description' - Description of the report generator.
--
-- 'tags', 'createLicenseManagerReportGenerator_tags' - Tags to add to the report generator.
--
-- 'reportGeneratorName', 'createLicenseManagerReportGenerator_reportGeneratorName' - Name of the report generator.
--
-- 'type'', 'createLicenseManagerReportGenerator_type' - Type of reports to generate. The following report types an be generated:
--
-- -   License configuration report - Reports the number and details of
--     consumed licenses for a license configuration.
--
-- -   Resource report - Reports the tracked licenses and resource
--     consumption for a license configuration.
--
-- 'reportContext', 'createLicenseManagerReportGenerator_reportContext' - Defines the type of license configuration the report generator tracks.
--
-- 'reportFrequency', 'createLicenseManagerReportGenerator_reportFrequency' - Frequency by which reports are generated. Reports can be generated
-- daily, monthly, or weekly.
--
-- 'clientToken', 'createLicenseManagerReportGenerator_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
newCreateLicenseManagerReportGenerator ::
  -- | 'reportGeneratorName'
  Prelude.Text ->
  -- | 'reportContext'
  ReportContext ->
  -- | 'reportFrequency'
  ReportFrequency ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateLicenseManagerReportGenerator
newCreateLicenseManagerReportGenerator
  pReportGeneratorName_
  pReportContext_
  pReportFrequency_
  pClientToken_ =
    CreateLicenseManagerReportGenerator'
      { description =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        reportGeneratorName =
          pReportGeneratorName_,
        type' = Prelude.mempty,
        reportContext = pReportContext_,
        reportFrequency = pReportFrequency_,
        clientToken = pClientToken_
      }

-- | Description of the report generator.
createLicenseManagerReportGenerator_description :: Lens.Lens' CreateLicenseManagerReportGenerator (Prelude.Maybe Prelude.Text)
createLicenseManagerReportGenerator_description = Lens.lens (\CreateLicenseManagerReportGenerator' {description} -> description) (\s@CreateLicenseManagerReportGenerator' {} a -> s {description = a} :: CreateLicenseManagerReportGenerator)

-- | Tags to add to the report generator.
createLicenseManagerReportGenerator_tags :: Lens.Lens' CreateLicenseManagerReportGenerator (Prelude.Maybe [Tag])
createLicenseManagerReportGenerator_tags = Lens.lens (\CreateLicenseManagerReportGenerator' {tags} -> tags) (\s@CreateLicenseManagerReportGenerator' {} a -> s {tags = a} :: CreateLicenseManagerReportGenerator) Prelude.. Lens.mapping Lens.coerced

-- | Name of the report generator.
createLicenseManagerReportGenerator_reportGeneratorName :: Lens.Lens' CreateLicenseManagerReportGenerator Prelude.Text
createLicenseManagerReportGenerator_reportGeneratorName = Lens.lens (\CreateLicenseManagerReportGenerator' {reportGeneratorName} -> reportGeneratorName) (\s@CreateLicenseManagerReportGenerator' {} a -> s {reportGeneratorName = a} :: CreateLicenseManagerReportGenerator)

-- | Type of reports to generate. The following report types an be generated:
--
-- -   License configuration report - Reports the number and details of
--     consumed licenses for a license configuration.
--
-- -   Resource report - Reports the tracked licenses and resource
--     consumption for a license configuration.
createLicenseManagerReportGenerator_type :: Lens.Lens' CreateLicenseManagerReportGenerator [ReportType]
createLicenseManagerReportGenerator_type = Lens.lens (\CreateLicenseManagerReportGenerator' {type'} -> type') (\s@CreateLicenseManagerReportGenerator' {} a -> s {type' = a} :: CreateLicenseManagerReportGenerator) Prelude.. Lens.coerced

-- | Defines the type of license configuration the report generator tracks.
createLicenseManagerReportGenerator_reportContext :: Lens.Lens' CreateLicenseManagerReportGenerator ReportContext
createLicenseManagerReportGenerator_reportContext = Lens.lens (\CreateLicenseManagerReportGenerator' {reportContext} -> reportContext) (\s@CreateLicenseManagerReportGenerator' {} a -> s {reportContext = a} :: CreateLicenseManagerReportGenerator)

-- | Frequency by which reports are generated. Reports can be generated
-- daily, monthly, or weekly.
createLicenseManagerReportGenerator_reportFrequency :: Lens.Lens' CreateLicenseManagerReportGenerator ReportFrequency
createLicenseManagerReportGenerator_reportFrequency = Lens.lens (\CreateLicenseManagerReportGenerator' {reportFrequency} -> reportFrequency) (\s@CreateLicenseManagerReportGenerator' {} a -> s {reportFrequency = a} :: CreateLicenseManagerReportGenerator)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createLicenseManagerReportGenerator_clientToken :: Lens.Lens' CreateLicenseManagerReportGenerator Prelude.Text
createLicenseManagerReportGenerator_clientToken = Lens.lens (\CreateLicenseManagerReportGenerator' {clientToken} -> clientToken) (\s@CreateLicenseManagerReportGenerator' {} a -> s {clientToken = a} :: CreateLicenseManagerReportGenerator)

instance
  Core.AWSRequest
    CreateLicenseManagerReportGenerator
  where
  type
    AWSResponse CreateLicenseManagerReportGenerator =
      CreateLicenseManagerReportGeneratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLicenseManagerReportGeneratorResponse'
            Prelude.<$> (x Data..?> "LicenseManagerReportGeneratorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateLicenseManagerReportGenerator
  where
  hashWithSalt
    _salt
    CreateLicenseManagerReportGenerator' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` reportGeneratorName
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` reportContext
        `Prelude.hashWithSalt` reportFrequency
        `Prelude.hashWithSalt` clientToken

instance
  Prelude.NFData
    CreateLicenseManagerReportGenerator
  where
  rnf CreateLicenseManagerReportGenerator' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf reportGeneratorName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf reportContext
      `Prelude.seq` Prelude.rnf reportFrequency
      `Prelude.seq` Prelude.rnf clientToken

instance
  Data.ToHeaders
    CreateLicenseManagerReportGenerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.CreateLicenseManagerReportGenerator" ::
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
    CreateLicenseManagerReportGenerator
  where
  toJSON CreateLicenseManagerReportGenerator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ReportGeneratorName" Data..= reportGeneratorName),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("ReportContext" Data..= reportContext),
            Prelude.Just
              ("ReportFrequency" Data..= reportFrequency),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance
  Data.ToPath
    CreateLicenseManagerReportGenerator
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateLicenseManagerReportGenerator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLicenseManagerReportGeneratorResponse' smart constructor.
data CreateLicenseManagerReportGeneratorResponse = CreateLicenseManagerReportGeneratorResponse'
  { -- | The Amazon Resource Name (ARN) of the new report generator.
    licenseManagerReportGeneratorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLicenseManagerReportGeneratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseManagerReportGeneratorArn', 'createLicenseManagerReportGeneratorResponse_licenseManagerReportGeneratorArn' - The Amazon Resource Name (ARN) of the new report generator.
--
-- 'httpStatus', 'createLicenseManagerReportGeneratorResponse_httpStatus' - The response's http status code.
newCreateLicenseManagerReportGeneratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLicenseManagerReportGeneratorResponse
newCreateLicenseManagerReportGeneratorResponse
  pHttpStatus_ =
    CreateLicenseManagerReportGeneratorResponse'
      { licenseManagerReportGeneratorArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the new report generator.
createLicenseManagerReportGeneratorResponse_licenseManagerReportGeneratorArn :: Lens.Lens' CreateLicenseManagerReportGeneratorResponse (Prelude.Maybe Prelude.Text)
createLicenseManagerReportGeneratorResponse_licenseManagerReportGeneratorArn = Lens.lens (\CreateLicenseManagerReportGeneratorResponse' {licenseManagerReportGeneratorArn} -> licenseManagerReportGeneratorArn) (\s@CreateLicenseManagerReportGeneratorResponse' {} a -> s {licenseManagerReportGeneratorArn = a} :: CreateLicenseManagerReportGeneratorResponse)

-- | The response's http status code.
createLicenseManagerReportGeneratorResponse_httpStatus :: Lens.Lens' CreateLicenseManagerReportGeneratorResponse Prelude.Int
createLicenseManagerReportGeneratorResponse_httpStatus = Lens.lens (\CreateLicenseManagerReportGeneratorResponse' {httpStatus} -> httpStatus) (\s@CreateLicenseManagerReportGeneratorResponse' {} a -> s {httpStatus = a} :: CreateLicenseManagerReportGeneratorResponse)

instance
  Prelude.NFData
    CreateLicenseManagerReportGeneratorResponse
  where
  rnf CreateLicenseManagerReportGeneratorResponse' {..} =
    Prelude.rnf licenseManagerReportGeneratorArn
      `Prelude.seq` Prelude.rnf httpStatus
