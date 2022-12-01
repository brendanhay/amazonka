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
-- Module      : Amazonka.LicenseManager.UpdateLicenseManagerReportGenerator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a report generator.
--
-- After you make changes to a report generator, it starts generating new
-- reports within 60 minutes of being updated.
module Amazonka.LicenseManager.UpdateLicenseManagerReportGenerator
  ( -- * Creating a Request
    UpdateLicenseManagerReportGenerator (..),
    newUpdateLicenseManagerReportGenerator,

    -- * Request Lenses
    updateLicenseManagerReportGenerator_description,
    updateLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,
    updateLicenseManagerReportGenerator_reportGeneratorName,
    updateLicenseManagerReportGenerator_type,
    updateLicenseManagerReportGenerator_reportContext,
    updateLicenseManagerReportGenerator_reportFrequency,
    updateLicenseManagerReportGenerator_clientToken,

    -- * Destructuring the Response
    UpdateLicenseManagerReportGeneratorResponse (..),
    newUpdateLicenseManagerReportGeneratorResponse,

    -- * Response Lenses
    updateLicenseManagerReportGeneratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLicenseManagerReportGenerator' smart constructor.
data UpdateLicenseManagerReportGenerator = UpdateLicenseManagerReportGenerator'
  { -- | Description of the report generator.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the report generator to update.
    licenseManagerReportGeneratorArn :: Prelude.Text,
    -- | Name of the report generator.
    reportGeneratorName :: Prelude.Text,
    -- | Type of reports to generate. The following report types are supported:
    --
    -- -   License configuration report - Reports the number and details of
    --     consumed licenses for a license configuration.
    --
    -- -   Resource report - Reports the tracked licenses and resource
    --     consumption for a license configuration.
    type' :: [ReportType],
    -- | The report context.
    reportContext :: ReportContext,
    -- | Frequency by which reports are generated.
    reportFrequency :: ReportFrequency,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLicenseManagerReportGenerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateLicenseManagerReportGenerator_description' - Description of the report generator.
--
-- 'licenseManagerReportGeneratorArn', 'updateLicenseManagerReportGenerator_licenseManagerReportGeneratorArn' - Amazon Resource Name (ARN) of the report generator to update.
--
-- 'reportGeneratorName', 'updateLicenseManagerReportGenerator_reportGeneratorName' - Name of the report generator.
--
-- 'type'', 'updateLicenseManagerReportGenerator_type' - Type of reports to generate. The following report types are supported:
--
-- -   License configuration report - Reports the number and details of
--     consumed licenses for a license configuration.
--
-- -   Resource report - Reports the tracked licenses and resource
--     consumption for a license configuration.
--
-- 'reportContext', 'updateLicenseManagerReportGenerator_reportContext' - The report context.
--
-- 'reportFrequency', 'updateLicenseManagerReportGenerator_reportFrequency' - Frequency by which reports are generated.
--
-- 'clientToken', 'updateLicenseManagerReportGenerator_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
newUpdateLicenseManagerReportGenerator ::
  -- | 'licenseManagerReportGeneratorArn'
  Prelude.Text ->
  -- | 'reportGeneratorName'
  Prelude.Text ->
  -- | 'reportContext'
  ReportContext ->
  -- | 'reportFrequency'
  ReportFrequency ->
  -- | 'clientToken'
  Prelude.Text ->
  UpdateLicenseManagerReportGenerator
newUpdateLicenseManagerReportGenerator
  pLicenseManagerReportGeneratorArn_
  pReportGeneratorName_
  pReportContext_
  pReportFrequency_
  pClientToken_ =
    UpdateLicenseManagerReportGenerator'
      { description =
          Prelude.Nothing,
        licenseManagerReportGeneratorArn =
          pLicenseManagerReportGeneratorArn_,
        reportGeneratorName =
          pReportGeneratorName_,
        type' = Prelude.mempty,
        reportContext = pReportContext_,
        reportFrequency = pReportFrequency_,
        clientToken = pClientToken_
      }

-- | Description of the report generator.
updateLicenseManagerReportGenerator_description :: Lens.Lens' UpdateLicenseManagerReportGenerator (Prelude.Maybe Prelude.Text)
updateLicenseManagerReportGenerator_description = Lens.lens (\UpdateLicenseManagerReportGenerator' {description} -> description) (\s@UpdateLicenseManagerReportGenerator' {} a -> s {description = a} :: UpdateLicenseManagerReportGenerator)

-- | Amazon Resource Name (ARN) of the report generator to update.
updateLicenseManagerReportGenerator_licenseManagerReportGeneratorArn :: Lens.Lens' UpdateLicenseManagerReportGenerator Prelude.Text
updateLicenseManagerReportGenerator_licenseManagerReportGeneratorArn = Lens.lens (\UpdateLicenseManagerReportGenerator' {licenseManagerReportGeneratorArn} -> licenseManagerReportGeneratorArn) (\s@UpdateLicenseManagerReportGenerator' {} a -> s {licenseManagerReportGeneratorArn = a} :: UpdateLicenseManagerReportGenerator)

-- | Name of the report generator.
updateLicenseManagerReportGenerator_reportGeneratorName :: Lens.Lens' UpdateLicenseManagerReportGenerator Prelude.Text
updateLicenseManagerReportGenerator_reportGeneratorName = Lens.lens (\UpdateLicenseManagerReportGenerator' {reportGeneratorName} -> reportGeneratorName) (\s@UpdateLicenseManagerReportGenerator' {} a -> s {reportGeneratorName = a} :: UpdateLicenseManagerReportGenerator)

-- | Type of reports to generate. The following report types are supported:
--
-- -   License configuration report - Reports the number and details of
--     consumed licenses for a license configuration.
--
-- -   Resource report - Reports the tracked licenses and resource
--     consumption for a license configuration.
updateLicenseManagerReportGenerator_type :: Lens.Lens' UpdateLicenseManagerReportGenerator [ReportType]
updateLicenseManagerReportGenerator_type = Lens.lens (\UpdateLicenseManagerReportGenerator' {type'} -> type') (\s@UpdateLicenseManagerReportGenerator' {} a -> s {type' = a} :: UpdateLicenseManagerReportGenerator) Prelude.. Lens.coerced

-- | The report context.
updateLicenseManagerReportGenerator_reportContext :: Lens.Lens' UpdateLicenseManagerReportGenerator ReportContext
updateLicenseManagerReportGenerator_reportContext = Lens.lens (\UpdateLicenseManagerReportGenerator' {reportContext} -> reportContext) (\s@UpdateLicenseManagerReportGenerator' {} a -> s {reportContext = a} :: UpdateLicenseManagerReportGenerator)

-- | Frequency by which reports are generated.
updateLicenseManagerReportGenerator_reportFrequency :: Lens.Lens' UpdateLicenseManagerReportGenerator ReportFrequency
updateLicenseManagerReportGenerator_reportFrequency = Lens.lens (\UpdateLicenseManagerReportGenerator' {reportFrequency} -> reportFrequency) (\s@UpdateLicenseManagerReportGenerator' {} a -> s {reportFrequency = a} :: UpdateLicenseManagerReportGenerator)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateLicenseManagerReportGenerator_clientToken :: Lens.Lens' UpdateLicenseManagerReportGenerator Prelude.Text
updateLicenseManagerReportGenerator_clientToken = Lens.lens (\UpdateLicenseManagerReportGenerator' {clientToken} -> clientToken) (\s@UpdateLicenseManagerReportGenerator' {} a -> s {clientToken = a} :: UpdateLicenseManagerReportGenerator)

instance
  Core.AWSRequest
    UpdateLicenseManagerReportGenerator
  where
  type
    AWSResponse UpdateLicenseManagerReportGenerator =
      UpdateLicenseManagerReportGeneratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLicenseManagerReportGeneratorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateLicenseManagerReportGenerator
  where
  hashWithSalt
    _salt
    UpdateLicenseManagerReportGenerator' {..} =
      _salt `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` licenseManagerReportGeneratorArn
        `Prelude.hashWithSalt` reportGeneratorName
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` reportContext
        `Prelude.hashWithSalt` reportFrequency
        `Prelude.hashWithSalt` clientToken

instance
  Prelude.NFData
    UpdateLicenseManagerReportGenerator
  where
  rnf UpdateLicenseManagerReportGenerator' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf licenseManagerReportGeneratorArn
      `Prelude.seq` Prelude.rnf reportGeneratorName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf reportContext
      `Prelude.seq` Prelude.rnf reportFrequency
      `Prelude.seq` Prelude.rnf clientToken

instance
  Core.ToHeaders
    UpdateLicenseManagerReportGenerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.UpdateLicenseManagerReportGenerator" ::
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
    UpdateLicenseManagerReportGenerator
  where
  toJSON UpdateLicenseManagerReportGenerator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            Prelude.Just
              ( "LicenseManagerReportGeneratorArn"
                  Core..= licenseManagerReportGeneratorArn
              ),
            Prelude.Just
              ("ReportGeneratorName" Core..= reportGeneratorName),
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("ReportContext" Core..= reportContext),
            Prelude.Just
              ("ReportFrequency" Core..= reportFrequency),
            Prelude.Just ("ClientToken" Core..= clientToken)
          ]
      )

instance
  Core.ToPath
    UpdateLicenseManagerReportGenerator
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateLicenseManagerReportGenerator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLicenseManagerReportGeneratorResponse' smart constructor.
data UpdateLicenseManagerReportGeneratorResponse = UpdateLicenseManagerReportGeneratorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLicenseManagerReportGeneratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLicenseManagerReportGeneratorResponse_httpStatus' - The response's http status code.
newUpdateLicenseManagerReportGeneratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLicenseManagerReportGeneratorResponse
newUpdateLicenseManagerReportGeneratorResponse
  pHttpStatus_ =
    UpdateLicenseManagerReportGeneratorResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateLicenseManagerReportGeneratorResponse_httpStatus :: Lens.Lens' UpdateLicenseManagerReportGeneratorResponse Prelude.Int
updateLicenseManagerReportGeneratorResponse_httpStatus = Lens.lens (\UpdateLicenseManagerReportGeneratorResponse' {httpStatus} -> httpStatus) (\s@UpdateLicenseManagerReportGeneratorResponse' {} a -> s {httpStatus = a} :: UpdateLicenseManagerReportGeneratorResponse)

instance
  Prelude.NFData
    UpdateLicenseManagerReportGeneratorResponse
  where
  rnf UpdateLicenseManagerReportGeneratorResponse' {..} =
    Prelude.rnf httpStatus
