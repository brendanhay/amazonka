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
-- Module      : Amazonka.LicenseManager.GetLicenseManagerReportGenerator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified report generator.
module Amazonka.LicenseManager.GetLicenseManagerReportGenerator
  ( -- * Creating a Request
    GetLicenseManagerReportGenerator (..),
    newGetLicenseManagerReportGenerator,

    -- * Request Lenses
    getLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,

    -- * Destructuring the Response
    GetLicenseManagerReportGeneratorResponse (..),
    newGetLicenseManagerReportGeneratorResponse,

    -- * Response Lenses
    getLicenseManagerReportGeneratorResponse_reportGenerator,
    getLicenseManagerReportGeneratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLicenseManagerReportGenerator' smart constructor.
data GetLicenseManagerReportGenerator = GetLicenseManagerReportGenerator'
  { -- | Amazon Resource Name (ARN) of the report generator.
    licenseManagerReportGeneratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLicenseManagerReportGenerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseManagerReportGeneratorArn', 'getLicenseManagerReportGenerator_licenseManagerReportGeneratorArn' - Amazon Resource Name (ARN) of the report generator.
newGetLicenseManagerReportGenerator ::
  -- | 'licenseManagerReportGeneratorArn'
  Prelude.Text ->
  GetLicenseManagerReportGenerator
newGetLicenseManagerReportGenerator
  pLicenseManagerReportGeneratorArn_ =
    GetLicenseManagerReportGenerator'
      { licenseManagerReportGeneratorArn =
          pLicenseManagerReportGeneratorArn_
      }

-- | Amazon Resource Name (ARN) of the report generator.
getLicenseManagerReportGenerator_licenseManagerReportGeneratorArn :: Lens.Lens' GetLicenseManagerReportGenerator Prelude.Text
getLicenseManagerReportGenerator_licenseManagerReportGeneratorArn = Lens.lens (\GetLicenseManagerReportGenerator' {licenseManagerReportGeneratorArn} -> licenseManagerReportGeneratorArn) (\s@GetLicenseManagerReportGenerator' {} a -> s {licenseManagerReportGeneratorArn = a} :: GetLicenseManagerReportGenerator)

instance
  Core.AWSRequest
    GetLicenseManagerReportGenerator
  where
  type
    AWSResponse GetLicenseManagerReportGenerator =
      GetLicenseManagerReportGeneratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLicenseManagerReportGeneratorResponse'
            Prelude.<$> (x Data..?> "ReportGenerator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetLicenseManagerReportGenerator
  where
  hashWithSalt
    _salt
    GetLicenseManagerReportGenerator' {..} =
      _salt
        `Prelude.hashWithSalt` licenseManagerReportGeneratorArn

instance
  Prelude.NFData
    GetLicenseManagerReportGenerator
  where
  rnf GetLicenseManagerReportGenerator' {..} =
    Prelude.rnf licenseManagerReportGeneratorArn

instance
  Data.ToHeaders
    GetLicenseManagerReportGenerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.GetLicenseManagerReportGenerator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLicenseManagerReportGenerator where
  toJSON GetLicenseManagerReportGenerator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LicenseManagerReportGeneratorArn"
                  Data..= licenseManagerReportGeneratorArn
              )
          ]
      )

instance Data.ToPath GetLicenseManagerReportGenerator where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetLicenseManagerReportGenerator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLicenseManagerReportGeneratorResponse' smart constructor.
data GetLicenseManagerReportGeneratorResponse = GetLicenseManagerReportGeneratorResponse'
  { -- | A report generator that creates periodic reports about your license
    -- configurations.
    reportGenerator :: Prelude.Maybe ReportGenerator,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLicenseManagerReportGeneratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportGenerator', 'getLicenseManagerReportGeneratorResponse_reportGenerator' - A report generator that creates periodic reports about your license
-- configurations.
--
-- 'httpStatus', 'getLicenseManagerReportGeneratorResponse_httpStatus' - The response's http status code.
newGetLicenseManagerReportGeneratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLicenseManagerReportGeneratorResponse
newGetLicenseManagerReportGeneratorResponse
  pHttpStatus_ =
    GetLicenseManagerReportGeneratorResponse'
      { reportGenerator =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A report generator that creates periodic reports about your license
-- configurations.
getLicenseManagerReportGeneratorResponse_reportGenerator :: Lens.Lens' GetLicenseManagerReportGeneratorResponse (Prelude.Maybe ReportGenerator)
getLicenseManagerReportGeneratorResponse_reportGenerator = Lens.lens (\GetLicenseManagerReportGeneratorResponse' {reportGenerator} -> reportGenerator) (\s@GetLicenseManagerReportGeneratorResponse' {} a -> s {reportGenerator = a} :: GetLicenseManagerReportGeneratorResponse)

-- | The response's http status code.
getLicenseManagerReportGeneratorResponse_httpStatus :: Lens.Lens' GetLicenseManagerReportGeneratorResponse Prelude.Int
getLicenseManagerReportGeneratorResponse_httpStatus = Lens.lens (\GetLicenseManagerReportGeneratorResponse' {httpStatus} -> httpStatus) (\s@GetLicenseManagerReportGeneratorResponse' {} a -> s {httpStatus = a} :: GetLicenseManagerReportGeneratorResponse)

instance
  Prelude.NFData
    GetLicenseManagerReportGeneratorResponse
  where
  rnf GetLicenseManagerReportGeneratorResponse' {..} =
    Prelude.rnf reportGenerator
      `Prelude.seq` Prelude.rnf httpStatus
