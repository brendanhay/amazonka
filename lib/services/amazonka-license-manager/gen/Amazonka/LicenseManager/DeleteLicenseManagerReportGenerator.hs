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
-- Module      : Amazonka.LicenseManager.DeleteLicenseManagerReportGenerator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified report generator.
--
-- This action deletes the report generator, which stops it from generating
-- future reports. The action cannot be reversed. It has no effect on the
-- previous reports from this generator.
module Amazonka.LicenseManager.DeleteLicenseManagerReportGenerator
  ( -- * Creating a Request
    DeleteLicenseManagerReportGenerator (..),
    newDeleteLicenseManagerReportGenerator,

    -- * Request Lenses
    deleteLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,

    -- * Destructuring the Response
    DeleteLicenseManagerReportGeneratorResponse (..),
    newDeleteLicenseManagerReportGeneratorResponse,

    -- * Response Lenses
    deleteLicenseManagerReportGeneratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLicenseManagerReportGenerator' smart constructor.
data DeleteLicenseManagerReportGenerator = DeleteLicenseManagerReportGenerator'
  { -- | Amazon Resource Name (ARN) of the report generator to be deleted.
    licenseManagerReportGeneratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLicenseManagerReportGenerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseManagerReportGeneratorArn', 'deleteLicenseManagerReportGenerator_licenseManagerReportGeneratorArn' - Amazon Resource Name (ARN) of the report generator to be deleted.
newDeleteLicenseManagerReportGenerator ::
  -- | 'licenseManagerReportGeneratorArn'
  Prelude.Text ->
  DeleteLicenseManagerReportGenerator
newDeleteLicenseManagerReportGenerator
  pLicenseManagerReportGeneratorArn_ =
    DeleteLicenseManagerReportGenerator'
      { licenseManagerReportGeneratorArn =
          pLicenseManagerReportGeneratorArn_
      }

-- | Amazon Resource Name (ARN) of the report generator to be deleted.
deleteLicenseManagerReportGenerator_licenseManagerReportGeneratorArn :: Lens.Lens' DeleteLicenseManagerReportGenerator Prelude.Text
deleteLicenseManagerReportGenerator_licenseManagerReportGeneratorArn = Lens.lens (\DeleteLicenseManagerReportGenerator' {licenseManagerReportGeneratorArn} -> licenseManagerReportGeneratorArn) (\s@DeleteLicenseManagerReportGenerator' {} a -> s {licenseManagerReportGeneratorArn = a} :: DeleteLicenseManagerReportGenerator)

instance
  Core.AWSRequest
    DeleteLicenseManagerReportGenerator
  where
  type
    AWSResponse DeleteLicenseManagerReportGenerator =
      DeleteLicenseManagerReportGeneratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLicenseManagerReportGeneratorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteLicenseManagerReportGenerator
  where
  hashWithSalt
    _salt
    DeleteLicenseManagerReportGenerator' {..} =
      _salt
        `Prelude.hashWithSalt` licenseManagerReportGeneratorArn

instance
  Prelude.NFData
    DeleteLicenseManagerReportGenerator
  where
  rnf DeleteLicenseManagerReportGenerator' {..} =
    Prelude.rnf licenseManagerReportGeneratorArn

instance
  Core.ToHeaders
    DeleteLicenseManagerReportGenerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.DeleteLicenseManagerReportGenerator" ::
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
    DeleteLicenseManagerReportGenerator
  where
  toJSON DeleteLicenseManagerReportGenerator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LicenseManagerReportGeneratorArn"
                  Core..= licenseManagerReportGeneratorArn
              )
          ]
      )

instance
  Core.ToPath
    DeleteLicenseManagerReportGenerator
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteLicenseManagerReportGenerator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLicenseManagerReportGeneratorResponse' smart constructor.
data DeleteLicenseManagerReportGeneratorResponse = DeleteLicenseManagerReportGeneratorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLicenseManagerReportGeneratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLicenseManagerReportGeneratorResponse_httpStatus' - The response's http status code.
newDeleteLicenseManagerReportGeneratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLicenseManagerReportGeneratorResponse
newDeleteLicenseManagerReportGeneratorResponse
  pHttpStatus_ =
    DeleteLicenseManagerReportGeneratorResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteLicenseManagerReportGeneratorResponse_httpStatus :: Lens.Lens' DeleteLicenseManagerReportGeneratorResponse Prelude.Int
deleteLicenseManagerReportGeneratorResponse_httpStatus = Lens.lens (\DeleteLicenseManagerReportGeneratorResponse' {httpStatus} -> httpStatus) (\s@DeleteLicenseManagerReportGeneratorResponse' {} a -> s {httpStatus = a} :: DeleteLicenseManagerReportGeneratorResponse)

instance
  Prelude.NFData
    DeleteLicenseManagerReportGeneratorResponse
  where
  rnf DeleteLicenseManagerReportGeneratorResponse' {..} =
    Prelude.rnf httpStatus
