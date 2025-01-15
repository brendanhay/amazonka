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
-- Module      : Amazonka.LicenseManager.DeleteLicense
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified license.
module Amazonka.LicenseManager.DeleteLicense
  ( -- * Creating a Request
    DeleteLicense (..),
    newDeleteLicense,

    -- * Request Lenses
    deleteLicense_licenseArn,
    deleteLicense_sourceVersion,

    -- * Destructuring the Response
    DeleteLicenseResponse (..),
    newDeleteLicenseResponse,

    -- * Response Lenses
    deleteLicenseResponse_deletionDate,
    deleteLicenseResponse_status,
    deleteLicenseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLicense' smart constructor.
data DeleteLicense = DeleteLicense'
  { -- | Amazon Resource Name (ARN) of the license.
    licenseArn :: Prelude.Text,
    -- | Current version of the license.
    sourceVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLicense' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseArn', 'deleteLicense_licenseArn' - Amazon Resource Name (ARN) of the license.
--
-- 'sourceVersion', 'deleteLicense_sourceVersion' - Current version of the license.
newDeleteLicense ::
  -- | 'licenseArn'
  Prelude.Text ->
  -- | 'sourceVersion'
  Prelude.Text ->
  DeleteLicense
newDeleteLicense pLicenseArn_ pSourceVersion_ =
  DeleteLicense'
    { licenseArn = pLicenseArn_,
      sourceVersion = pSourceVersion_
    }

-- | Amazon Resource Name (ARN) of the license.
deleteLicense_licenseArn :: Lens.Lens' DeleteLicense Prelude.Text
deleteLicense_licenseArn = Lens.lens (\DeleteLicense' {licenseArn} -> licenseArn) (\s@DeleteLicense' {} a -> s {licenseArn = a} :: DeleteLicense)

-- | Current version of the license.
deleteLicense_sourceVersion :: Lens.Lens' DeleteLicense Prelude.Text
deleteLicense_sourceVersion = Lens.lens (\DeleteLicense' {sourceVersion} -> sourceVersion) (\s@DeleteLicense' {} a -> s {sourceVersion = a} :: DeleteLicense)

instance Core.AWSRequest DeleteLicense where
  type
    AWSResponse DeleteLicense =
      DeleteLicenseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLicenseResponse'
            Prelude.<$> (x Data..?> "DeletionDate")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLicense where
  hashWithSalt _salt DeleteLicense' {..} =
    _salt
      `Prelude.hashWithSalt` licenseArn
      `Prelude.hashWithSalt` sourceVersion

instance Prelude.NFData DeleteLicense where
  rnf DeleteLicense' {..} =
    Prelude.rnf licenseArn `Prelude.seq`
      Prelude.rnf sourceVersion

instance Data.ToHeaders DeleteLicense where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.DeleteLicense" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLicense where
  toJSON DeleteLicense' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("LicenseArn" Data..= licenseArn),
            Prelude.Just
              ("SourceVersion" Data..= sourceVersion)
          ]
      )

instance Data.ToPath DeleteLicense where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLicense where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLicenseResponse' smart constructor.
data DeleteLicenseResponse = DeleteLicenseResponse'
  { -- | Date when the license is deleted.
    deletionDate :: Prelude.Maybe Prelude.Text,
    -- | License status.
    status :: Prelude.Maybe LicenseDeletionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLicenseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionDate', 'deleteLicenseResponse_deletionDate' - Date when the license is deleted.
--
-- 'status', 'deleteLicenseResponse_status' - License status.
--
-- 'httpStatus', 'deleteLicenseResponse_httpStatus' - The response's http status code.
newDeleteLicenseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLicenseResponse
newDeleteLicenseResponse pHttpStatus_ =
  DeleteLicenseResponse'
    { deletionDate =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Date when the license is deleted.
deleteLicenseResponse_deletionDate :: Lens.Lens' DeleteLicenseResponse (Prelude.Maybe Prelude.Text)
deleteLicenseResponse_deletionDate = Lens.lens (\DeleteLicenseResponse' {deletionDate} -> deletionDate) (\s@DeleteLicenseResponse' {} a -> s {deletionDate = a} :: DeleteLicenseResponse)

-- | License status.
deleteLicenseResponse_status :: Lens.Lens' DeleteLicenseResponse (Prelude.Maybe LicenseDeletionStatus)
deleteLicenseResponse_status = Lens.lens (\DeleteLicenseResponse' {status} -> status) (\s@DeleteLicenseResponse' {} a -> s {status = a} :: DeleteLicenseResponse)

-- | The response's http status code.
deleteLicenseResponse_httpStatus :: Lens.Lens' DeleteLicenseResponse Prelude.Int
deleteLicenseResponse_httpStatus = Lens.lens (\DeleteLicenseResponse' {httpStatus} -> httpStatus) (\s@DeleteLicenseResponse' {} a -> s {httpStatus = a} :: DeleteLicenseResponse)

instance Prelude.NFData DeleteLicenseResponse where
  rnf DeleteLicenseResponse' {..} =
    Prelude.rnf deletionDate `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf httpStatus
