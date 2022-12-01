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
-- Module      : Amazonka.LicenseManager.DeleteLicenseConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified license configuration.
--
-- You cannot delete a license configuration that is in use.
module Amazonka.LicenseManager.DeleteLicenseConfiguration
  ( -- * Creating a Request
    DeleteLicenseConfiguration (..),
    newDeleteLicenseConfiguration,

    -- * Request Lenses
    deleteLicenseConfiguration_licenseConfigurationArn,

    -- * Destructuring the Response
    DeleteLicenseConfigurationResponse (..),
    newDeleteLicenseConfigurationResponse,

    -- * Response Lenses
    deleteLicenseConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLicenseConfiguration' smart constructor.
data DeleteLicenseConfiguration = DeleteLicenseConfiguration'
  { -- | ID of the license configuration.
    licenseConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLicenseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'deleteLicenseConfiguration_licenseConfigurationArn' - ID of the license configuration.
newDeleteLicenseConfiguration ::
  -- | 'licenseConfigurationArn'
  Prelude.Text ->
  DeleteLicenseConfiguration
newDeleteLicenseConfiguration
  pLicenseConfigurationArn_ =
    DeleteLicenseConfiguration'
      { licenseConfigurationArn =
          pLicenseConfigurationArn_
      }

-- | ID of the license configuration.
deleteLicenseConfiguration_licenseConfigurationArn :: Lens.Lens' DeleteLicenseConfiguration Prelude.Text
deleteLicenseConfiguration_licenseConfigurationArn = Lens.lens (\DeleteLicenseConfiguration' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@DeleteLicenseConfiguration' {} a -> s {licenseConfigurationArn = a} :: DeleteLicenseConfiguration)

instance Core.AWSRequest DeleteLicenseConfiguration where
  type
    AWSResponse DeleteLicenseConfiguration =
      DeleteLicenseConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLicenseConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLicenseConfiguration where
  hashWithSalt _salt DeleteLicenseConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` licenseConfigurationArn

instance Prelude.NFData DeleteLicenseConfiguration where
  rnf DeleteLicenseConfiguration' {..} =
    Prelude.rnf licenseConfigurationArn

instance Core.ToHeaders DeleteLicenseConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.DeleteLicenseConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteLicenseConfiguration where
  toJSON DeleteLicenseConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LicenseConfigurationArn"
                  Core..= licenseConfigurationArn
              )
          ]
      )

instance Core.ToPath DeleteLicenseConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLicenseConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLicenseConfigurationResponse' smart constructor.
data DeleteLicenseConfigurationResponse = DeleteLicenseConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLicenseConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLicenseConfigurationResponse_httpStatus' - The response's http status code.
newDeleteLicenseConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLicenseConfigurationResponse
newDeleteLicenseConfigurationResponse pHttpStatus_ =
  DeleteLicenseConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLicenseConfigurationResponse_httpStatus :: Lens.Lens' DeleteLicenseConfigurationResponse Prelude.Int
deleteLicenseConfigurationResponse_httpStatus = Lens.lens (\DeleteLicenseConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteLicenseConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteLicenseConfigurationResponse)

instance
  Prelude.NFData
    DeleteLicenseConfigurationResponse
  where
  rnf DeleteLicenseConfigurationResponse' {..} =
    Prelude.rnf httpStatus
