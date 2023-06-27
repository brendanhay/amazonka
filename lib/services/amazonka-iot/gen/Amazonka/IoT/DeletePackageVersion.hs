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
-- Module      : Amazonka.IoT.DeletePackageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version from a software package.
--
-- __Note:__ If a package version is designated as default, you must remove
-- the designation from the package using the UpdatePackage action.
module Amazonka.IoT.DeletePackageVersion
  ( -- * Creating a Request
    DeletePackageVersion (..),
    newDeletePackageVersion,

    -- * Request Lenses
    deletePackageVersion_clientToken,
    deletePackageVersion_packageName,
    deletePackageVersion_versionName,

    -- * Destructuring the Response
    DeletePackageVersionResponse (..),
    newDeletePackageVersionResponse,

    -- * Response Lenses
    deletePackageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePackageVersion' smart constructor.
data DeletePackageVersion = DeletePackageVersion'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated package.
    packageName :: Prelude.Text,
    -- | The name of the target package version.
    versionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deletePackageVersion_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'packageName', 'deletePackageVersion_packageName' - The name of the associated package.
--
-- 'versionName', 'deletePackageVersion_versionName' - The name of the target package version.
newDeletePackageVersion ::
  -- | 'packageName'
  Prelude.Text ->
  -- | 'versionName'
  Prelude.Text ->
  DeletePackageVersion
newDeletePackageVersion pPackageName_ pVersionName_ =
  DeletePackageVersion'
    { clientToken =
        Prelude.Nothing,
      packageName = pPackageName_,
      versionName = pVersionName_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
deletePackageVersion_clientToken :: Lens.Lens' DeletePackageVersion (Prelude.Maybe Prelude.Text)
deletePackageVersion_clientToken = Lens.lens (\DeletePackageVersion' {clientToken} -> clientToken) (\s@DeletePackageVersion' {} a -> s {clientToken = a} :: DeletePackageVersion)

-- | The name of the associated package.
deletePackageVersion_packageName :: Lens.Lens' DeletePackageVersion Prelude.Text
deletePackageVersion_packageName = Lens.lens (\DeletePackageVersion' {packageName} -> packageName) (\s@DeletePackageVersion' {} a -> s {packageName = a} :: DeletePackageVersion)

-- | The name of the target package version.
deletePackageVersion_versionName :: Lens.Lens' DeletePackageVersion Prelude.Text
deletePackageVersion_versionName = Lens.lens (\DeletePackageVersion' {versionName} -> versionName) (\s@DeletePackageVersion' {} a -> s {versionName = a} :: DeletePackageVersion)

instance Core.AWSRequest DeletePackageVersion where
  type
    AWSResponse DeletePackageVersion =
      DeletePackageVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePackageVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackageVersion where
  hashWithSalt _salt DeletePackageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` versionName

instance Prelude.NFData DeletePackageVersion where
  rnf DeletePackageVersion' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf versionName

instance Data.ToHeaders DeletePackageVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePackageVersion where
  toPath DeletePackageVersion' {..} =
    Prelude.mconcat
      [ "/packages/",
        Data.toBS packageName,
        "/versions/",
        Data.toBS versionName
      ]

instance Data.ToQuery DeletePackageVersion where
  toQuery DeletePackageVersion' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeletePackageVersionResponse' smart constructor.
data DeletePackageVersionResponse = DeletePackageVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePackageVersionResponse_httpStatus' - The response's http status code.
newDeletePackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePackageVersionResponse
newDeletePackageVersionResponse pHttpStatus_ =
  DeletePackageVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePackageVersionResponse_httpStatus :: Lens.Lens' DeletePackageVersionResponse Prelude.Int
deletePackageVersionResponse_httpStatus = Lens.lens (\DeletePackageVersionResponse' {httpStatus} -> httpStatus) (\s@DeletePackageVersionResponse' {} a -> s {httpStatus = a} :: DeletePackageVersionResponse)

instance Prelude.NFData DeletePackageVersionResponse where
  rnf DeletePackageVersionResponse' {..} =
    Prelude.rnf httpStatus
