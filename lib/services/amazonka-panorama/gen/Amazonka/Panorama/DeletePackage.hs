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
-- Module      : Amazonka.Panorama.DeletePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a package.
--
-- To delete a package, you need permission to call @s3:DeleteObject@ in
-- addition to permissions for the AWS Panorama API.
module Amazonka.Panorama.DeletePackage
  ( -- * Creating a Request
    DeletePackage (..),
    newDeletePackage,

    -- * Request Lenses
    deletePackage_forceDelete,
    deletePackage_packageId,

    -- * Destructuring the Response
    DeletePackageResponse (..),
    newDeletePackageResponse,

    -- * Response Lenses
    deletePackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePackage' smart constructor.
data DeletePackage = DeletePackage'
  { -- | Delete the package even if it has artifacts stored in its access point.
    -- Deletes the package\'s artifacts from Amazon S3.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The package\'s ID.
    packageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDelete', 'deletePackage_forceDelete' - Delete the package even if it has artifacts stored in its access point.
-- Deletes the package\'s artifacts from Amazon S3.
--
-- 'packageId', 'deletePackage_packageId' - The package\'s ID.
newDeletePackage ::
  -- | 'packageId'
  Prelude.Text ->
  DeletePackage
newDeletePackage pPackageId_ =
  DeletePackage'
    { forceDelete = Prelude.Nothing,
      packageId = pPackageId_
    }

-- | Delete the package even if it has artifacts stored in its access point.
-- Deletes the package\'s artifacts from Amazon S3.
deletePackage_forceDelete :: Lens.Lens' DeletePackage (Prelude.Maybe Prelude.Bool)
deletePackage_forceDelete = Lens.lens (\DeletePackage' {forceDelete} -> forceDelete) (\s@DeletePackage' {} a -> s {forceDelete = a} :: DeletePackage)

-- | The package\'s ID.
deletePackage_packageId :: Lens.Lens' DeletePackage Prelude.Text
deletePackage_packageId = Lens.lens (\DeletePackage' {packageId} -> packageId) (\s@DeletePackage' {} a -> s {packageId = a} :: DeletePackage)

instance Core.AWSRequest DeletePackage where
  type
    AWSResponse DeletePackage =
      DeletePackageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackage where
  hashWithSalt _salt DeletePackage' {..} =
    _salt
      `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` packageId

instance Prelude.NFData DeletePackage where
  rnf DeletePackage' {..} =
    Prelude.rnf forceDelete
      `Prelude.seq` Prelude.rnf packageId

instance Data.ToHeaders DeletePackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePackage where
  toPath DeletePackage' {..} =
    Prelude.mconcat ["/packages/", Data.toBS packageId]

instance Data.ToQuery DeletePackage where
  toQuery DeletePackage' {..} =
    Prelude.mconcat ["ForceDelete" Data.=: forceDelete]

-- | /See:/ 'newDeletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePackageResponse_httpStatus' - The response's http status code.
newDeletePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePackageResponse
newDeletePackageResponse pHttpStatus_ =
  DeletePackageResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePackageResponse_httpStatus :: Lens.Lens' DeletePackageResponse Prelude.Int
deletePackageResponse_httpStatus = Lens.lens (\DeletePackageResponse' {httpStatus} -> httpStatus) (\s@DeletePackageResponse' {} a -> s {httpStatus = a} :: DeletePackageResponse)

instance Prelude.NFData DeletePackageResponse where
  rnf DeletePackageResponse' {..} =
    Prelude.rnf httpStatus
