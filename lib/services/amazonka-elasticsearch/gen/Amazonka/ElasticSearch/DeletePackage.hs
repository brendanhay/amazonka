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
-- Module      : Amazonka.ElasticSearch.DeletePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the package.
module Amazonka.ElasticSearch.DeletePackage
  ( -- * Creating a Request
    DeletePackage (..),
    newDeletePackage,

    -- * Request Lenses
    deletePackage_packageID,

    -- * Destructuring the Response
    DeletePackageResponse (..),
    newDeletePackageResponse,

    -- * Response Lenses
    deletePackageResponse_packageDetails,
    deletePackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to @ DeletePackage @ operation.
--
-- /See:/ 'newDeletePackage' smart constructor.
data DeletePackage = DeletePackage'
  { -- | Internal ID of the package that you want to delete. Use
    -- @DescribePackages@ to find this value.
    packageID :: Prelude.Text
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
-- 'packageID', 'deletePackage_packageID' - Internal ID of the package that you want to delete. Use
-- @DescribePackages@ to find this value.
newDeletePackage ::
  -- | 'packageID'
  Prelude.Text ->
  DeletePackage
newDeletePackage pPackageID_ =
  DeletePackage' {packageID = pPackageID_}

-- | Internal ID of the package that you want to delete. Use
-- @DescribePackages@ to find this value.
deletePackage_packageID :: Lens.Lens' DeletePackage Prelude.Text
deletePackage_packageID = Lens.lens (\DeletePackage' {packageID} -> packageID) (\s@DeletePackage' {} a -> s {packageID = a} :: DeletePackage)

instance Core.AWSRequest DeletePackage where
  type
    AWSResponse DeletePackage =
      DeletePackageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePackageResponse'
            Prelude.<$> (x Data..?> "PackageDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackage where
  hashWithSalt _salt DeletePackage' {..} =
    _salt `Prelude.hashWithSalt` packageID

instance Prelude.NFData DeletePackage where
  rnf DeletePackage' {..} = Prelude.rnf packageID

instance Data.ToHeaders DeletePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePackage where
  toPath DeletePackage' {..} =
    Prelude.mconcat
      ["/2015-01-01/packages/", Data.toBS packageID]

instance Data.ToQuery DeletePackage where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response parameters to @ DeletePackage @ operation.
--
-- /See:/ 'newDeletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { -- | @PackageDetails@
    packageDetails :: Prelude.Maybe PackageDetails,
    -- | The response's http status code.
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
-- 'packageDetails', 'deletePackageResponse_packageDetails' - @PackageDetails@
--
-- 'httpStatus', 'deletePackageResponse_httpStatus' - The response's http status code.
newDeletePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePackageResponse
newDeletePackageResponse pHttpStatus_ =
  DeletePackageResponse'
    { packageDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | @PackageDetails@
deletePackageResponse_packageDetails :: Lens.Lens' DeletePackageResponse (Prelude.Maybe PackageDetails)
deletePackageResponse_packageDetails = Lens.lens (\DeletePackageResponse' {packageDetails} -> packageDetails) (\s@DeletePackageResponse' {} a -> s {packageDetails = a} :: DeletePackageResponse)

-- | The response's http status code.
deletePackageResponse_httpStatus :: Lens.Lens' DeletePackageResponse Prelude.Int
deletePackageResponse_httpStatus = Lens.lens (\DeletePackageResponse' {httpStatus} -> httpStatus) (\s@DeletePackageResponse' {} a -> s {httpStatus = a} :: DeletePackageResponse)

instance Prelude.NFData DeletePackageResponse where
  rnf DeletePackageResponse' {..} =
    Prelude.rnf packageDetails
      `Prelude.seq` Prelude.rnf httpStatus
