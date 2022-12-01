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
-- Module      : Amazonka.OpenSearch.DeletePackage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon OpenSearch Service package. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/custom-packages.html Custom packages for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DeletePackage
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
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes a package from OpenSearch Service. The package can\'t be
-- associated with any OpenSearch Service domain.
--
-- /See:/ 'newDeletePackage' smart constructor.
data DeletePackage = DeletePackage'
  { -- | The internal ID of the package you want to delete. Use
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
-- 'packageID', 'deletePackage_packageID' - The internal ID of the package you want to delete. Use
-- @DescribePackages@ to find this value.
newDeletePackage ::
  -- | 'packageID'
  Prelude.Text ->
  DeletePackage
newDeletePackage pPackageID_ =
  DeletePackage' {packageID = pPackageID_}

-- | The internal ID of the package you want to delete. Use
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
            Prelude.<$> (x Core..?> "PackageDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackage where
  hashWithSalt _salt DeletePackage' {..} =
    _salt `Prelude.hashWithSalt` packageID

instance Prelude.NFData DeletePackage where
  rnf DeletePackage' {..} = Prelude.rnf packageID

instance Core.ToHeaders DeletePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeletePackage where
  toPath DeletePackage' {..} =
    Prelude.mconcat
      ["/2021-01-01/packages/", Core.toBS packageID]

instance Core.ToQuery DeletePackage where
  toQuery = Prelude.const Prelude.mempty

-- | Container for the response parameters to the @DeletePackage@ operation.
--
-- /See:/ 'newDeletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { -- | Information about the deleted package.
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
-- 'packageDetails', 'deletePackageResponse_packageDetails' - Information about the deleted package.
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

-- | Information about the deleted package.
deletePackageResponse_packageDetails :: Lens.Lens' DeletePackageResponse (Prelude.Maybe PackageDetails)
deletePackageResponse_packageDetails = Lens.lens (\DeletePackageResponse' {packageDetails} -> packageDetails) (\s@DeletePackageResponse' {} a -> s {packageDetails = a} :: DeletePackageResponse)

-- | The response's http status code.
deletePackageResponse_httpStatus :: Lens.Lens' DeletePackageResponse Prelude.Int
deletePackageResponse_httpStatus = Lens.lens (\DeletePackageResponse' {httpStatus} -> httpStatus) (\s@DeletePackageResponse' {} a -> s {httpStatus = a} :: DeletePackageResponse)

instance Prelude.NFData DeletePackageResponse where
  rnf DeletePackageResponse' {..} =
    Prelude.rnf packageDetails
      `Prelude.seq` Prelude.rnf httpStatus
