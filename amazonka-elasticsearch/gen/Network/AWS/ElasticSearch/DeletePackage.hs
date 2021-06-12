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
-- Module      : Network.AWS.ElasticSearch.DeletePackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the package.
module Network.AWS.ElasticSearch.DeletePackage
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ DeletePackage @ operation.
--
-- /See:/ 'newDeletePackage' smart constructor.
data DeletePackage = DeletePackage'
  { -- | Internal ID of the package that you want to delete. Use
    -- @DescribePackages@ to find this value.
    packageID :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeletePackage
newDeletePackage pPackageID_ =
  DeletePackage' {packageID = pPackageID_}

-- | Internal ID of the package that you want to delete. Use
-- @DescribePackages@ to find this value.
deletePackage_packageID :: Lens.Lens' DeletePackage Core.Text
deletePackage_packageID = Lens.lens (\DeletePackage' {packageID} -> packageID) (\s@DeletePackage' {} a -> s {packageID = a} :: DeletePackage)

instance Core.AWSRequest DeletePackage where
  type
    AWSResponse DeletePackage =
      DeletePackageResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePackageResponse'
            Core.<$> (x Core..?> "PackageDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeletePackage

instance Core.NFData DeletePackage

instance Core.ToHeaders DeletePackage where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeletePackage where
  toPath DeletePackage' {..} =
    Core.mconcat
      ["/2015-01-01/packages/", Core.toBS packageID]

instance Core.ToQuery DeletePackage where
  toQuery = Core.const Core.mempty

-- | Container for response parameters to @ DeletePackage @ operation.
--
-- /See:/ 'newDeletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { -- | @PackageDetails@
    packageDetails :: Core.Maybe PackageDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeletePackageResponse
newDeletePackageResponse pHttpStatus_ =
  DeletePackageResponse'
    { packageDetails =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | @PackageDetails@
deletePackageResponse_packageDetails :: Lens.Lens' DeletePackageResponse (Core.Maybe PackageDetails)
deletePackageResponse_packageDetails = Lens.lens (\DeletePackageResponse' {packageDetails} -> packageDetails) (\s@DeletePackageResponse' {} a -> s {packageDetails = a} :: DeletePackageResponse)

-- | The response's http status code.
deletePackageResponse_httpStatus :: Lens.Lens' DeletePackageResponse Core.Int
deletePackageResponse_httpStatus = Lens.lens (\DeletePackageResponse' {httpStatus} -> httpStatus) (\s@DeletePackageResponse' {} a -> s {httpStatus = a} :: DeletePackageResponse)

instance Core.NFData DeletePackageResponse
