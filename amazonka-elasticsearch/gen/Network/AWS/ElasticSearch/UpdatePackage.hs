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
-- Module      : Network.AWS.ElasticSearch.UpdatePackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a package for use with Amazon ES domains.
module Network.AWS.ElasticSearch.UpdatePackage
  ( -- * Creating a Request
    UpdatePackage (..),
    newUpdatePackage,

    -- * Request Lenses
    updatePackage_commitMessage,
    updatePackage_packageDescription,
    updatePackage_packageID,
    updatePackage_packageSource,

    -- * Destructuring the Response
    UpdatePackageResponse (..),
    newUpdatePackageResponse,

    -- * Response Lenses
    updatePackageResponse_packageDetails,
    updatePackageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @ UpdatePackage @ operation.
--
-- /See:/ 'newUpdatePackage' smart constructor.
data UpdatePackage = UpdatePackage'
  { -- | An info message for the new version which will be shown as part of
    -- @GetPackageVersionHistoryResponse@.
    commitMessage :: Core.Maybe Core.Text,
    -- | New description of the package.
    packageDescription :: Core.Maybe Core.Text,
    -- | Unique identifier for the package.
    packageID :: Core.Text,
    packageSource :: PackageSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitMessage', 'updatePackage_commitMessage' - An info message for the new version which will be shown as part of
-- @GetPackageVersionHistoryResponse@.
--
-- 'packageDescription', 'updatePackage_packageDescription' - New description of the package.
--
-- 'packageID', 'updatePackage_packageID' - Unique identifier for the package.
--
-- 'packageSource', 'updatePackage_packageSource' - Undocumented member.
newUpdatePackage ::
  -- | 'packageID'
  Core.Text ->
  -- | 'packageSource'
  PackageSource ->
  UpdatePackage
newUpdatePackage pPackageID_ pPackageSource_ =
  UpdatePackage'
    { commitMessage = Core.Nothing,
      packageDescription = Core.Nothing,
      packageID = pPackageID_,
      packageSource = pPackageSource_
    }

-- | An info message for the new version which will be shown as part of
-- @GetPackageVersionHistoryResponse@.
updatePackage_commitMessage :: Lens.Lens' UpdatePackage (Core.Maybe Core.Text)
updatePackage_commitMessage = Lens.lens (\UpdatePackage' {commitMessage} -> commitMessage) (\s@UpdatePackage' {} a -> s {commitMessage = a} :: UpdatePackage)

-- | New description of the package.
updatePackage_packageDescription :: Lens.Lens' UpdatePackage (Core.Maybe Core.Text)
updatePackage_packageDescription = Lens.lens (\UpdatePackage' {packageDescription} -> packageDescription) (\s@UpdatePackage' {} a -> s {packageDescription = a} :: UpdatePackage)

-- | Unique identifier for the package.
updatePackage_packageID :: Lens.Lens' UpdatePackage Core.Text
updatePackage_packageID = Lens.lens (\UpdatePackage' {packageID} -> packageID) (\s@UpdatePackage' {} a -> s {packageID = a} :: UpdatePackage)

-- | Undocumented member.
updatePackage_packageSource :: Lens.Lens' UpdatePackage PackageSource
updatePackage_packageSource = Lens.lens (\UpdatePackage' {packageSource} -> packageSource) (\s@UpdatePackage' {} a -> s {packageSource = a} :: UpdatePackage)

instance Core.AWSRequest UpdatePackage where
  type
    AWSResponse UpdatePackage =
      UpdatePackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePackageResponse'
            Core.<$> (x Core..?> "PackageDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdatePackage

instance Core.NFData UpdatePackage

instance Core.ToHeaders UpdatePackage where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdatePackage where
  toJSON UpdatePackage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CommitMessage" Core..=) Core.<$> commitMessage,
            ("PackageDescription" Core..=)
              Core.<$> packageDescription,
            Core.Just ("PackageID" Core..= packageID),
            Core.Just ("PackageSource" Core..= packageSource)
          ]
      )

instance Core.ToPath UpdatePackage where
  toPath = Core.const "/2015-01-01/packages/update"

instance Core.ToQuery UpdatePackage where
  toQuery = Core.const Core.mempty

-- | Container for response returned by @ UpdatePackage @ operation.
--
-- /See:/ 'newUpdatePackageResponse' smart constructor.
data UpdatePackageResponse = UpdatePackageResponse'
  { -- | Information about the package @PackageDetails@.
    packageDetails :: Core.Maybe PackageDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageDetails', 'updatePackageResponse_packageDetails' - Information about the package @PackageDetails@.
--
-- 'httpStatus', 'updatePackageResponse_httpStatus' - The response's http status code.
newUpdatePackageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdatePackageResponse
newUpdatePackageResponse pHttpStatus_ =
  UpdatePackageResponse'
    { packageDetails =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the package @PackageDetails@.
updatePackageResponse_packageDetails :: Lens.Lens' UpdatePackageResponse (Core.Maybe PackageDetails)
updatePackageResponse_packageDetails = Lens.lens (\UpdatePackageResponse' {packageDetails} -> packageDetails) (\s@UpdatePackageResponse' {} a -> s {packageDetails = a} :: UpdatePackageResponse)

-- | The response's http status code.
updatePackageResponse_httpStatus :: Lens.Lens' UpdatePackageResponse Core.Int
updatePackageResponse_httpStatus = Lens.lens (\UpdatePackageResponse' {httpStatus} -> httpStatus) (\s@UpdatePackageResponse' {} a -> s {httpStatus = a} :: UpdatePackageResponse)

instance Core.NFData UpdatePackageResponse
