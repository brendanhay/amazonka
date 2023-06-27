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
-- Module      : Amazonka.OpenSearch.UpdatePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a package for use with Amazon OpenSearch Service domains. For
-- more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/custom-packages.html Custom packages for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.UpdatePackage
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to the @UpdatePackage@ operation.
--
-- /See:/ 'newUpdatePackage' smart constructor.
data UpdatePackage = UpdatePackage'
  { -- | Commit message for the updated file, which is shown as part of
    -- @GetPackageVersionHistoryResponse@.
    commitMessage :: Prelude.Maybe Prelude.Text,
    -- | A new description of the package.
    packageDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the package.
    packageID :: Prelude.Text,
    -- | Amazon S3 bucket and key for the package.
    packageSource :: PackageSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitMessage', 'updatePackage_commitMessage' - Commit message for the updated file, which is shown as part of
-- @GetPackageVersionHistoryResponse@.
--
-- 'packageDescription', 'updatePackage_packageDescription' - A new description of the package.
--
-- 'packageID', 'updatePackage_packageID' - The unique identifier for the package.
--
-- 'packageSource', 'updatePackage_packageSource' - Amazon S3 bucket and key for the package.
newUpdatePackage ::
  -- | 'packageID'
  Prelude.Text ->
  -- | 'packageSource'
  PackageSource ->
  UpdatePackage
newUpdatePackage pPackageID_ pPackageSource_ =
  UpdatePackage'
    { commitMessage = Prelude.Nothing,
      packageDescription = Prelude.Nothing,
      packageID = pPackageID_,
      packageSource = pPackageSource_
    }

-- | Commit message for the updated file, which is shown as part of
-- @GetPackageVersionHistoryResponse@.
updatePackage_commitMessage :: Lens.Lens' UpdatePackage (Prelude.Maybe Prelude.Text)
updatePackage_commitMessage = Lens.lens (\UpdatePackage' {commitMessage} -> commitMessage) (\s@UpdatePackage' {} a -> s {commitMessage = a} :: UpdatePackage)

-- | A new description of the package.
updatePackage_packageDescription :: Lens.Lens' UpdatePackage (Prelude.Maybe Prelude.Text)
updatePackage_packageDescription = Lens.lens (\UpdatePackage' {packageDescription} -> packageDescription) (\s@UpdatePackage' {} a -> s {packageDescription = a} :: UpdatePackage)

-- | The unique identifier for the package.
updatePackage_packageID :: Lens.Lens' UpdatePackage Prelude.Text
updatePackage_packageID = Lens.lens (\UpdatePackage' {packageID} -> packageID) (\s@UpdatePackage' {} a -> s {packageID = a} :: UpdatePackage)

-- | Amazon S3 bucket and key for the package.
updatePackage_packageSource :: Lens.Lens' UpdatePackage PackageSource
updatePackage_packageSource = Lens.lens (\UpdatePackage' {packageSource} -> packageSource) (\s@UpdatePackage' {} a -> s {packageSource = a} :: UpdatePackage)

instance Core.AWSRequest UpdatePackage where
  type
    AWSResponse UpdatePackage =
      UpdatePackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePackageResponse'
            Prelude.<$> (x Data..?> "PackageDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePackage where
  hashWithSalt _salt UpdatePackage' {..} =
    _salt
      `Prelude.hashWithSalt` commitMessage
      `Prelude.hashWithSalt` packageDescription
      `Prelude.hashWithSalt` packageID
      `Prelude.hashWithSalt` packageSource

instance Prelude.NFData UpdatePackage where
  rnf UpdatePackage' {..} =
    Prelude.rnf commitMessage
      `Prelude.seq` Prelude.rnf packageDescription
      `Prelude.seq` Prelude.rnf packageID
      `Prelude.seq` Prelude.rnf packageSource

instance Data.ToHeaders UpdatePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePackage where
  toJSON UpdatePackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CommitMessage" Data..=) Prelude.<$> commitMessage,
            ("PackageDescription" Data..=)
              Prelude.<$> packageDescription,
            Prelude.Just ("PackageID" Data..= packageID),
            Prelude.Just
              ("PackageSource" Data..= packageSource)
          ]
      )

instance Data.ToPath UpdatePackage where
  toPath = Prelude.const "/2021-01-01/packages/update"

instance Data.ToQuery UpdatePackage where
  toQuery = Prelude.const Prelude.mempty

-- | Container for the response returned by the @UpdatePackage@ operation.
--
-- /See:/ 'newUpdatePackageResponse' smart constructor.
data UpdatePackageResponse = UpdatePackageResponse'
  { -- | Information about a package.
    packageDetails :: Prelude.Maybe PackageDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageDetails', 'updatePackageResponse_packageDetails' - Information about a package.
--
-- 'httpStatus', 'updatePackageResponse_httpStatus' - The response's http status code.
newUpdatePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePackageResponse
newUpdatePackageResponse pHttpStatus_ =
  UpdatePackageResponse'
    { packageDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a package.
updatePackageResponse_packageDetails :: Lens.Lens' UpdatePackageResponse (Prelude.Maybe PackageDetails)
updatePackageResponse_packageDetails = Lens.lens (\UpdatePackageResponse' {packageDetails} -> packageDetails) (\s@UpdatePackageResponse' {} a -> s {packageDetails = a} :: UpdatePackageResponse)

-- | The response's http status code.
updatePackageResponse_httpStatus :: Lens.Lens' UpdatePackageResponse Prelude.Int
updatePackageResponse_httpStatus = Lens.lens (\UpdatePackageResponse' {httpStatus} -> httpStatus) (\s@UpdatePackageResponse' {} a -> s {httpStatus = a} :: UpdatePackageResponse)

instance Prelude.NFData UpdatePackageResponse where
  rnf UpdatePackageResponse' {..} =
    Prelude.rnf packageDetails
      `Prelude.seq` Prelude.rnf httpStatus
