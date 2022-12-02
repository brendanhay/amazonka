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
-- Module      : Amazonka.Panorama.CreatePackage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a package and storage location in an Amazon S3 access point.
module Amazonka.Panorama.CreatePackage
  ( -- * Creating a Request
    CreatePackage (..),
    newCreatePackage,

    -- * Request Lenses
    createPackage_tags,
    createPackage_packageName,

    -- * Destructuring the Response
    CreatePackageResponse (..),
    newCreatePackageResponse,

    -- * Response Lenses
    createPackageResponse_arn,
    createPackageResponse_packageId,
    createPackageResponse_httpStatus,
    createPackageResponse_storageLocation,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePackage' smart constructor.
data CreatePackage = CreatePackage'
  { -- | Tags for the package.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the package.
    packageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPackage_tags' - Tags for the package.
--
-- 'packageName', 'createPackage_packageName' - A name for the package.
newCreatePackage ::
  -- | 'packageName'
  Prelude.Text ->
  CreatePackage
newCreatePackage pPackageName_ =
  CreatePackage'
    { tags = Prelude.Nothing,
      packageName = pPackageName_
    }

-- | Tags for the package.
createPackage_tags :: Lens.Lens' CreatePackage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackage_tags = Lens.lens (\CreatePackage' {tags} -> tags) (\s@CreatePackage' {} a -> s {tags = a} :: CreatePackage) Prelude.. Lens.mapping Lens.coerced

-- | A name for the package.
createPackage_packageName :: Lens.Lens' CreatePackage Prelude.Text
createPackage_packageName = Lens.lens (\CreatePackage' {packageName} -> packageName) (\s@CreatePackage' {} a -> s {packageName = a} :: CreatePackage)

instance Core.AWSRequest CreatePackage where
  type
    AWSResponse CreatePackage =
      CreatePackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePackageResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "PackageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "StorageLocation")
      )

instance Prelude.Hashable CreatePackage where
  hashWithSalt _salt CreatePackage' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` packageName

instance Prelude.NFData CreatePackage where
  rnf CreatePackage' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf packageName

instance Data.ToHeaders CreatePackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePackage where
  toJSON CreatePackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("PackageName" Data..= packageName)
          ]
      )

instance Data.ToPath CreatePackage where
  toPath = Prelude.const "/packages"

instance Data.ToQuery CreatePackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePackageResponse' smart constructor.
data CreatePackageResponse = CreatePackageResponse'
  { -- | The package\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The package\'s ID.
    packageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The package\'s storage location.
    storageLocation :: StorageLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createPackageResponse_arn' - The package\'s ARN.
--
-- 'packageId', 'createPackageResponse_packageId' - The package\'s ID.
--
-- 'httpStatus', 'createPackageResponse_httpStatus' - The response's http status code.
--
-- 'storageLocation', 'createPackageResponse_storageLocation' - The package\'s storage location.
newCreatePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'storageLocation'
  StorageLocation ->
  CreatePackageResponse
newCreatePackageResponse
  pHttpStatus_
  pStorageLocation_ =
    CreatePackageResponse'
      { arn = Prelude.Nothing,
        packageId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        storageLocation = pStorageLocation_
      }

-- | The package\'s ARN.
createPackageResponse_arn :: Lens.Lens' CreatePackageResponse (Prelude.Maybe Prelude.Text)
createPackageResponse_arn = Lens.lens (\CreatePackageResponse' {arn} -> arn) (\s@CreatePackageResponse' {} a -> s {arn = a} :: CreatePackageResponse)

-- | The package\'s ID.
createPackageResponse_packageId :: Lens.Lens' CreatePackageResponse (Prelude.Maybe Prelude.Text)
createPackageResponse_packageId = Lens.lens (\CreatePackageResponse' {packageId} -> packageId) (\s@CreatePackageResponse' {} a -> s {packageId = a} :: CreatePackageResponse)

-- | The response's http status code.
createPackageResponse_httpStatus :: Lens.Lens' CreatePackageResponse Prelude.Int
createPackageResponse_httpStatus = Lens.lens (\CreatePackageResponse' {httpStatus} -> httpStatus) (\s@CreatePackageResponse' {} a -> s {httpStatus = a} :: CreatePackageResponse)

-- | The package\'s storage location.
createPackageResponse_storageLocation :: Lens.Lens' CreatePackageResponse StorageLocation
createPackageResponse_storageLocation = Lens.lens (\CreatePackageResponse' {storageLocation} -> storageLocation) (\s@CreatePackageResponse' {} a -> s {storageLocation = a} :: CreatePackageResponse)

instance Prelude.NFData CreatePackageResponse where
  rnf CreatePackageResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf packageId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf storageLocation
