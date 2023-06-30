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
-- Module      : Amazonka.ElasticSearch.CreatePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a package for use with Amazon ES domains.
module Amazonka.ElasticSearch.CreatePackage
  ( -- * Creating a Request
    CreatePackage (..),
    newCreatePackage,

    -- * Request Lenses
    createPackage_packageDescription,
    createPackage_packageName,
    createPackage_packageType,
    createPackage_packageSource,

    -- * Destructuring the Response
    CreatePackageResponse (..),
    newCreatePackageResponse,

    -- * Response Lenses
    createPackageResponse_packageDetails,
    createPackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to @ @@CreatePackage@@ @ operation.
--
-- /See:/ 'newCreatePackage' smart constructor.
data CreatePackage = CreatePackage'
  { -- | Description of the package.
    packageDescription :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier for the package.
    packageName :: Prelude.Text,
    -- | Type of package. Currently supports only TXT-DICTIONARY.
    packageType :: PackageType,
    -- | The customer S3 location @PackageSource@ for importing the package.
    packageSource :: PackageSource
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
-- 'packageDescription', 'createPackage_packageDescription' - Description of the package.
--
-- 'packageName', 'createPackage_packageName' - Unique identifier for the package.
--
-- 'packageType', 'createPackage_packageType' - Type of package. Currently supports only TXT-DICTIONARY.
--
-- 'packageSource', 'createPackage_packageSource' - The customer S3 location @PackageSource@ for importing the package.
newCreatePackage ::
  -- | 'packageName'
  Prelude.Text ->
  -- | 'packageType'
  PackageType ->
  -- | 'packageSource'
  PackageSource ->
  CreatePackage
newCreatePackage
  pPackageName_
  pPackageType_
  pPackageSource_ =
    CreatePackage'
      { packageDescription =
          Prelude.Nothing,
        packageName = pPackageName_,
        packageType = pPackageType_,
        packageSource = pPackageSource_
      }

-- | Description of the package.
createPackage_packageDescription :: Lens.Lens' CreatePackage (Prelude.Maybe Prelude.Text)
createPackage_packageDescription = Lens.lens (\CreatePackage' {packageDescription} -> packageDescription) (\s@CreatePackage' {} a -> s {packageDescription = a} :: CreatePackage)

-- | Unique identifier for the package.
createPackage_packageName :: Lens.Lens' CreatePackage Prelude.Text
createPackage_packageName = Lens.lens (\CreatePackage' {packageName} -> packageName) (\s@CreatePackage' {} a -> s {packageName = a} :: CreatePackage)

-- | Type of package. Currently supports only TXT-DICTIONARY.
createPackage_packageType :: Lens.Lens' CreatePackage PackageType
createPackage_packageType = Lens.lens (\CreatePackage' {packageType} -> packageType) (\s@CreatePackage' {} a -> s {packageType = a} :: CreatePackage)

-- | The customer S3 location @PackageSource@ for importing the package.
createPackage_packageSource :: Lens.Lens' CreatePackage PackageSource
createPackage_packageSource = Lens.lens (\CreatePackage' {packageSource} -> packageSource) (\s@CreatePackage' {} a -> s {packageSource = a} :: CreatePackage)

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
            Prelude.<$> (x Data..?> "PackageDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePackage where
  hashWithSalt _salt CreatePackage' {..} =
    _salt
      `Prelude.hashWithSalt` packageDescription
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` packageType
      `Prelude.hashWithSalt` packageSource

instance Prelude.NFData CreatePackage where
  rnf CreatePackage' {..} =
    Prelude.rnf packageDescription
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packageType
      `Prelude.seq` Prelude.rnf packageSource

instance Data.ToHeaders CreatePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreatePackage where
  toJSON CreatePackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PackageDescription" Data..=)
              Prelude.<$> packageDescription,
            Prelude.Just ("PackageName" Data..= packageName),
            Prelude.Just ("PackageType" Data..= packageType),
            Prelude.Just
              ("PackageSource" Data..= packageSource)
          ]
      )

instance Data.ToPath CreatePackage where
  toPath = Prelude.const "/2015-01-01/packages"

instance Data.ToQuery CreatePackage where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response returned by @ @@CreatePackage@@ @ operation.
--
-- /See:/ 'newCreatePackageResponse' smart constructor.
data CreatePackageResponse = CreatePackageResponse'
  { -- | Information about the package @PackageDetails@.
    packageDetails :: Prelude.Maybe PackageDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'packageDetails', 'createPackageResponse_packageDetails' - Information about the package @PackageDetails@.
--
-- 'httpStatus', 'createPackageResponse_httpStatus' - The response's http status code.
newCreatePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePackageResponse
newCreatePackageResponse pHttpStatus_ =
  CreatePackageResponse'
    { packageDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the package @PackageDetails@.
createPackageResponse_packageDetails :: Lens.Lens' CreatePackageResponse (Prelude.Maybe PackageDetails)
createPackageResponse_packageDetails = Lens.lens (\CreatePackageResponse' {packageDetails} -> packageDetails) (\s@CreatePackageResponse' {} a -> s {packageDetails = a} :: CreatePackageResponse)

-- | The response's http status code.
createPackageResponse_httpStatus :: Lens.Lens' CreatePackageResponse Prelude.Int
createPackageResponse_httpStatus = Lens.lens (\CreatePackageResponse' {httpStatus} -> httpStatus) (\s@CreatePackageResponse' {} a -> s {httpStatus = a} :: CreatePackageResponse)

instance Prelude.NFData CreatePackageResponse where
  rnf CreatePackageResponse' {..} =
    Prelude.rnf packageDetails
      `Prelude.seq` Prelude.rnf httpStatus
