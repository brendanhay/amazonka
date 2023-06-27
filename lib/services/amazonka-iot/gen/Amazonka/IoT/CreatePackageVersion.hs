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
-- Module      : Amazonka.IoT.CreatePackageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version for an existing IoT software package.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreatePackageVersion>
-- and
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetIndexingConfiguration>
-- actions.
module Amazonka.IoT.CreatePackageVersion
  ( -- * Creating a Request
    CreatePackageVersion (..),
    newCreatePackageVersion,

    -- * Request Lenses
    createPackageVersion_attributes,
    createPackageVersion_clientToken,
    createPackageVersion_description,
    createPackageVersion_tags,
    createPackageVersion_packageName,
    createPackageVersion_versionName,

    -- * Destructuring the Response
    CreatePackageVersionResponse (..),
    newCreatePackageVersionResponse,

    -- * Response Lenses
    createPackageVersionResponse_attributes,
    createPackageVersionResponse_description,
    createPackageVersionResponse_errorReason,
    createPackageVersionResponse_packageName,
    createPackageVersionResponse_packageVersionArn,
    createPackageVersionResponse_status,
    createPackageVersionResponse_versionName,
    createPackageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePackageVersion' smart constructor.
data CreatePackageVersion = CreatePackageVersion'
  { -- | Metadata that can be used to define a package version’s configuration.
    -- For example, the S3 file location, configuration options that are being
    -- sent to the device or fleet.
    --
    -- The combined size of all the attributes on a package version is limited
    -- to 3KB.
    attributes :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A summary of the package version being created. This can be used to
    -- outline the package\'s contents or purpose.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Metadata that can be used to manage the package version.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the associated package.
    packageName :: Prelude.Text,
    -- | The name of the new package version.
    versionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'createPackageVersion_attributes' - Metadata that can be used to define a package version’s configuration.
-- For example, the S3 file location, configuration options that are being
-- sent to the device or fleet.
--
-- The combined size of all the attributes on a package version is limited
-- to 3KB.
--
-- 'clientToken', 'createPackageVersion_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'description', 'createPackageVersion_description' - A summary of the package version being created. This can be used to
-- outline the package\'s contents or purpose.
--
-- 'tags', 'createPackageVersion_tags' - Metadata that can be used to manage the package version.
--
-- 'packageName', 'createPackageVersion_packageName' - The name of the associated package.
--
-- 'versionName', 'createPackageVersion_versionName' - The name of the new package version.
newCreatePackageVersion ::
  -- | 'packageName'
  Prelude.Text ->
  -- | 'versionName'
  Prelude.Text ->
  CreatePackageVersion
newCreatePackageVersion pPackageName_ pVersionName_ =
  CreatePackageVersion'
    { attributes = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      packageName = pPackageName_,
      versionName = pVersionName_
    }

-- | Metadata that can be used to define a package version’s configuration.
-- For example, the S3 file location, configuration options that are being
-- sent to the device or fleet.
--
-- The combined size of all the attributes on a package version is limited
-- to 3KB.
createPackageVersion_attributes :: Lens.Lens' CreatePackageVersion (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackageVersion_attributes = Lens.lens (\CreatePackageVersion' {attributes} -> attributes) (\s@CreatePackageVersion' {} a -> s {attributes = a} :: CreatePackageVersion) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
createPackageVersion_clientToken :: Lens.Lens' CreatePackageVersion (Prelude.Maybe Prelude.Text)
createPackageVersion_clientToken = Lens.lens (\CreatePackageVersion' {clientToken} -> clientToken) (\s@CreatePackageVersion' {} a -> s {clientToken = a} :: CreatePackageVersion)

-- | A summary of the package version being created. This can be used to
-- outline the package\'s contents or purpose.
createPackageVersion_description :: Lens.Lens' CreatePackageVersion (Prelude.Maybe Prelude.Text)
createPackageVersion_description = Lens.lens (\CreatePackageVersion' {description} -> description) (\s@CreatePackageVersion' {} a -> s {description = a} :: CreatePackageVersion) Prelude.. Lens.mapping Data._Sensitive

-- | Metadata that can be used to manage the package version.
createPackageVersion_tags :: Lens.Lens' CreatePackageVersion (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackageVersion_tags = Lens.lens (\CreatePackageVersion' {tags} -> tags) (\s@CreatePackageVersion' {} a -> s {tags = a} :: CreatePackageVersion) Prelude.. Lens.mapping Lens.coerced

-- | The name of the associated package.
createPackageVersion_packageName :: Lens.Lens' CreatePackageVersion Prelude.Text
createPackageVersion_packageName = Lens.lens (\CreatePackageVersion' {packageName} -> packageName) (\s@CreatePackageVersion' {} a -> s {packageName = a} :: CreatePackageVersion)

-- | The name of the new package version.
createPackageVersion_versionName :: Lens.Lens' CreatePackageVersion Prelude.Text
createPackageVersion_versionName = Lens.lens (\CreatePackageVersion' {versionName} -> versionName) (\s@CreatePackageVersion' {} a -> s {versionName = a} :: CreatePackageVersion)

instance Core.AWSRequest CreatePackageVersion where
  type
    AWSResponse CreatePackageVersion =
      CreatePackageVersionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePackageVersionResponse'
            Prelude.<$> (x Data..?> "attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "errorReason")
            Prelude.<*> (x Data..?> "packageName")
            Prelude.<*> (x Data..?> "packageVersionArn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "versionName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePackageVersion where
  hashWithSalt _salt CreatePackageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` versionName

instance Prelude.NFData CreatePackageVersion where
  rnf CreatePackageVersion' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf versionName

instance Data.ToHeaders CreatePackageVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreatePackageVersion where
  toJSON CreatePackageVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreatePackageVersion where
  toPath CreatePackageVersion' {..} =
    Prelude.mconcat
      [ "/packages/",
        Data.toBS packageName,
        "/versions/",
        Data.toBS versionName
      ]

instance Data.ToQuery CreatePackageVersion where
  toQuery CreatePackageVersion' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newCreatePackageVersionResponse' smart constructor.
data CreatePackageVersionResponse = CreatePackageVersionResponse'
  { -- | Metadata that were added to the package version that can be used to
    -- define a package version’s configuration.
    attributes :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The package version description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Error reason for a package version failure during creation or update.
    errorReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the package.
    packageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the package version. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
    status :: Prelude.Maybe PackageVersionStatus,
    -- | The name of the new package version.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'createPackageVersionResponse_attributes' - Metadata that were added to the package version that can be used to
-- define a package version’s configuration.
--
-- 'description', 'createPackageVersionResponse_description' - The package version description.
--
-- 'errorReason', 'createPackageVersionResponse_errorReason' - Error reason for a package version failure during creation or update.
--
-- 'packageName', 'createPackageVersionResponse_packageName' - The name of the associated package.
--
-- 'packageVersionArn', 'createPackageVersionResponse_packageVersionArn' - The Amazon Resource Name (ARN) for the package.
--
-- 'status', 'createPackageVersionResponse_status' - The status of the package version. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
--
-- 'versionName', 'createPackageVersionResponse_versionName' - The name of the new package version.
--
-- 'httpStatus', 'createPackageVersionResponse_httpStatus' - The response's http status code.
newCreatePackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePackageVersionResponse
newCreatePackageVersionResponse pHttpStatus_ =
  CreatePackageVersionResponse'
    { attributes =
        Prelude.Nothing,
      description = Prelude.Nothing,
      errorReason = Prelude.Nothing,
      packageName = Prelude.Nothing,
      packageVersionArn = Prelude.Nothing,
      status = Prelude.Nothing,
      versionName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata that were added to the package version that can be used to
-- define a package version’s configuration.
createPackageVersionResponse_attributes :: Lens.Lens' CreatePackageVersionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPackageVersionResponse_attributes = Lens.lens (\CreatePackageVersionResponse' {attributes} -> attributes) (\s@CreatePackageVersionResponse' {} a -> s {attributes = a} :: CreatePackageVersionResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The package version description.
createPackageVersionResponse_description :: Lens.Lens' CreatePackageVersionResponse (Prelude.Maybe Prelude.Text)
createPackageVersionResponse_description = Lens.lens (\CreatePackageVersionResponse' {description} -> description) (\s@CreatePackageVersionResponse' {} a -> s {description = a} :: CreatePackageVersionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Error reason for a package version failure during creation or update.
createPackageVersionResponse_errorReason :: Lens.Lens' CreatePackageVersionResponse (Prelude.Maybe Prelude.Text)
createPackageVersionResponse_errorReason = Lens.lens (\CreatePackageVersionResponse' {errorReason} -> errorReason) (\s@CreatePackageVersionResponse' {} a -> s {errorReason = a} :: CreatePackageVersionResponse)

-- | The name of the associated package.
createPackageVersionResponse_packageName :: Lens.Lens' CreatePackageVersionResponse (Prelude.Maybe Prelude.Text)
createPackageVersionResponse_packageName = Lens.lens (\CreatePackageVersionResponse' {packageName} -> packageName) (\s@CreatePackageVersionResponse' {} a -> s {packageName = a} :: CreatePackageVersionResponse)

-- | The Amazon Resource Name (ARN) for the package.
createPackageVersionResponse_packageVersionArn :: Lens.Lens' CreatePackageVersionResponse (Prelude.Maybe Prelude.Text)
createPackageVersionResponse_packageVersionArn = Lens.lens (\CreatePackageVersionResponse' {packageVersionArn} -> packageVersionArn) (\s@CreatePackageVersionResponse' {} a -> s {packageVersionArn = a} :: CreatePackageVersionResponse)

-- | The status of the package version. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
createPackageVersionResponse_status :: Lens.Lens' CreatePackageVersionResponse (Prelude.Maybe PackageVersionStatus)
createPackageVersionResponse_status = Lens.lens (\CreatePackageVersionResponse' {status} -> status) (\s@CreatePackageVersionResponse' {} a -> s {status = a} :: CreatePackageVersionResponse)

-- | The name of the new package version.
createPackageVersionResponse_versionName :: Lens.Lens' CreatePackageVersionResponse (Prelude.Maybe Prelude.Text)
createPackageVersionResponse_versionName = Lens.lens (\CreatePackageVersionResponse' {versionName} -> versionName) (\s@CreatePackageVersionResponse' {} a -> s {versionName = a} :: CreatePackageVersionResponse)

-- | The response's http status code.
createPackageVersionResponse_httpStatus :: Lens.Lens' CreatePackageVersionResponse Prelude.Int
createPackageVersionResponse_httpStatus = Lens.lens (\CreatePackageVersionResponse' {httpStatus} -> httpStatus) (\s@CreatePackageVersionResponse' {} a -> s {httpStatus = a} :: CreatePackageVersionResponse)

instance Prelude.NFData CreatePackageVersionResponse where
  rnf CreatePackageVersionResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorReason
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packageVersionArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf httpStatus
