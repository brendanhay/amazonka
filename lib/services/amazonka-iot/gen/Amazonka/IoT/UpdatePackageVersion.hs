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
-- Module      : Amazonka.IoT.UpdatePackageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the supported fields for a specific package version.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdatePackageVersion>
-- and
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetIndexingConfiguration>
-- actions.
module Amazonka.IoT.UpdatePackageVersion
  ( -- * Creating a Request
    UpdatePackageVersion (..),
    newUpdatePackageVersion,

    -- * Request Lenses
    updatePackageVersion_action,
    updatePackageVersion_attributes,
    updatePackageVersion_clientToken,
    updatePackageVersion_description,
    updatePackageVersion_packageName,
    updatePackageVersion_versionName,

    -- * Destructuring the Response
    UpdatePackageVersionResponse (..),
    newUpdatePackageVersionResponse,

    -- * Response Lenses
    updatePackageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePackageVersion' smart constructor.
data UpdatePackageVersion = UpdatePackageVersion'
  { -- | The status that the package version should be assigned. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
    action :: Prelude.Maybe PackageVersionAction,
    -- | Metadata that can be used to define a package version’s configuration.
    -- For example, the S3 file location, configuration options that are being
    -- sent to the device or fleet.
    --
    -- __Note:__ Attributes can be updated only when the package version is in
    -- a draft state.
    --
    -- The combined size of all the attributes on a package version is limited
    -- to 3KB.
    attributes :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The package version description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the associated software package.
    packageName :: Prelude.Text,
    -- | The name of the target package version.
    versionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'updatePackageVersion_action' - The status that the package version should be assigned. For more
-- information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
--
-- 'attributes', 'updatePackageVersion_attributes' - Metadata that can be used to define a package version’s configuration.
-- For example, the S3 file location, configuration options that are being
-- sent to the device or fleet.
--
-- __Note:__ Attributes can be updated only when the package version is in
-- a draft state.
--
-- The combined size of all the attributes on a package version is limited
-- to 3KB.
--
-- 'clientToken', 'updatePackageVersion_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'description', 'updatePackageVersion_description' - The package version description.
--
-- 'packageName', 'updatePackageVersion_packageName' - The name of the associated software package.
--
-- 'versionName', 'updatePackageVersion_versionName' - The name of the target package version.
newUpdatePackageVersion ::
  -- | 'packageName'
  Prelude.Text ->
  -- | 'versionName'
  Prelude.Text ->
  UpdatePackageVersion
newUpdatePackageVersion pPackageName_ pVersionName_ =
  UpdatePackageVersion'
    { action = Prelude.Nothing,
      attributes = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      packageName = pPackageName_,
      versionName = pVersionName_
    }

-- | The status that the package version should be assigned. For more
-- information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
updatePackageVersion_action :: Lens.Lens' UpdatePackageVersion (Prelude.Maybe PackageVersionAction)
updatePackageVersion_action = Lens.lens (\UpdatePackageVersion' {action} -> action) (\s@UpdatePackageVersion' {} a -> s {action = a} :: UpdatePackageVersion)

-- | Metadata that can be used to define a package version’s configuration.
-- For example, the S3 file location, configuration options that are being
-- sent to the device or fleet.
--
-- __Note:__ Attributes can be updated only when the package version is in
-- a draft state.
--
-- The combined size of all the attributes on a package version is limited
-- to 3KB.
updatePackageVersion_attributes :: Lens.Lens' UpdatePackageVersion (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updatePackageVersion_attributes = Lens.lens (\UpdatePackageVersion' {attributes} -> attributes) (\s@UpdatePackageVersion' {} a -> s {attributes = a} :: UpdatePackageVersion) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updatePackageVersion_clientToken :: Lens.Lens' UpdatePackageVersion (Prelude.Maybe Prelude.Text)
updatePackageVersion_clientToken = Lens.lens (\UpdatePackageVersion' {clientToken} -> clientToken) (\s@UpdatePackageVersion' {} a -> s {clientToken = a} :: UpdatePackageVersion)

-- | The package version description.
updatePackageVersion_description :: Lens.Lens' UpdatePackageVersion (Prelude.Maybe Prelude.Text)
updatePackageVersion_description = Lens.lens (\UpdatePackageVersion' {description} -> description) (\s@UpdatePackageVersion' {} a -> s {description = a} :: UpdatePackageVersion) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the associated software package.
updatePackageVersion_packageName :: Lens.Lens' UpdatePackageVersion Prelude.Text
updatePackageVersion_packageName = Lens.lens (\UpdatePackageVersion' {packageName} -> packageName) (\s@UpdatePackageVersion' {} a -> s {packageName = a} :: UpdatePackageVersion)

-- | The name of the target package version.
updatePackageVersion_versionName :: Lens.Lens' UpdatePackageVersion Prelude.Text
updatePackageVersion_versionName = Lens.lens (\UpdatePackageVersion' {versionName} -> versionName) (\s@UpdatePackageVersion' {} a -> s {versionName = a} :: UpdatePackageVersion)

instance Core.AWSRequest UpdatePackageVersion where
  type
    AWSResponse UpdatePackageVersion =
      UpdatePackageVersionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePackageVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePackageVersion where
  hashWithSalt _salt UpdatePackageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` versionName

instance Prelude.NFData UpdatePackageVersion where
  rnf UpdatePackageVersion' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf versionName

instance Data.ToHeaders UpdatePackageVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePackageVersion where
  toJSON UpdatePackageVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("action" Data..=) Prelude.<$> action,
            ("attributes" Data..=) Prelude.<$> attributes,
            ("description" Data..=) Prelude.<$> description
          ]
      )

instance Data.ToPath UpdatePackageVersion where
  toPath UpdatePackageVersion' {..} =
    Prelude.mconcat
      [ "/packages/",
        Data.toBS packageName,
        "/versions/",
        Data.toBS versionName
      ]

instance Data.ToQuery UpdatePackageVersion where
  toQuery UpdatePackageVersion' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newUpdatePackageVersionResponse' smart constructor.
data UpdatePackageVersionResponse = UpdatePackageVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePackageVersionResponse_httpStatus' - The response's http status code.
newUpdatePackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePackageVersionResponse
newUpdatePackageVersionResponse pHttpStatus_ =
  UpdatePackageVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updatePackageVersionResponse_httpStatus :: Lens.Lens' UpdatePackageVersionResponse Prelude.Int
updatePackageVersionResponse_httpStatus = Lens.lens (\UpdatePackageVersionResponse' {httpStatus} -> httpStatus) (\s@UpdatePackageVersionResponse' {} a -> s {httpStatus = a} :: UpdatePackageVersionResponse)

instance Prelude.NFData UpdatePackageVersionResponse where
  rnf UpdatePackageVersionResponse' {..} =
    Prelude.rnf httpStatus
