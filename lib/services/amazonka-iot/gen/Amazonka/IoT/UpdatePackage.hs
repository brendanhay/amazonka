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
-- Module      : Amazonka.IoT.UpdatePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the supported fields for a specific package.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdatePackage>
-- and
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetIndexingConfiguration>
-- actions.
module Amazonka.IoT.UpdatePackage
  ( -- * Creating a Request
    UpdatePackage (..),
    newUpdatePackage,

    -- * Request Lenses
    updatePackage_clientToken,
    updatePackage_defaultVersionName,
    updatePackage_description,
    updatePackage_unsetDefaultVersion,
    updatePackage_packageName,

    -- * Destructuring the Response
    UpdatePackageResponse (..),
    newUpdatePackageResponse,

    -- * Response Lenses
    updatePackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePackage' smart constructor.
data UpdatePackage = UpdatePackage'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the default package version.
    --
    -- __Note:__ You cannot name a @defaultVersion@ and set
    -- @unsetDefaultVersion@ equal to @true@ at the same time.
    defaultVersionName :: Prelude.Maybe Prelude.Text,
    -- | The package description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Indicates whether you want to remove the named default package version
    -- from the software package. Set as @true@ to remove the default package
    -- version.
    --
    -- __Note:__ You cannot name a @defaultVersion@ and set
    -- @unsetDefaultVersion@ equal to @true@ at the same time.
    unsetDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The name of the target package.
    packageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updatePackage_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'defaultVersionName', 'updatePackage_defaultVersionName' - The name of the default package version.
--
-- __Note:__ You cannot name a @defaultVersion@ and set
-- @unsetDefaultVersion@ equal to @true@ at the same time.
--
-- 'description', 'updatePackage_description' - The package description.
--
-- 'unsetDefaultVersion', 'updatePackage_unsetDefaultVersion' - Indicates whether you want to remove the named default package version
-- from the software package. Set as @true@ to remove the default package
-- version.
--
-- __Note:__ You cannot name a @defaultVersion@ and set
-- @unsetDefaultVersion@ equal to @true@ at the same time.
--
-- 'packageName', 'updatePackage_packageName' - The name of the target package.
newUpdatePackage ::
  -- | 'packageName'
  Prelude.Text ->
  UpdatePackage
newUpdatePackage pPackageName_ =
  UpdatePackage'
    { clientToken = Prelude.Nothing,
      defaultVersionName = Prelude.Nothing,
      description = Prelude.Nothing,
      unsetDefaultVersion = Prelude.Nothing,
      packageName = pPackageName_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updatePackage_clientToken :: Lens.Lens' UpdatePackage (Prelude.Maybe Prelude.Text)
updatePackage_clientToken = Lens.lens (\UpdatePackage' {clientToken} -> clientToken) (\s@UpdatePackage' {} a -> s {clientToken = a} :: UpdatePackage)

-- | The name of the default package version.
--
-- __Note:__ You cannot name a @defaultVersion@ and set
-- @unsetDefaultVersion@ equal to @true@ at the same time.
updatePackage_defaultVersionName :: Lens.Lens' UpdatePackage (Prelude.Maybe Prelude.Text)
updatePackage_defaultVersionName = Lens.lens (\UpdatePackage' {defaultVersionName} -> defaultVersionName) (\s@UpdatePackage' {} a -> s {defaultVersionName = a} :: UpdatePackage)

-- | The package description.
updatePackage_description :: Lens.Lens' UpdatePackage (Prelude.Maybe Prelude.Text)
updatePackage_description = Lens.lens (\UpdatePackage' {description} -> description) (\s@UpdatePackage' {} a -> s {description = a} :: UpdatePackage) Prelude.. Lens.mapping Data._Sensitive

-- | Indicates whether you want to remove the named default package version
-- from the software package. Set as @true@ to remove the default package
-- version.
--
-- __Note:__ You cannot name a @defaultVersion@ and set
-- @unsetDefaultVersion@ equal to @true@ at the same time.
updatePackage_unsetDefaultVersion :: Lens.Lens' UpdatePackage (Prelude.Maybe Prelude.Bool)
updatePackage_unsetDefaultVersion = Lens.lens (\UpdatePackage' {unsetDefaultVersion} -> unsetDefaultVersion) (\s@UpdatePackage' {} a -> s {unsetDefaultVersion = a} :: UpdatePackage)

-- | The name of the target package.
updatePackage_packageName :: Lens.Lens' UpdatePackage Prelude.Text
updatePackage_packageName = Lens.lens (\UpdatePackage' {packageName} -> packageName) (\s@UpdatePackage' {} a -> s {packageName = a} :: UpdatePackage)

instance Core.AWSRequest UpdatePackage where
  type
    AWSResponse UpdatePackage =
      UpdatePackageResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePackage where
  hashWithSalt _salt UpdatePackage' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` defaultVersionName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` unsetDefaultVersion
      `Prelude.hashWithSalt` packageName

instance Prelude.NFData UpdatePackage where
  rnf UpdatePackage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf defaultVersionName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf unsetDefaultVersion
      `Prelude.seq` Prelude.rnf packageName

instance Data.ToHeaders UpdatePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePackage where
  toJSON UpdatePackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("defaultVersionName" Data..=)
              Prelude.<$> defaultVersionName,
            ("description" Data..=) Prelude.<$> description,
            ("unsetDefaultVersion" Data..=)
              Prelude.<$> unsetDefaultVersion
          ]
      )

instance Data.ToPath UpdatePackage where
  toPath UpdatePackage' {..} =
    Prelude.mconcat
      ["/packages/", Data.toBS packageName]

instance Data.ToQuery UpdatePackage where
  toQuery UpdatePackage' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newUpdatePackageResponse' smart constructor.
data UpdatePackageResponse = UpdatePackageResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'updatePackageResponse_httpStatus' - The response's http status code.
newUpdatePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePackageResponse
newUpdatePackageResponse pHttpStatus_ =
  UpdatePackageResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updatePackageResponse_httpStatus :: Lens.Lens' UpdatePackageResponse Prelude.Int
updatePackageResponse_httpStatus = Lens.lens (\UpdatePackageResponse' {httpStatus} -> httpStatus) (\s@UpdatePackageResponse' {} a -> s {httpStatus = a} :: UpdatePackageResponse)

instance Prelude.NFData UpdatePackageResponse where
  rnf UpdatePackageResponse' {..} =
    Prelude.rnf httpStatus
