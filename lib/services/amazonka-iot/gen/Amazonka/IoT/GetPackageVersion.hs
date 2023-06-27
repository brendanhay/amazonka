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
-- Module      : Amazonka.IoT.GetPackageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified package version.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetPackageVersion>
-- action.
module Amazonka.IoT.GetPackageVersion
  ( -- * Creating a Request
    GetPackageVersion (..),
    newGetPackageVersion,

    -- * Request Lenses
    getPackageVersion_packageName,
    getPackageVersion_versionName,

    -- * Destructuring the Response
    GetPackageVersionResponse (..),
    newGetPackageVersionResponse,

    -- * Response Lenses
    getPackageVersionResponse_attributes,
    getPackageVersionResponse_creationDate,
    getPackageVersionResponse_description,
    getPackageVersionResponse_errorReason,
    getPackageVersionResponse_lastModifiedDate,
    getPackageVersionResponse_packageName,
    getPackageVersionResponse_packageVersionArn,
    getPackageVersionResponse_status,
    getPackageVersionResponse_versionName,
    getPackageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPackageVersion' smart constructor.
data GetPackageVersion = GetPackageVersion'
  { -- | The name of the associated package.
    packageName :: Prelude.Text,
    -- | The name of the target package version.
    versionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageName', 'getPackageVersion_packageName' - The name of the associated package.
--
-- 'versionName', 'getPackageVersion_versionName' - The name of the target package version.
newGetPackageVersion ::
  -- | 'packageName'
  Prelude.Text ->
  -- | 'versionName'
  Prelude.Text ->
  GetPackageVersion
newGetPackageVersion pPackageName_ pVersionName_ =
  GetPackageVersion'
    { packageName = pPackageName_,
      versionName = pVersionName_
    }

-- | The name of the associated package.
getPackageVersion_packageName :: Lens.Lens' GetPackageVersion Prelude.Text
getPackageVersion_packageName = Lens.lens (\GetPackageVersion' {packageName} -> packageName) (\s@GetPackageVersion' {} a -> s {packageName = a} :: GetPackageVersion)

-- | The name of the target package version.
getPackageVersion_versionName :: Lens.Lens' GetPackageVersion Prelude.Text
getPackageVersion_versionName = Lens.lens (\GetPackageVersion' {versionName} -> versionName) (\s@GetPackageVersion' {} a -> s {versionName = a} :: GetPackageVersion)

instance Core.AWSRequest GetPackageVersion where
  type
    AWSResponse GetPackageVersion =
      GetPackageVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPackageVersionResponse'
            Prelude.<$> (x Data..?> "attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "errorReason")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "packageName")
            Prelude.<*> (x Data..?> "packageVersionArn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "versionName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPackageVersion where
  hashWithSalt _salt GetPackageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` versionName

instance Prelude.NFData GetPackageVersion where
  rnf GetPackageVersion' {..} =
    Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf versionName

instance Data.ToHeaders GetPackageVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPackageVersion where
  toPath GetPackageVersion' {..} =
    Prelude.mconcat
      [ "/packages/",
        Data.toBS packageName,
        "/versions/",
        Data.toBS versionName
      ]

instance Data.ToQuery GetPackageVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPackageVersionResponse' smart constructor.
data GetPackageVersionResponse = GetPackageVersionResponse'
  { -- | Metadata that were added to the package version that can be used to
    -- define a package version’s configuration.
    attributes :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The date when the package version was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The package version description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Error reason for a package version failure during creation or update.
    errorReason :: Prelude.Maybe Prelude.Text,
    -- | The date when the package version was last updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the package version.
    packageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status associated to the package version. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
    status :: Prelude.Maybe PackageVersionStatus,
    -- | The name of the package version.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getPackageVersionResponse_attributes' - Metadata that were added to the package version that can be used to
-- define a package version’s configuration.
--
-- 'creationDate', 'getPackageVersionResponse_creationDate' - The date when the package version was created.
--
-- 'description', 'getPackageVersionResponse_description' - The package version description.
--
-- 'errorReason', 'getPackageVersionResponse_errorReason' - Error reason for a package version failure during creation or update.
--
-- 'lastModifiedDate', 'getPackageVersionResponse_lastModifiedDate' - The date when the package version was last updated.
--
-- 'packageName', 'getPackageVersionResponse_packageName' - The name of the package.
--
-- 'packageVersionArn', 'getPackageVersionResponse_packageVersionArn' - The ARN for the package version.
--
-- 'status', 'getPackageVersionResponse_status' - The status associated to the package version. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
--
-- 'versionName', 'getPackageVersionResponse_versionName' - The name of the package version.
--
-- 'httpStatus', 'getPackageVersionResponse_httpStatus' - The response's http status code.
newGetPackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPackageVersionResponse
newGetPackageVersionResponse pHttpStatus_ =
  GetPackageVersionResponse'
    { attributes =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      errorReason = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      packageName = Prelude.Nothing,
      packageVersionArn = Prelude.Nothing,
      status = Prelude.Nothing,
      versionName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata that were added to the package version that can be used to
-- define a package version’s configuration.
getPackageVersionResponse_attributes :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getPackageVersionResponse_attributes = Lens.lens (\GetPackageVersionResponse' {attributes} -> attributes) (\s@GetPackageVersionResponse' {} a -> s {attributes = a} :: GetPackageVersionResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The date when the package version was created.
getPackageVersionResponse_creationDate :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe Prelude.UTCTime)
getPackageVersionResponse_creationDate = Lens.lens (\GetPackageVersionResponse' {creationDate} -> creationDate) (\s@GetPackageVersionResponse' {} a -> s {creationDate = a} :: GetPackageVersionResponse) Prelude.. Lens.mapping Data._Time

-- | The package version description.
getPackageVersionResponse_description :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe Prelude.Text)
getPackageVersionResponse_description = Lens.lens (\GetPackageVersionResponse' {description} -> description) (\s@GetPackageVersionResponse' {} a -> s {description = a} :: GetPackageVersionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Error reason for a package version failure during creation or update.
getPackageVersionResponse_errorReason :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe Prelude.Text)
getPackageVersionResponse_errorReason = Lens.lens (\GetPackageVersionResponse' {errorReason} -> errorReason) (\s@GetPackageVersionResponse' {} a -> s {errorReason = a} :: GetPackageVersionResponse)

-- | The date when the package version was last updated.
getPackageVersionResponse_lastModifiedDate :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe Prelude.UTCTime)
getPackageVersionResponse_lastModifiedDate = Lens.lens (\GetPackageVersionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@GetPackageVersionResponse' {} a -> s {lastModifiedDate = a} :: GetPackageVersionResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the package.
getPackageVersionResponse_packageName :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe Prelude.Text)
getPackageVersionResponse_packageName = Lens.lens (\GetPackageVersionResponse' {packageName} -> packageName) (\s@GetPackageVersionResponse' {} a -> s {packageName = a} :: GetPackageVersionResponse)

-- | The ARN for the package version.
getPackageVersionResponse_packageVersionArn :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe Prelude.Text)
getPackageVersionResponse_packageVersionArn = Lens.lens (\GetPackageVersionResponse' {packageVersionArn} -> packageVersionArn) (\s@GetPackageVersionResponse' {} a -> s {packageVersionArn = a} :: GetPackageVersionResponse)

-- | The status associated to the package version. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
getPackageVersionResponse_status :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe PackageVersionStatus)
getPackageVersionResponse_status = Lens.lens (\GetPackageVersionResponse' {status} -> status) (\s@GetPackageVersionResponse' {} a -> s {status = a} :: GetPackageVersionResponse)

-- | The name of the package version.
getPackageVersionResponse_versionName :: Lens.Lens' GetPackageVersionResponse (Prelude.Maybe Prelude.Text)
getPackageVersionResponse_versionName = Lens.lens (\GetPackageVersionResponse' {versionName} -> versionName) (\s@GetPackageVersionResponse' {} a -> s {versionName = a} :: GetPackageVersionResponse)

-- | The response's http status code.
getPackageVersionResponse_httpStatus :: Lens.Lens' GetPackageVersionResponse Prelude.Int
getPackageVersionResponse_httpStatus = Lens.lens (\GetPackageVersionResponse' {httpStatus} -> httpStatus) (\s@GetPackageVersionResponse' {} a -> s {httpStatus = a} :: GetPackageVersionResponse)

instance Prelude.NFData GetPackageVersionResponse where
  rnf GetPackageVersionResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorReason
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packageVersionArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf httpStatus
