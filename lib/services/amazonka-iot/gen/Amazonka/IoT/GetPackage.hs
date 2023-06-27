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
-- Module      : Amazonka.IoT.GetPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified software package.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetPackage>
-- action.
module Amazonka.IoT.GetPackage
  ( -- * Creating a Request
    GetPackage (..),
    newGetPackage,

    -- * Request Lenses
    getPackage_packageName,

    -- * Destructuring the Response
    GetPackageResponse (..),
    newGetPackageResponse,

    -- * Response Lenses
    getPackageResponse_creationDate,
    getPackageResponse_defaultVersionName,
    getPackageResponse_description,
    getPackageResponse_lastModifiedDate,
    getPackageResponse_packageArn,
    getPackageResponse_packageName,
    getPackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPackage' smart constructor.
data GetPackage = GetPackage'
  { -- | The name of the target package.
    packageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageName', 'getPackage_packageName' - The name of the target package.
newGetPackage ::
  -- | 'packageName'
  Prelude.Text ->
  GetPackage
newGetPackage pPackageName_ =
  GetPackage' {packageName = pPackageName_}

-- | The name of the target package.
getPackage_packageName :: Lens.Lens' GetPackage Prelude.Text
getPackage_packageName = Lens.lens (\GetPackage' {packageName} -> packageName) (\s@GetPackage' {} a -> s {packageName = a} :: GetPackage)

instance Core.AWSRequest GetPackage where
  type AWSResponse GetPackage = GetPackageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPackageResponse'
            Prelude.<$> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "defaultVersionName")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "packageArn")
            Prelude.<*> (x Data..?> "packageName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPackage where
  hashWithSalt _salt GetPackage' {..} =
    _salt `Prelude.hashWithSalt` packageName

instance Prelude.NFData GetPackage where
  rnf GetPackage' {..} = Prelude.rnf packageName

instance Data.ToHeaders GetPackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPackage where
  toPath GetPackage' {..} =
    Prelude.mconcat
      ["/packages/", Data.toBS packageName]

instance Data.ToQuery GetPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPackageResponse' smart constructor.
data GetPackageResponse = GetPackageResponse'
  { -- | The date the package was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the default package version.
    defaultVersionName :: Prelude.Maybe Prelude.Text,
    -- | The package description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The date when the package was last updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The ARN for the package.
    packageArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'getPackageResponse_creationDate' - The date the package was created.
--
-- 'defaultVersionName', 'getPackageResponse_defaultVersionName' - The name of the default package version.
--
-- 'description', 'getPackageResponse_description' - The package description.
--
-- 'lastModifiedDate', 'getPackageResponse_lastModifiedDate' - The date when the package was last updated.
--
-- 'packageArn', 'getPackageResponse_packageArn' - The ARN for the package.
--
-- 'packageName', 'getPackageResponse_packageName' - The name of the package.
--
-- 'httpStatus', 'getPackageResponse_httpStatus' - The response's http status code.
newGetPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPackageResponse
newGetPackageResponse pHttpStatus_ =
  GetPackageResponse'
    { creationDate = Prelude.Nothing,
      defaultVersionName = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      packageArn = Prelude.Nothing,
      packageName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date the package was created.
getPackageResponse_creationDate :: Lens.Lens' GetPackageResponse (Prelude.Maybe Prelude.UTCTime)
getPackageResponse_creationDate = Lens.lens (\GetPackageResponse' {creationDate} -> creationDate) (\s@GetPackageResponse' {} a -> s {creationDate = a} :: GetPackageResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the default package version.
getPackageResponse_defaultVersionName :: Lens.Lens' GetPackageResponse (Prelude.Maybe Prelude.Text)
getPackageResponse_defaultVersionName = Lens.lens (\GetPackageResponse' {defaultVersionName} -> defaultVersionName) (\s@GetPackageResponse' {} a -> s {defaultVersionName = a} :: GetPackageResponse)

-- | The package description.
getPackageResponse_description :: Lens.Lens' GetPackageResponse (Prelude.Maybe Prelude.Text)
getPackageResponse_description = Lens.lens (\GetPackageResponse' {description} -> description) (\s@GetPackageResponse' {} a -> s {description = a} :: GetPackageResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The date when the package was last updated.
getPackageResponse_lastModifiedDate :: Lens.Lens' GetPackageResponse (Prelude.Maybe Prelude.UTCTime)
getPackageResponse_lastModifiedDate = Lens.lens (\GetPackageResponse' {lastModifiedDate} -> lastModifiedDate) (\s@GetPackageResponse' {} a -> s {lastModifiedDate = a} :: GetPackageResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN for the package.
getPackageResponse_packageArn :: Lens.Lens' GetPackageResponse (Prelude.Maybe Prelude.Text)
getPackageResponse_packageArn = Lens.lens (\GetPackageResponse' {packageArn} -> packageArn) (\s@GetPackageResponse' {} a -> s {packageArn = a} :: GetPackageResponse)

-- | The name of the package.
getPackageResponse_packageName :: Lens.Lens' GetPackageResponse (Prelude.Maybe Prelude.Text)
getPackageResponse_packageName = Lens.lens (\GetPackageResponse' {packageName} -> packageName) (\s@GetPackageResponse' {} a -> s {packageName = a} :: GetPackageResponse)

-- | The response's http status code.
getPackageResponse_httpStatus :: Lens.Lens' GetPackageResponse Prelude.Int
getPackageResponse_httpStatus = Lens.lens (\GetPackageResponse' {httpStatus} -> httpStatus) (\s@GetPackageResponse' {} a -> s {httpStatus = a} :: GetPackageResponse)

instance Prelude.NFData GetPackageResponse where
  rnf GetPackageResponse' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf defaultVersionName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf packageArn
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf httpStatus
