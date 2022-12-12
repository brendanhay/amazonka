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
-- Module      : Amazonka.Panorama.DescribePackageVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a package version.
module Amazonka.Panorama.DescribePackageVersion
  ( -- * Creating a Request
    DescribePackageVersion (..),
    newDescribePackageVersion,

    -- * Request Lenses
    describePackageVersion_ownerAccount,
    describePackageVersion_patchVersion,
    describePackageVersion_packageId,
    describePackageVersion_packageVersion,

    -- * Destructuring the Response
    DescribePackageVersionResponse (..),
    newDescribePackageVersionResponse,

    -- * Response Lenses
    describePackageVersionResponse_ownerAccount,
    describePackageVersionResponse_packageArn,
    describePackageVersionResponse_registeredTime,
    describePackageVersionResponse_statusDescription,
    describePackageVersionResponse_httpStatus,
    describePackageVersionResponse_isLatestPatch,
    describePackageVersionResponse_packageId,
    describePackageVersionResponse_packageName,
    describePackageVersionResponse_packageVersion,
    describePackageVersionResponse_patchVersion,
    describePackageVersionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePackageVersion' smart constructor.
data DescribePackageVersion = DescribePackageVersion'
  { -- | The version\'s owner account.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The version\'s patch version.
    patchVersion :: Prelude.Maybe Prelude.Text,
    -- | The version\'s ID.
    packageId :: Prelude.Text,
    -- | The version\'s version.
    packageVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAccount', 'describePackageVersion_ownerAccount' - The version\'s owner account.
--
-- 'patchVersion', 'describePackageVersion_patchVersion' - The version\'s patch version.
--
-- 'packageId', 'describePackageVersion_packageId' - The version\'s ID.
--
-- 'packageVersion', 'describePackageVersion_packageVersion' - The version\'s version.
newDescribePackageVersion ::
  -- | 'packageId'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  DescribePackageVersion
newDescribePackageVersion
  pPackageId_
  pPackageVersion_ =
    DescribePackageVersion'
      { ownerAccount =
          Prelude.Nothing,
        patchVersion = Prelude.Nothing,
        packageId = pPackageId_,
        packageVersion = pPackageVersion_
      }

-- | The version\'s owner account.
describePackageVersion_ownerAccount :: Lens.Lens' DescribePackageVersion (Prelude.Maybe Prelude.Text)
describePackageVersion_ownerAccount = Lens.lens (\DescribePackageVersion' {ownerAccount} -> ownerAccount) (\s@DescribePackageVersion' {} a -> s {ownerAccount = a} :: DescribePackageVersion)

-- | The version\'s patch version.
describePackageVersion_patchVersion :: Lens.Lens' DescribePackageVersion (Prelude.Maybe Prelude.Text)
describePackageVersion_patchVersion = Lens.lens (\DescribePackageVersion' {patchVersion} -> patchVersion) (\s@DescribePackageVersion' {} a -> s {patchVersion = a} :: DescribePackageVersion)

-- | The version\'s ID.
describePackageVersion_packageId :: Lens.Lens' DescribePackageVersion Prelude.Text
describePackageVersion_packageId = Lens.lens (\DescribePackageVersion' {packageId} -> packageId) (\s@DescribePackageVersion' {} a -> s {packageId = a} :: DescribePackageVersion)

-- | The version\'s version.
describePackageVersion_packageVersion :: Lens.Lens' DescribePackageVersion Prelude.Text
describePackageVersion_packageVersion = Lens.lens (\DescribePackageVersion' {packageVersion} -> packageVersion) (\s@DescribePackageVersion' {} a -> s {packageVersion = a} :: DescribePackageVersion)

instance Core.AWSRequest DescribePackageVersion where
  type
    AWSResponse DescribePackageVersion =
      DescribePackageVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackageVersionResponse'
            Prelude.<$> (x Data..?> "OwnerAccount")
            Prelude.<*> (x Data..?> "PackageArn")
            Prelude.<*> (x Data..?> "RegisteredTime")
            Prelude.<*> (x Data..?> "StatusDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "IsLatestPatch")
            Prelude.<*> (x Data..:> "PackageId")
            Prelude.<*> (x Data..:> "PackageName")
            Prelude.<*> (x Data..:> "PackageVersion")
            Prelude.<*> (x Data..:> "PatchVersion")
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable DescribePackageVersion where
  hashWithSalt _salt DescribePackageVersion' {..} =
    _salt `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` patchVersion
      `Prelude.hashWithSalt` packageId
      `Prelude.hashWithSalt` packageVersion

instance Prelude.NFData DescribePackageVersion where
  rnf DescribePackageVersion' {..} =
    Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf patchVersion
      `Prelude.seq` Prelude.rnf packageId
      `Prelude.seq` Prelude.rnf packageVersion

instance Data.ToHeaders DescribePackageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePackageVersion where
  toPath DescribePackageVersion' {..} =
    Prelude.mconcat
      [ "/packages/metadata/",
        Data.toBS packageId,
        "/versions/",
        Data.toBS packageVersion
      ]

instance Data.ToQuery DescribePackageVersion where
  toQuery DescribePackageVersion' {..} =
    Prelude.mconcat
      [ "OwnerAccount" Data.=: ownerAccount,
        "PatchVersion" Data.=: patchVersion
      ]

-- | /See:/ 'newDescribePackageVersionResponse' smart constructor.
data DescribePackageVersionResponse = DescribePackageVersionResponse'
  { -- | The account ID of the version\'s owner.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the package.
    packageArn :: Prelude.Maybe Prelude.Text,
    -- | The version\'s registered time.
    registeredTime :: Prelude.Maybe Data.POSIX,
    -- | The version\'s status description.
    statusDescription :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Whether the version is the latest available.
    isLatestPatch :: Prelude.Bool,
    -- | The version\'s ID.
    packageId :: Prelude.Text,
    -- | The version\'s name.
    packageName :: Prelude.Text,
    -- | The version\'s version.
    packageVersion :: Prelude.Text,
    -- | The version\'s patch version.
    patchVersion :: Prelude.Text,
    -- | The version\'s status.
    status :: PackageVersionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAccount', 'describePackageVersionResponse_ownerAccount' - The account ID of the version\'s owner.
--
-- 'packageArn', 'describePackageVersionResponse_packageArn' - The ARN of the package.
--
-- 'registeredTime', 'describePackageVersionResponse_registeredTime' - The version\'s registered time.
--
-- 'statusDescription', 'describePackageVersionResponse_statusDescription' - The version\'s status description.
--
-- 'httpStatus', 'describePackageVersionResponse_httpStatus' - The response's http status code.
--
-- 'isLatestPatch', 'describePackageVersionResponse_isLatestPatch' - Whether the version is the latest available.
--
-- 'packageId', 'describePackageVersionResponse_packageId' - The version\'s ID.
--
-- 'packageName', 'describePackageVersionResponse_packageName' - The version\'s name.
--
-- 'packageVersion', 'describePackageVersionResponse_packageVersion' - The version\'s version.
--
-- 'patchVersion', 'describePackageVersionResponse_patchVersion' - The version\'s patch version.
--
-- 'status', 'describePackageVersionResponse_status' - The version\'s status.
newDescribePackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isLatestPatch'
  Prelude.Bool ->
  -- | 'packageId'
  Prelude.Text ->
  -- | 'packageName'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  -- | 'patchVersion'
  Prelude.Text ->
  -- | 'status'
  PackageVersionStatus ->
  DescribePackageVersionResponse
newDescribePackageVersionResponse
  pHttpStatus_
  pIsLatestPatch_
  pPackageId_
  pPackageName_
  pPackageVersion_
  pPatchVersion_
  pStatus_ =
    DescribePackageVersionResponse'
      { ownerAccount =
          Prelude.Nothing,
        packageArn = Prelude.Nothing,
        registeredTime = Prelude.Nothing,
        statusDescription = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        isLatestPatch = pIsLatestPatch_,
        packageId = pPackageId_,
        packageName = pPackageName_,
        packageVersion = pPackageVersion_,
        patchVersion = pPatchVersion_,
        status = pStatus_
      }

-- | The account ID of the version\'s owner.
describePackageVersionResponse_ownerAccount :: Lens.Lens' DescribePackageVersionResponse (Prelude.Maybe Prelude.Text)
describePackageVersionResponse_ownerAccount = Lens.lens (\DescribePackageVersionResponse' {ownerAccount} -> ownerAccount) (\s@DescribePackageVersionResponse' {} a -> s {ownerAccount = a} :: DescribePackageVersionResponse)

-- | The ARN of the package.
describePackageVersionResponse_packageArn :: Lens.Lens' DescribePackageVersionResponse (Prelude.Maybe Prelude.Text)
describePackageVersionResponse_packageArn = Lens.lens (\DescribePackageVersionResponse' {packageArn} -> packageArn) (\s@DescribePackageVersionResponse' {} a -> s {packageArn = a} :: DescribePackageVersionResponse)

-- | The version\'s registered time.
describePackageVersionResponse_registeredTime :: Lens.Lens' DescribePackageVersionResponse (Prelude.Maybe Prelude.UTCTime)
describePackageVersionResponse_registeredTime = Lens.lens (\DescribePackageVersionResponse' {registeredTime} -> registeredTime) (\s@DescribePackageVersionResponse' {} a -> s {registeredTime = a} :: DescribePackageVersionResponse) Prelude.. Lens.mapping Data._Time

-- | The version\'s status description.
describePackageVersionResponse_statusDescription :: Lens.Lens' DescribePackageVersionResponse (Prelude.Maybe Prelude.Text)
describePackageVersionResponse_statusDescription = Lens.lens (\DescribePackageVersionResponse' {statusDescription} -> statusDescription) (\s@DescribePackageVersionResponse' {} a -> s {statusDescription = a} :: DescribePackageVersionResponse)

-- | The response's http status code.
describePackageVersionResponse_httpStatus :: Lens.Lens' DescribePackageVersionResponse Prelude.Int
describePackageVersionResponse_httpStatus = Lens.lens (\DescribePackageVersionResponse' {httpStatus} -> httpStatus) (\s@DescribePackageVersionResponse' {} a -> s {httpStatus = a} :: DescribePackageVersionResponse)

-- | Whether the version is the latest available.
describePackageVersionResponse_isLatestPatch :: Lens.Lens' DescribePackageVersionResponse Prelude.Bool
describePackageVersionResponse_isLatestPatch = Lens.lens (\DescribePackageVersionResponse' {isLatestPatch} -> isLatestPatch) (\s@DescribePackageVersionResponse' {} a -> s {isLatestPatch = a} :: DescribePackageVersionResponse)

-- | The version\'s ID.
describePackageVersionResponse_packageId :: Lens.Lens' DescribePackageVersionResponse Prelude.Text
describePackageVersionResponse_packageId = Lens.lens (\DescribePackageVersionResponse' {packageId} -> packageId) (\s@DescribePackageVersionResponse' {} a -> s {packageId = a} :: DescribePackageVersionResponse)

-- | The version\'s name.
describePackageVersionResponse_packageName :: Lens.Lens' DescribePackageVersionResponse Prelude.Text
describePackageVersionResponse_packageName = Lens.lens (\DescribePackageVersionResponse' {packageName} -> packageName) (\s@DescribePackageVersionResponse' {} a -> s {packageName = a} :: DescribePackageVersionResponse)

-- | The version\'s version.
describePackageVersionResponse_packageVersion :: Lens.Lens' DescribePackageVersionResponse Prelude.Text
describePackageVersionResponse_packageVersion = Lens.lens (\DescribePackageVersionResponse' {packageVersion} -> packageVersion) (\s@DescribePackageVersionResponse' {} a -> s {packageVersion = a} :: DescribePackageVersionResponse)

-- | The version\'s patch version.
describePackageVersionResponse_patchVersion :: Lens.Lens' DescribePackageVersionResponse Prelude.Text
describePackageVersionResponse_patchVersion = Lens.lens (\DescribePackageVersionResponse' {patchVersion} -> patchVersion) (\s@DescribePackageVersionResponse' {} a -> s {patchVersion = a} :: DescribePackageVersionResponse)

-- | The version\'s status.
describePackageVersionResponse_status :: Lens.Lens' DescribePackageVersionResponse PackageVersionStatus
describePackageVersionResponse_status = Lens.lens (\DescribePackageVersionResponse' {status} -> status) (\s@DescribePackageVersionResponse' {} a -> s {status = a} :: DescribePackageVersionResponse)

instance
  Prelude.NFData
    DescribePackageVersionResponse
  where
  rnf DescribePackageVersionResponse' {..} =
    Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf packageArn
      `Prelude.seq` Prelude.rnf registeredTime
      `Prelude.seq` Prelude.rnf statusDescription
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf isLatestPatch
      `Prelude.seq` Prelude.rnf packageId
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf patchVersion
      `Prelude.seq` Prelude.rnf status
