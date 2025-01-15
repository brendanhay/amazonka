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
-- Module      : Amazonka.Panorama.DescribePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a package.
module Amazonka.Panorama.DescribePackage
  ( -- * Creating a Request
    DescribePackage (..),
    newDescribePackage,

    -- * Request Lenses
    describePackage_packageId,

    -- * Destructuring the Response
    DescribePackageResponse (..),
    newDescribePackageResponse,

    -- * Response Lenses
    describePackageResponse_readAccessPrincipalArns,
    describePackageResponse_writeAccessPrincipalArns,
    describePackageResponse_httpStatus,
    describePackageResponse_arn,
    describePackageResponse_createdTime,
    describePackageResponse_packageId,
    describePackageResponse_packageName,
    describePackageResponse_storageLocation,
    describePackageResponse_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePackage' smart constructor.
data DescribePackage = DescribePackage'
  { -- | The package\'s ID.
    packageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageId', 'describePackage_packageId' - The package\'s ID.
newDescribePackage ::
  -- | 'packageId'
  Prelude.Text ->
  DescribePackage
newDescribePackage pPackageId_ =
  DescribePackage' {packageId = pPackageId_}

-- | The package\'s ID.
describePackage_packageId :: Lens.Lens' DescribePackage Prelude.Text
describePackage_packageId = Lens.lens (\DescribePackage' {packageId} -> packageId) (\s@DescribePackage' {} a -> s {packageId = a} :: DescribePackage)

instance Core.AWSRequest DescribePackage where
  type
    AWSResponse DescribePackage =
      DescribePackageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackageResponse'
            Prelude.<$> ( x
                            Data..?> "ReadAccessPrincipalArns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "WriteAccessPrincipalArns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "CreatedTime")
            Prelude.<*> (x Data..:> "PackageId")
            Prelude.<*> (x Data..:> "PackageName")
            Prelude.<*> (x Data..:> "StorageLocation")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribePackage where
  hashWithSalt _salt DescribePackage' {..} =
    _salt `Prelude.hashWithSalt` packageId

instance Prelude.NFData DescribePackage where
  rnf DescribePackage' {..} = Prelude.rnf packageId

instance Data.ToHeaders DescribePackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePackage where
  toPath DescribePackage' {..} =
    Prelude.mconcat
      ["/packages/metadata/", Data.toBS packageId]

instance Data.ToQuery DescribePackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePackageResponse' smart constructor.
data DescribePackageResponse = DescribePackageResponse'
  { -- | ARNs of accounts that have read access to the package.
    readAccessPrincipalArns :: Prelude.Maybe [Prelude.Text],
    -- | ARNs of accounts that have write access to the package.
    writeAccessPrincipalArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The package\'s ARN.
    arn :: Prelude.Text,
    -- | When the package was created.
    createdTime :: Data.POSIX,
    -- | The package\'s ID.
    packageId :: Prelude.Text,
    -- | The package\'s name.
    packageName :: Prelude.Text,
    -- | The package\'s storage location.
    storageLocation :: StorageLocation,
    -- | The package\'s tags.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readAccessPrincipalArns', 'describePackageResponse_readAccessPrincipalArns' - ARNs of accounts that have read access to the package.
--
-- 'writeAccessPrincipalArns', 'describePackageResponse_writeAccessPrincipalArns' - ARNs of accounts that have write access to the package.
--
-- 'httpStatus', 'describePackageResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'describePackageResponse_arn' - The package\'s ARN.
--
-- 'createdTime', 'describePackageResponse_createdTime' - When the package was created.
--
-- 'packageId', 'describePackageResponse_packageId' - The package\'s ID.
--
-- 'packageName', 'describePackageResponse_packageName' - The package\'s name.
--
-- 'storageLocation', 'describePackageResponse_storageLocation' - The package\'s storage location.
--
-- 'tags', 'describePackageResponse_tags' - The package\'s tags.
newDescribePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'packageId'
  Prelude.Text ->
  -- | 'packageName'
  Prelude.Text ->
  -- | 'storageLocation'
  StorageLocation ->
  DescribePackageResponse
newDescribePackageResponse
  pHttpStatus_
  pArn_
  pCreatedTime_
  pPackageId_
  pPackageName_
  pStorageLocation_ =
    DescribePackageResponse'
      { readAccessPrincipalArns =
          Prelude.Nothing,
        writeAccessPrincipalArns = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        packageId = pPackageId_,
        packageName = pPackageName_,
        storageLocation = pStorageLocation_,
        tags = Prelude.mempty
      }

-- | ARNs of accounts that have read access to the package.
describePackageResponse_readAccessPrincipalArns :: Lens.Lens' DescribePackageResponse (Prelude.Maybe [Prelude.Text])
describePackageResponse_readAccessPrincipalArns = Lens.lens (\DescribePackageResponse' {readAccessPrincipalArns} -> readAccessPrincipalArns) (\s@DescribePackageResponse' {} a -> s {readAccessPrincipalArns = a} :: DescribePackageResponse) Prelude.. Lens.mapping Lens.coerced

-- | ARNs of accounts that have write access to the package.
describePackageResponse_writeAccessPrincipalArns :: Lens.Lens' DescribePackageResponse (Prelude.Maybe [Prelude.Text])
describePackageResponse_writeAccessPrincipalArns = Lens.lens (\DescribePackageResponse' {writeAccessPrincipalArns} -> writeAccessPrincipalArns) (\s@DescribePackageResponse' {} a -> s {writeAccessPrincipalArns = a} :: DescribePackageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePackageResponse_httpStatus :: Lens.Lens' DescribePackageResponse Prelude.Int
describePackageResponse_httpStatus = Lens.lens (\DescribePackageResponse' {httpStatus} -> httpStatus) (\s@DescribePackageResponse' {} a -> s {httpStatus = a} :: DescribePackageResponse)

-- | The package\'s ARN.
describePackageResponse_arn :: Lens.Lens' DescribePackageResponse Prelude.Text
describePackageResponse_arn = Lens.lens (\DescribePackageResponse' {arn} -> arn) (\s@DescribePackageResponse' {} a -> s {arn = a} :: DescribePackageResponse)

-- | When the package was created.
describePackageResponse_createdTime :: Lens.Lens' DescribePackageResponse Prelude.UTCTime
describePackageResponse_createdTime = Lens.lens (\DescribePackageResponse' {createdTime} -> createdTime) (\s@DescribePackageResponse' {} a -> s {createdTime = a} :: DescribePackageResponse) Prelude.. Data._Time

-- | The package\'s ID.
describePackageResponse_packageId :: Lens.Lens' DescribePackageResponse Prelude.Text
describePackageResponse_packageId = Lens.lens (\DescribePackageResponse' {packageId} -> packageId) (\s@DescribePackageResponse' {} a -> s {packageId = a} :: DescribePackageResponse)

-- | The package\'s name.
describePackageResponse_packageName :: Lens.Lens' DescribePackageResponse Prelude.Text
describePackageResponse_packageName = Lens.lens (\DescribePackageResponse' {packageName} -> packageName) (\s@DescribePackageResponse' {} a -> s {packageName = a} :: DescribePackageResponse)

-- | The package\'s storage location.
describePackageResponse_storageLocation :: Lens.Lens' DescribePackageResponse StorageLocation
describePackageResponse_storageLocation = Lens.lens (\DescribePackageResponse' {storageLocation} -> storageLocation) (\s@DescribePackageResponse' {} a -> s {storageLocation = a} :: DescribePackageResponse)

-- | The package\'s tags.
describePackageResponse_tags :: Lens.Lens' DescribePackageResponse (Prelude.HashMap Prelude.Text Prelude.Text)
describePackageResponse_tags = Lens.lens (\DescribePackageResponse' {tags} -> tags) (\s@DescribePackageResponse' {} a -> s {tags = a} :: DescribePackageResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribePackageResponse where
  rnf DescribePackageResponse' {..} =
    Prelude.rnf readAccessPrincipalArns `Prelude.seq`
      Prelude.rnf writeAccessPrincipalArns `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf arn `Prelude.seq`
            Prelude.rnf createdTime `Prelude.seq`
              Prelude.rnf packageId `Prelude.seq`
                Prelude.rnf packageName `Prelude.seq`
                  Prelude.rnf storageLocation `Prelude.seq`
                    Prelude.rnf tags
