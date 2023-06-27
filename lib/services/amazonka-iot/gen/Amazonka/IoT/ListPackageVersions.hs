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
-- Module      : Amazonka.IoT.ListPackageVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the software package versions associated to the account.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListPackageVersions>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListPackageVersions
  ( -- * Creating a Request
    ListPackageVersions (..),
    newListPackageVersions,

    -- * Request Lenses
    listPackageVersions_maxResults,
    listPackageVersions_nextToken,
    listPackageVersions_status,
    listPackageVersions_packageName,

    -- * Destructuring the Response
    ListPackageVersionsResponse (..),
    newListPackageVersionsResponse,

    -- * Response Lenses
    listPackageVersionsResponse_nextToken,
    listPackageVersionsResponse_packageVersionSummaries,
    listPackageVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackageVersions' smart constructor.
data ListPackageVersions = ListPackageVersions'
  { -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the package version. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
    status :: Prelude.Maybe PackageVersionStatus,
    -- | The name of the target package.
    packageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPackageVersions_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'listPackageVersions_nextToken' - The token for the next set of results.
--
-- 'status', 'listPackageVersions_status' - The status of the package version. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
--
-- 'packageName', 'listPackageVersions_packageName' - The name of the target package.
newListPackageVersions ::
  -- | 'packageName'
  Prelude.Text ->
  ListPackageVersions
newListPackageVersions pPackageName_ =
  ListPackageVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      packageName = pPackageName_
    }

-- | The maximum number of results to return at one time.
listPackageVersions_maxResults :: Lens.Lens' ListPackageVersions (Prelude.Maybe Prelude.Natural)
listPackageVersions_maxResults = Lens.lens (\ListPackageVersions' {maxResults} -> maxResults) (\s@ListPackageVersions' {} a -> s {maxResults = a} :: ListPackageVersions)

-- | The token for the next set of results.
listPackageVersions_nextToken :: Lens.Lens' ListPackageVersions (Prelude.Maybe Prelude.Text)
listPackageVersions_nextToken = Lens.lens (\ListPackageVersions' {nextToken} -> nextToken) (\s@ListPackageVersions' {} a -> s {nextToken = a} :: ListPackageVersions)

-- | The status of the package version. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
listPackageVersions_status :: Lens.Lens' ListPackageVersions (Prelude.Maybe PackageVersionStatus)
listPackageVersions_status = Lens.lens (\ListPackageVersions' {status} -> status) (\s@ListPackageVersions' {} a -> s {status = a} :: ListPackageVersions)

-- | The name of the target package.
listPackageVersions_packageName :: Lens.Lens' ListPackageVersions Prelude.Text
listPackageVersions_packageName = Lens.lens (\ListPackageVersions' {packageName} -> packageName) (\s@ListPackageVersions' {} a -> s {packageName = a} :: ListPackageVersions)

instance Core.AWSPager ListPackageVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPackageVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPackageVersionsResponse_packageVersionSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPackageVersions_nextToken
          Lens..~ rs
          Lens.^? listPackageVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPackageVersions where
  type
    AWSResponse ListPackageVersions =
      ListPackageVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackageVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "packageVersionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackageVersions where
  hashWithSalt _salt ListPackageVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` packageName

instance Prelude.NFData ListPackageVersions where
  rnf ListPackageVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf packageName

instance Data.ToHeaders ListPackageVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPackageVersions where
  toPath ListPackageVersions' {..} =
    Prelude.mconcat
      ["/packages/", Data.toBS packageName, "/versions"]

instance Data.ToQuery ListPackageVersions where
  toQuery ListPackageVersions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListPackageVersionsResponse' smart constructor.
data ListPackageVersionsResponse = ListPackageVersionsResponse'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists the package versions associated to the package.
    packageVersionSummaries :: Prelude.Maybe [PackageVersionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackageVersionsResponse_nextToken' - The token for the next set of results.
--
-- 'packageVersionSummaries', 'listPackageVersionsResponse_packageVersionSummaries' - Lists the package versions associated to the package.
--
-- 'httpStatus', 'listPackageVersionsResponse_httpStatus' - The response's http status code.
newListPackageVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackageVersionsResponse
newListPackageVersionsResponse pHttpStatus_ =
  ListPackageVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      packageVersionSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results.
listPackageVersionsResponse_nextToken :: Lens.Lens' ListPackageVersionsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionsResponse_nextToken = Lens.lens (\ListPackageVersionsResponse' {nextToken} -> nextToken) (\s@ListPackageVersionsResponse' {} a -> s {nextToken = a} :: ListPackageVersionsResponse)

-- | Lists the package versions associated to the package.
listPackageVersionsResponse_packageVersionSummaries :: Lens.Lens' ListPackageVersionsResponse (Prelude.Maybe [PackageVersionSummary])
listPackageVersionsResponse_packageVersionSummaries = Lens.lens (\ListPackageVersionsResponse' {packageVersionSummaries} -> packageVersionSummaries) (\s@ListPackageVersionsResponse' {} a -> s {packageVersionSummaries = a} :: ListPackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPackageVersionsResponse_httpStatus :: Lens.Lens' ListPackageVersionsResponse Prelude.Int
listPackageVersionsResponse_httpStatus = Lens.lens (\ListPackageVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPackageVersionsResponse' {} a -> s {httpStatus = a} :: ListPackageVersionsResponse)

instance Prelude.NFData ListPackageVersionsResponse where
  rnf ListPackageVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packageVersionSummaries
      `Prelude.seq` Prelude.rnf httpStatus
