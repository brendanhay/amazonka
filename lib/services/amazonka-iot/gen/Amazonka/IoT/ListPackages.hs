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
-- Module      : Amazonka.IoT.ListPackages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the software packages associated to the account.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListPackages>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListPackages
  ( -- * Creating a Request
    ListPackages (..),
    newListPackages,

    -- * Request Lenses
    listPackages_maxResults,
    listPackages_nextToken,

    -- * Destructuring the Response
    ListPackagesResponse (..),
    newListPackagesResponse,

    -- * Response Lenses
    listPackagesResponse_nextToken,
    listPackagesResponse_packageSummaries,
    listPackagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackages' smart constructor.
data ListPackages = ListPackages'
  { -- | The maximum number of results returned at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPackages_maxResults' - The maximum number of results returned at one time.
--
-- 'nextToken', 'listPackages_nextToken' - The token for the next set of results.
newListPackages ::
  ListPackages
newListPackages =
  ListPackages'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results returned at one time.
listPackages_maxResults :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Natural)
listPackages_maxResults = Lens.lens (\ListPackages' {maxResults} -> maxResults) (\s@ListPackages' {} a -> s {maxResults = a} :: ListPackages)

-- | The token for the next set of results.
listPackages_nextToken :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_nextToken = Lens.lens (\ListPackages' {nextToken} -> nextToken) (\s@ListPackages' {} a -> s {nextToken = a} :: ListPackages)

instance Core.AWSPager ListPackages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPackagesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPackagesResponse_packageSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPackages_nextToken
          Lens..~ rs
          Lens.^? listPackagesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPackages where
  type AWSResponse ListPackages = ListPackagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "packageSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackages where
  hashWithSalt _salt ListPackages' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPackages where
  rnf ListPackages' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPackages where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPackages where
  toPath = Prelude.const "/packages"

instance Data.ToQuery ListPackages where
  toQuery ListPackages' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListPackagesResponse' smart constructor.
data ListPackagesResponse = ListPackagesResponse'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The software package summary.
    packageSummaries :: Prelude.Maybe [PackageSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackagesResponse_nextToken' - The token for the next set of results.
--
-- 'packageSummaries', 'listPackagesResponse_packageSummaries' - The software package summary.
--
-- 'httpStatus', 'listPackagesResponse_httpStatus' - The response's http status code.
newListPackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackagesResponse
newListPackagesResponse pHttpStatus_ =
  ListPackagesResponse'
    { nextToken = Prelude.Nothing,
      packageSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results.
listPackagesResponse_nextToken :: Lens.Lens' ListPackagesResponse (Prelude.Maybe Prelude.Text)
listPackagesResponse_nextToken = Lens.lens (\ListPackagesResponse' {nextToken} -> nextToken) (\s@ListPackagesResponse' {} a -> s {nextToken = a} :: ListPackagesResponse)

-- | The software package summary.
listPackagesResponse_packageSummaries :: Lens.Lens' ListPackagesResponse (Prelude.Maybe [PackageSummary])
listPackagesResponse_packageSummaries = Lens.lens (\ListPackagesResponse' {packageSummaries} -> packageSummaries) (\s@ListPackagesResponse' {} a -> s {packageSummaries = a} :: ListPackagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPackagesResponse_httpStatus :: Lens.Lens' ListPackagesResponse Prelude.Int
listPackagesResponse_httpStatus = Lens.lens (\ListPackagesResponse' {httpStatus} -> httpStatus) (\s@ListPackagesResponse' {} a -> s {httpStatus = a} :: ListPackagesResponse)

instance Prelude.NFData ListPackagesResponse where
  rnf ListPackagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packageSummaries
      `Prelude.seq` Prelude.rnf httpStatus
