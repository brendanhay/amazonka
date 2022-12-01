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
-- Module      : Amazonka.EKS.ListFargateProfiles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Fargate profiles associated with the specified cluster in your
-- Amazon Web Services account in the specified Region.
--
-- This operation returns paginated results.
module Amazonka.EKS.ListFargateProfiles
  ( -- * Creating a Request
    ListFargateProfiles (..),
    newListFargateProfiles,

    -- * Request Lenses
    listFargateProfiles_nextToken,
    listFargateProfiles_maxResults,
    listFargateProfiles_clusterName,

    -- * Destructuring the Response
    ListFargateProfilesResponse (..),
    newListFargateProfilesResponse,

    -- * Response Lenses
    listFargateProfilesResponse_nextToken,
    listFargateProfilesResponse_fargateProfileNames,
    listFargateProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFargateProfiles' smart constructor.
data ListFargateProfiles = ListFargateProfiles'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @ListFargateProfiles@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of Fargate profile results returned by
    -- @ListFargateProfiles@ in paginated output. When you use this parameter,
    -- @ListFargateProfiles@ returns only @maxResults@ results in a single page
    -- along with a @nextToken@ response element. You can see the remaining
    -- results of the initial request by sending another @ListFargateProfiles@
    -- request with the returned @nextToken@ value. This value can be between 1
    -- and 100. If you don\'t use this parameter, @ListFargateProfiles@ returns
    -- up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Amazon EKS cluster that you would like to list Fargate
    -- profiles in.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFargateProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFargateProfiles_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListFargateProfiles@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
--
-- 'maxResults', 'listFargateProfiles_maxResults' - The maximum number of Fargate profile results returned by
-- @ListFargateProfiles@ in paginated output. When you use this parameter,
-- @ListFargateProfiles@ returns only @maxResults@ results in a single page
-- along with a @nextToken@ response element. You can see the remaining
-- results of the initial request by sending another @ListFargateProfiles@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 100. If you don\'t use this parameter, @ListFargateProfiles@ returns
-- up to 100 results and a @nextToken@ value if applicable.
--
-- 'clusterName', 'listFargateProfiles_clusterName' - The name of the Amazon EKS cluster that you would like to list Fargate
-- profiles in.
newListFargateProfiles ::
  -- | 'clusterName'
  Prelude.Text ->
  ListFargateProfiles
newListFargateProfiles pClusterName_ =
  ListFargateProfiles'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      clusterName = pClusterName_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @ListFargateProfiles@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
listFargateProfiles_nextToken :: Lens.Lens' ListFargateProfiles (Prelude.Maybe Prelude.Text)
listFargateProfiles_nextToken = Lens.lens (\ListFargateProfiles' {nextToken} -> nextToken) (\s@ListFargateProfiles' {} a -> s {nextToken = a} :: ListFargateProfiles)

-- | The maximum number of Fargate profile results returned by
-- @ListFargateProfiles@ in paginated output. When you use this parameter,
-- @ListFargateProfiles@ returns only @maxResults@ results in a single page
-- along with a @nextToken@ response element. You can see the remaining
-- results of the initial request by sending another @ListFargateProfiles@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 100. If you don\'t use this parameter, @ListFargateProfiles@ returns
-- up to 100 results and a @nextToken@ value if applicable.
listFargateProfiles_maxResults :: Lens.Lens' ListFargateProfiles (Prelude.Maybe Prelude.Natural)
listFargateProfiles_maxResults = Lens.lens (\ListFargateProfiles' {maxResults} -> maxResults) (\s@ListFargateProfiles' {} a -> s {maxResults = a} :: ListFargateProfiles)

-- | The name of the Amazon EKS cluster that you would like to list Fargate
-- profiles in.
listFargateProfiles_clusterName :: Lens.Lens' ListFargateProfiles Prelude.Text
listFargateProfiles_clusterName = Lens.lens (\ListFargateProfiles' {clusterName} -> clusterName) (\s@ListFargateProfiles' {} a -> s {clusterName = a} :: ListFargateProfiles)

instance Core.AWSPager ListFargateProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFargateProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFargateProfilesResponse_fargateProfileNames
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFargateProfiles_nextToken
          Lens..~ rs
          Lens.^? listFargateProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListFargateProfiles where
  type
    AWSResponse ListFargateProfiles =
      ListFargateProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFargateProfilesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "fargateProfileNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFargateProfiles where
  hashWithSalt _salt ListFargateProfiles' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData ListFargateProfiles where
  rnf ListFargateProfiles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf clusterName

instance Core.ToHeaders ListFargateProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListFargateProfiles where
  toPath ListFargateProfiles' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/fargate-profiles"
      ]

instance Core.ToQuery ListFargateProfiles where
  toQuery ListFargateProfiles' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListFargateProfilesResponse' smart constructor.
data ListFargateProfilesResponse = ListFargateProfilesResponse'
  { -- | The @nextToken@ value to include in a future @ListFargateProfiles@
    -- request. When the results of a @ListFargateProfiles@ request exceed
    -- @maxResults@, you can use this value to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all of the Fargate profiles associated with the specified
    -- cluster.
    fargateProfileNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFargateProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFargateProfilesResponse_nextToken' - The @nextToken@ value to include in a future @ListFargateProfiles@
-- request. When the results of a @ListFargateProfiles@ request exceed
-- @maxResults@, you can use this value to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'fargateProfileNames', 'listFargateProfilesResponse_fargateProfileNames' - A list of all of the Fargate profiles associated with the specified
-- cluster.
--
-- 'httpStatus', 'listFargateProfilesResponse_httpStatus' - The response's http status code.
newListFargateProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFargateProfilesResponse
newListFargateProfilesResponse pHttpStatus_ =
  ListFargateProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      fargateProfileNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListFargateProfiles@
-- request. When the results of a @ListFargateProfiles@ request exceed
-- @maxResults@, you can use this value to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listFargateProfilesResponse_nextToken :: Lens.Lens' ListFargateProfilesResponse (Prelude.Maybe Prelude.Text)
listFargateProfilesResponse_nextToken = Lens.lens (\ListFargateProfilesResponse' {nextToken} -> nextToken) (\s@ListFargateProfilesResponse' {} a -> s {nextToken = a} :: ListFargateProfilesResponse)

-- | A list of all of the Fargate profiles associated with the specified
-- cluster.
listFargateProfilesResponse_fargateProfileNames :: Lens.Lens' ListFargateProfilesResponse (Prelude.Maybe [Prelude.Text])
listFargateProfilesResponse_fargateProfileNames = Lens.lens (\ListFargateProfilesResponse' {fargateProfileNames} -> fargateProfileNames) (\s@ListFargateProfilesResponse' {} a -> s {fargateProfileNames = a} :: ListFargateProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFargateProfilesResponse_httpStatus :: Lens.Lens' ListFargateProfilesResponse Prelude.Int
listFargateProfilesResponse_httpStatus = Lens.lens (\ListFargateProfilesResponse' {httpStatus} -> httpStatus) (\s@ListFargateProfilesResponse' {} a -> s {httpStatus = a} :: ListFargateProfilesResponse)

instance Prelude.NFData ListFargateProfilesResponse where
  rnf ListFargateProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fargateProfileNames
      `Prelude.seq` Prelude.rnf httpStatus
