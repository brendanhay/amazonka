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
-- Module      : Amazonka.Detective.ListDatasourcePackages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists data source packages in the behavior graph.
module Amazonka.Detective.ListDatasourcePackages
  ( -- * Creating a Request
    ListDatasourcePackages (..),
    newListDatasourcePackages,

    -- * Request Lenses
    listDatasourcePackages_nextToken,
    listDatasourcePackages_maxResults,
    listDatasourcePackages_graphArn,

    -- * Destructuring the Response
    ListDatasourcePackagesResponse (..),
    newListDatasourcePackagesResponse,

    -- * Response Lenses
    listDatasourcePackagesResponse_datasourcePackages,
    listDatasourcePackagesResponse_nextToken,
    listDatasourcePackagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasourcePackages' smart constructor.
data ListDatasourcePackages = ListDatasourcePackages'
  { -- | For requests to get the next page of results, the pagination token that
    -- was returned with the previous set of results. The initial request does
    -- not include a pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the behavior graph.
    graphArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasourcePackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasourcePackages_nextToken' - For requests to get the next page of results, the pagination token that
-- was returned with the previous set of results. The initial request does
-- not include a pagination token.
--
-- 'maxResults', 'listDatasourcePackages_maxResults' - The maximum number of results to return.
--
-- 'graphArn', 'listDatasourcePackages_graphArn' - The ARN of the behavior graph.
newListDatasourcePackages ::
  -- | 'graphArn'
  Prelude.Text ->
  ListDatasourcePackages
newListDatasourcePackages pGraphArn_ =
  ListDatasourcePackages'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      graphArn = pGraphArn_
    }

-- | For requests to get the next page of results, the pagination token that
-- was returned with the previous set of results. The initial request does
-- not include a pagination token.
listDatasourcePackages_nextToken :: Lens.Lens' ListDatasourcePackages (Prelude.Maybe Prelude.Text)
listDatasourcePackages_nextToken = Lens.lens (\ListDatasourcePackages' {nextToken} -> nextToken) (\s@ListDatasourcePackages' {} a -> s {nextToken = a} :: ListDatasourcePackages)

-- | The maximum number of results to return.
listDatasourcePackages_maxResults :: Lens.Lens' ListDatasourcePackages (Prelude.Maybe Prelude.Natural)
listDatasourcePackages_maxResults = Lens.lens (\ListDatasourcePackages' {maxResults} -> maxResults) (\s@ListDatasourcePackages' {} a -> s {maxResults = a} :: ListDatasourcePackages)

-- | The ARN of the behavior graph.
listDatasourcePackages_graphArn :: Lens.Lens' ListDatasourcePackages Prelude.Text
listDatasourcePackages_graphArn = Lens.lens (\ListDatasourcePackages' {graphArn} -> graphArn) (\s@ListDatasourcePackages' {} a -> s {graphArn = a} :: ListDatasourcePackages)

instance Core.AWSRequest ListDatasourcePackages where
  type
    AWSResponse ListDatasourcePackages =
      ListDatasourcePackagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasourcePackagesResponse'
            Prelude.<$> ( x Data..?> "DatasourcePackages"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasourcePackages where
  hashWithSalt _salt ListDatasourcePackages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` graphArn

instance Prelude.NFData ListDatasourcePackages where
  rnf ListDatasourcePackages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf graphArn

instance Data.ToHeaders ListDatasourcePackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatasourcePackages where
  toJSON ListDatasourcePackages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("GraphArn" Data..= graphArn)
          ]
      )

instance Data.ToPath ListDatasourcePackages where
  toPath = Prelude.const "/graph/datasources/list"

instance Data.ToQuery ListDatasourcePackages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasourcePackagesResponse' smart constructor.
data ListDatasourcePackagesResponse = ListDatasourcePackagesResponse'
  { -- | Details on the data source packages active in the behavior graph.
    datasourcePackages :: Prelude.Maybe (Prelude.HashMap DatasourcePackage DatasourcePackageIngestDetail),
    -- | For requests to get the next page of results, the pagination token that
    -- was returned with the previous set of results. The initial request does
    -- not include a pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasourcePackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasourcePackages', 'listDatasourcePackagesResponse_datasourcePackages' - Details on the data source packages active in the behavior graph.
--
-- 'nextToken', 'listDatasourcePackagesResponse_nextToken' - For requests to get the next page of results, the pagination token that
-- was returned with the previous set of results. The initial request does
-- not include a pagination token.
--
-- 'httpStatus', 'listDatasourcePackagesResponse_httpStatus' - The response's http status code.
newListDatasourcePackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasourcePackagesResponse
newListDatasourcePackagesResponse pHttpStatus_ =
  ListDatasourcePackagesResponse'
    { datasourcePackages =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details on the data source packages active in the behavior graph.
listDatasourcePackagesResponse_datasourcePackages :: Lens.Lens' ListDatasourcePackagesResponse (Prelude.Maybe (Prelude.HashMap DatasourcePackage DatasourcePackageIngestDetail))
listDatasourcePackagesResponse_datasourcePackages = Lens.lens (\ListDatasourcePackagesResponse' {datasourcePackages} -> datasourcePackages) (\s@ListDatasourcePackagesResponse' {} a -> s {datasourcePackages = a} :: ListDatasourcePackagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | For requests to get the next page of results, the pagination token that
-- was returned with the previous set of results. The initial request does
-- not include a pagination token.
listDatasourcePackagesResponse_nextToken :: Lens.Lens' ListDatasourcePackagesResponse (Prelude.Maybe Prelude.Text)
listDatasourcePackagesResponse_nextToken = Lens.lens (\ListDatasourcePackagesResponse' {nextToken} -> nextToken) (\s@ListDatasourcePackagesResponse' {} a -> s {nextToken = a} :: ListDatasourcePackagesResponse)

-- | The response's http status code.
listDatasourcePackagesResponse_httpStatus :: Lens.Lens' ListDatasourcePackagesResponse Prelude.Int
listDatasourcePackagesResponse_httpStatus = Lens.lens (\ListDatasourcePackagesResponse' {httpStatus} -> httpStatus) (\s@ListDatasourcePackagesResponse' {} a -> s {httpStatus = a} :: ListDatasourcePackagesResponse)

instance
  Prelude.NFData
    ListDatasourcePackagesResponse
  where
  rnf ListDatasourcePackagesResponse' {..} =
    Prelude.rnf datasourcePackages
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
