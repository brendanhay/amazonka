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
-- Module      : Amazonka.MigrationHubReFactorSpaces.ListEnvironmentVpcs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon Web Services Migration Hub Refactor Spaces service
-- virtual private clouds (VPCs) that are part of the environment.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubReFactorSpaces.ListEnvironmentVpcs
  ( -- * Creating a Request
    ListEnvironmentVpcs (..),
    newListEnvironmentVpcs,

    -- * Request Lenses
    listEnvironmentVpcs_maxResults,
    listEnvironmentVpcs_nextToken,
    listEnvironmentVpcs_environmentIdentifier,

    -- * Destructuring the Response
    ListEnvironmentVpcsResponse (..),
    newListEnvironmentVpcsResponse,

    -- * Response Lenses
    listEnvironmentVpcsResponse_environmentVpcList,
    listEnvironmentVpcsResponse_nextToken,
    listEnvironmentVpcsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnvironmentVpcs' smart constructor.
data ListEnvironmentVpcs = ListEnvironmentVpcs'
  { -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment.
    environmentIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentVpcs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEnvironmentVpcs_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listEnvironmentVpcs_nextToken' - The token for the next page of results.
--
-- 'environmentIdentifier', 'listEnvironmentVpcs_environmentIdentifier' - The ID of the environment.
newListEnvironmentVpcs ::
  -- | 'environmentIdentifier'
  Prelude.Text ->
  ListEnvironmentVpcs
newListEnvironmentVpcs pEnvironmentIdentifier_ =
  ListEnvironmentVpcs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      environmentIdentifier = pEnvironmentIdentifier_
    }

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listEnvironmentVpcs_maxResults :: Lens.Lens' ListEnvironmentVpcs (Prelude.Maybe Prelude.Natural)
listEnvironmentVpcs_maxResults = Lens.lens (\ListEnvironmentVpcs' {maxResults} -> maxResults) (\s@ListEnvironmentVpcs' {} a -> s {maxResults = a} :: ListEnvironmentVpcs)

-- | The token for the next page of results.
listEnvironmentVpcs_nextToken :: Lens.Lens' ListEnvironmentVpcs (Prelude.Maybe Prelude.Text)
listEnvironmentVpcs_nextToken = Lens.lens (\ListEnvironmentVpcs' {nextToken} -> nextToken) (\s@ListEnvironmentVpcs' {} a -> s {nextToken = a} :: ListEnvironmentVpcs)

-- | The ID of the environment.
listEnvironmentVpcs_environmentIdentifier :: Lens.Lens' ListEnvironmentVpcs Prelude.Text
listEnvironmentVpcs_environmentIdentifier = Lens.lens (\ListEnvironmentVpcs' {environmentIdentifier} -> environmentIdentifier) (\s@ListEnvironmentVpcs' {} a -> s {environmentIdentifier = a} :: ListEnvironmentVpcs)

instance Core.AWSPager ListEnvironmentVpcs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnvironmentVpcsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEnvironmentVpcsResponse_environmentVpcList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEnvironmentVpcs_nextToken
          Lens..~ rs
          Lens.^? listEnvironmentVpcsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEnvironmentVpcs where
  type
    AWSResponse ListEnvironmentVpcs =
      ListEnvironmentVpcsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentVpcsResponse'
            Prelude.<$> ( x
                            Data..?> "EnvironmentVpcList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEnvironmentVpcs where
  hashWithSalt _salt ListEnvironmentVpcs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` environmentIdentifier

instance Prelude.NFData ListEnvironmentVpcs where
  rnf ListEnvironmentVpcs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentIdentifier

instance Data.ToHeaders ListEnvironmentVpcs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEnvironmentVpcs where
  toPath ListEnvironmentVpcs' {..} =
    Prelude.mconcat
      [ "/environments/",
        Data.toBS environmentIdentifier,
        "/vpcs"
      ]

instance Data.ToQuery ListEnvironmentVpcs where
  toQuery ListEnvironmentVpcs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEnvironmentVpcsResponse' smart constructor.
data ListEnvironmentVpcsResponse = ListEnvironmentVpcsResponse'
  { -- | The list of @EnvironmentVpc@ objects.
    environmentVpcList :: Prelude.Maybe [EnvironmentVpc],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentVpcsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentVpcList', 'listEnvironmentVpcsResponse_environmentVpcList' - The list of @EnvironmentVpc@ objects.
--
-- 'nextToken', 'listEnvironmentVpcsResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'listEnvironmentVpcsResponse_httpStatus' - The response's http status code.
newListEnvironmentVpcsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnvironmentVpcsResponse
newListEnvironmentVpcsResponse pHttpStatus_ =
  ListEnvironmentVpcsResponse'
    { environmentVpcList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of @EnvironmentVpc@ objects.
listEnvironmentVpcsResponse_environmentVpcList :: Lens.Lens' ListEnvironmentVpcsResponse (Prelude.Maybe [EnvironmentVpc])
listEnvironmentVpcsResponse_environmentVpcList = Lens.lens (\ListEnvironmentVpcsResponse' {environmentVpcList} -> environmentVpcList) (\s@ListEnvironmentVpcsResponse' {} a -> s {environmentVpcList = a} :: ListEnvironmentVpcsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
listEnvironmentVpcsResponse_nextToken :: Lens.Lens' ListEnvironmentVpcsResponse (Prelude.Maybe Prelude.Text)
listEnvironmentVpcsResponse_nextToken = Lens.lens (\ListEnvironmentVpcsResponse' {nextToken} -> nextToken) (\s@ListEnvironmentVpcsResponse' {} a -> s {nextToken = a} :: ListEnvironmentVpcsResponse)

-- | The response's http status code.
listEnvironmentVpcsResponse_httpStatus :: Lens.Lens' ListEnvironmentVpcsResponse Prelude.Int
listEnvironmentVpcsResponse_httpStatus = Lens.lens (\ListEnvironmentVpcsResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentVpcsResponse' {} a -> s {httpStatus = a} :: ListEnvironmentVpcsResponse)

instance Prelude.NFData ListEnvironmentVpcsResponse where
  rnf ListEnvironmentVpcsResponse' {..} =
    Prelude.rnf environmentVpcList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
