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
-- Module      : Amazonka.Route53RecoveryReadiness.ListCrossAccountAuthorizations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of cross account readiness authorizations.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListCrossAccountAuthorizations
  ( -- * Creating a Request
    ListCrossAccountAuthorizations (..),
    newListCrossAccountAuthorizations,

    -- * Request Lenses
    listCrossAccountAuthorizations_nextToken,
    listCrossAccountAuthorizations_maxResults,

    -- * Destructuring the Response
    ListCrossAccountAuthorizationsResponse (..),
    newListCrossAccountAuthorizationsResponse,

    -- * Response Lenses
    listCrossAccountAuthorizationsResponse_crossAccountAuthorizations,
    listCrossAccountAuthorizationsResponse_nextToken,
    listCrossAccountAuthorizationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListCrossAccountAuthorizations' smart constructor.
data ListCrossAccountAuthorizations = ListCrossAccountAuthorizations'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCrossAccountAuthorizations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCrossAccountAuthorizations_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'listCrossAccountAuthorizations_maxResults' - Upper bound on number of records to return.
newListCrossAccountAuthorizations ::
  ListCrossAccountAuthorizations
newListCrossAccountAuthorizations =
  ListCrossAccountAuthorizations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token used to resume pagination from the end of a previous request.
listCrossAccountAuthorizations_nextToken :: Lens.Lens' ListCrossAccountAuthorizations (Prelude.Maybe Prelude.Text)
listCrossAccountAuthorizations_nextToken = Lens.lens (\ListCrossAccountAuthorizations' {nextToken} -> nextToken) (\s@ListCrossAccountAuthorizations' {} a -> s {nextToken = a} :: ListCrossAccountAuthorizations)

-- | Upper bound on number of records to return.
listCrossAccountAuthorizations_maxResults :: Lens.Lens' ListCrossAccountAuthorizations (Prelude.Maybe Prelude.Natural)
listCrossAccountAuthorizations_maxResults = Lens.lens (\ListCrossAccountAuthorizations' {maxResults} -> maxResults) (\s@ListCrossAccountAuthorizations' {} a -> s {maxResults = a} :: ListCrossAccountAuthorizations)

instance Core.AWSPager ListCrossAccountAuthorizations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCrossAccountAuthorizationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCrossAccountAuthorizationsResponse_crossAccountAuthorizations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCrossAccountAuthorizations_nextToken
          Lens..~ rs
          Lens.^? listCrossAccountAuthorizationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListCrossAccountAuthorizations
  where
  type
    AWSResponse ListCrossAccountAuthorizations =
      ListCrossAccountAuthorizationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCrossAccountAuthorizationsResponse'
            Prelude.<$> ( x Core..?> "crossAccountAuthorizations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCrossAccountAuthorizations
  where
  hashWithSalt
    salt'
    ListCrossAccountAuthorizations' {..} =
      salt' `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListCrossAccountAuthorizations
  where
  rnf ListCrossAccountAuthorizations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    ListCrossAccountAuthorizations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListCrossAccountAuthorizations where
  toPath = Prelude.const "/crossaccountauthorizations"

instance Core.ToQuery ListCrossAccountAuthorizations where
  toQuery ListCrossAccountAuthorizations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCrossAccountAuthorizationsResponse' smart constructor.
data ListCrossAccountAuthorizationsResponse = ListCrossAccountAuthorizationsResponse'
  { -- | A list of CrossAccountAuthorizations
    crossAccountAuthorizations :: Prelude.Maybe [Prelude.Text],
    -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCrossAccountAuthorizationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossAccountAuthorizations', 'listCrossAccountAuthorizationsResponse_crossAccountAuthorizations' - A list of CrossAccountAuthorizations
--
-- 'nextToken', 'listCrossAccountAuthorizationsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'httpStatus', 'listCrossAccountAuthorizationsResponse_httpStatus' - The response's http status code.
newListCrossAccountAuthorizationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCrossAccountAuthorizationsResponse
newListCrossAccountAuthorizationsResponse
  pHttpStatus_ =
    ListCrossAccountAuthorizationsResponse'
      { crossAccountAuthorizations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of CrossAccountAuthorizations
listCrossAccountAuthorizationsResponse_crossAccountAuthorizations :: Lens.Lens' ListCrossAccountAuthorizationsResponse (Prelude.Maybe [Prelude.Text])
listCrossAccountAuthorizationsResponse_crossAccountAuthorizations = Lens.lens (\ListCrossAccountAuthorizationsResponse' {crossAccountAuthorizations} -> crossAccountAuthorizations) (\s@ListCrossAccountAuthorizationsResponse' {} a -> s {crossAccountAuthorizations = a} :: ListCrossAccountAuthorizationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to resume pagination from the end of the
-- collection.
listCrossAccountAuthorizationsResponse_nextToken :: Lens.Lens' ListCrossAccountAuthorizationsResponse (Prelude.Maybe Prelude.Text)
listCrossAccountAuthorizationsResponse_nextToken = Lens.lens (\ListCrossAccountAuthorizationsResponse' {nextToken} -> nextToken) (\s@ListCrossAccountAuthorizationsResponse' {} a -> s {nextToken = a} :: ListCrossAccountAuthorizationsResponse)

-- | The response's http status code.
listCrossAccountAuthorizationsResponse_httpStatus :: Lens.Lens' ListCrossAccountAuthorizationsResponse Prelude.Int
listCrossAccountAuthorizationsResponse_httpStatus = Lens.lens (\ListCrossAccountAuthorizationsResponse' {httpStatus} -> httpStatus) (\s@ListCrossAccountAuthorizationsResponse' {} a -> s {httpStatus = a} :: ListCrossAccountAuthorizationsResponse)

instance
  Prelude.NFData
    ListCrossAccountAuthorizationsResponse
  where
  rnf ListCrossAccountAuthorizationsResponse' {..} =
    Prelude.rnf crossAccountAuthorizations
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
