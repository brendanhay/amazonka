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
-- Module      : Network.AWS.IoT.ListPrincipalThings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things associated with the specified principal. A principal
-- can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito
-- identities or federated identities.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPrincipalThings
  ( -- * Creating a Request
    ListPrincipalThings (..),
    newListPrincipalThings,

    -- * Request Lenses
    listPrincipalThings_nextToken,
    listPrincipalThings_maxResults,
    listPrincipalThings_principal,

    -- * Destructuring the Response
    ListPrincipalThingsResponse (..),
    newListPrincipalThingsResponse,

    -- * Response Lenses
    listPrincipalThingsResponse_nextToken,
    listPrincipalThingsResponse_things,
    listPrincipalThingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPrincipalThings operation.
--
-- /See:/ 'newListPrincipalThings' smart constructor.
data ListPrincipalThings = ListPrincipalThings'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The principal.
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrincipalThings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPrincipalThings_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listPrincipalThings_maxResults' - The maximum number of results to return in this operation.
--
-- 'principal', 'listPrincipalThings_principal' - The principal.
newListPrincipalThings ::
  -- | 'principal'
  Prelude.Text ->
  ListPrincipalThings
newListPrincipalThings pPrincipal_ =
  ListPrincipalThings'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      principal = pPrincipal_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listPrincipalThings_nextToken :: Lens.Lens' ListPrincipalThings (Prelude.Maybe Prelude.Text)
listPrincipalThings_nextToken = Lens.lens (\ListPrincipalThings' {nextToken} -> nextToken) (\s@ListPrincipalThings' {} a -> s {nextToken = a} :: ListPrincipalThings)

-- | The maximum number of results to return in this operation.
listPrincipalThings_maxResults :: Lens.Lens' ListPrincipalThings (Prelude.Maybe Prelude.Natural)
listPrincipalThings_maxResults = Lens.lens (\ListPrincipalThings' {maxResults} -> maxResults) (\s@ListPrincipalThings' {} a -> s {maxResults = a} :: ListPrincipalThings)

-- | The principal.
listPrincipalThings_principal :: Lens.Lens' ListPrincipalThings Prelude.Text
listPrincipalThings_principal = Lens.lens (\ListPrincipalThings' {principal} -> principal) (\s@ListPrincipalThings' {} a -> s {principal = a} :: ListPrincipalThings)

instance Core.AWSPager ListPrincipalThings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPrincipalThingsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPrincipalThingsResponse_things
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPrincipalThings_nextToken
          Lens..~ rs
          Lens.^? listPrincipalThingsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPrincipalThings where
  type
    AWSResponse ListPrincipalThings =
      ListPrincipalThingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPrincipalThingsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "things" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPrincipalThings

instance Prelude.NFData ListPrincipalThings

instance Core.ToHeaders ListPrincipalThings where
  toHeaders ListPrincipalThings' {..} =
    Prelude.mconcat
      ["x-amzn-principal" Core.=# principal]

instance Core.ToPath ListPrincipalThings where
  toPath = Prelude.const "/principals/things"

instance Core.ToQuery ListPrincipalThings where
  toQuery ListPrincipalThings' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | The output from the ListPrincipalThings operation.
--
-- /See:/ 'newListPrincipalThingsResponse' smart constructor.
data ListPrincipalThingsResponse = ListPrincipalThingsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The things.
    things :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrincipalThingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPrincipalThingsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'things', 'listPrincipalThingsResponse_things' - The things.
--
-- 'httpStatus', 'listPrincipalThingsResponse_httpStatus' - The response's http status code.
newListPrincipalThingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPrincipalThingsResponse
newListPrincipalThingsResponse pHttpStatus_ =
  ListPrincipalThingsResponse'
    { nextToken =
        Prelude.Nothing,
      things = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listPrincipalThingsResponse_nextToken :: Lens.Lens' ListPrincipalThingsResponse (Prelude.Maybe Prelude.Text)
listPrincipalThingsResponse_nextToken = Lens.lens (\ListPrincipalThingsResponse' {nextToken} -> nextToken) (\s@ListPrincipalThingsResponse' {} a -> s {nextToken = a} :: ListPrincipalThingsResponse)

-- | The things.
listPrincipalThingsResponse_things :: Lens.Lens' ListPrincipalThingsResponse (Prelude.Maybe [Prelude.Text])
listPrincipalThingsResponse_things = Lens.lens (\ListPrincipalThingsResponse' {things} -> things) (\s@ListPrincipalThingsResponse' {} a -> s {things = a} :: ListPrincipalThingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPrincipalThingsResponse_httpStatus :: Lens.Lens' ListPrincipalThingsResponse Prelude.Int
listPrincipalThingsResponse_httpStatus = Lens.lens (\ListPrincipalThingsResponse' {httpStatus} -> httpStatus) (\s@ListPrincipalThingsResponse' {} a -> s {httpStatus = a} :: ListPrincipalThingsResponse)

instance Prelude.NFData ListPrincipalThingsResponse
