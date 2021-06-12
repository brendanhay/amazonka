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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPrincipalThings operation.
--
-- /See:/ 'newListPrincipalThings' smart constructor.
data ListPrincipalThings = ListPrincipalThings'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Core.Maybe Core.Natural,
    -- | The principal.
    principal :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListPrincipalThings
newListPrincipalThings pPrincipal_ =
  ListPrincipalThings'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      principal = pPrincipal_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listPrincipalThings_nextToken :: Lens.Lens' ListPrincipalThings (Core.Maybe Core.Text)
listPrincipalThings_nextToken = Lens.lens (\ListPrincipalThings' {nextToken} -> nextToken) (\s@ListPrincipalThings' {} a -> s {nextToken = a} :: ListPrincipalThings)

-- | The maximum number of results to return in this operation.
listPrincipalThings_maxResults :: Lens.Lens' ListPrincipalThings (Core.Maybe Core.Natural)
listPrincipalThings_maxResults = Lens.lens (\ListPrincipalThings' {maxResults} -> maxResults) (\s@ListPrincipalThings' {} a -> s {maxResults = a} :: ListPrincipalThings)

-- | The principal.
listPrincipalThings_principal :: Lens.Lens' ListPrincipalThings Core.Text
listPrincipalThings_principal = Lens.lens (\ListPrincipalThings' {principal} -> principal) (\s@ListPrincipalThings' {} a -> s {principal = a} :: ListPrincipalThings)

instance Core.AWSPager ListPrincipalThings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPrincipalThingsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPrincipalThingsResponse_things Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPrincipalThings_nextToken
          Lens..~ rs
          Lens.^? listPrincipalThingsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListPrincipalThings where
  type
    AWSResponse ListPrincipalThings =
      ListPrincipalThingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPrincipalThingsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "things" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPrincipalThings

instance Core.NFData ListPrincipalThings

instance Core.ToHeaders ListPrincipalThings where
  toHeaders ListPrincipalThings' {..} =
    Core.mconcat ["x-amzn-principal" Core.=# principal]

instance Core.ToPath ListPrincipalThings where
  toPath = Core.const "/principals/things"

instance Core.ToQuery ListPrincipalThings where
  toQuery ListPrincipalThings' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | The output from the ListPrincipalThings operation.
--
-- /See:/ 'newListPrincipalThingsResponse' smart constructor.
data ListPrincipalThingsResponse = ListPrincipalThingsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The things.
    things :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListPrincipalThingsResponse
newListPrincipalThingsResponse pHttpStatus_ =
  ListPrincipalThingsResponse'
    { nextToken =
        Core.Nothing,
      things = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listPrincipalThingsResponse_nextToken :: Lens.Lens' ListPrincipalThingsResponse (Core.Maybe Core.Text)
listPrincipalThingsResponse_nextToken = Lens.lens (\ListPrincipalThingsResponse' {nextToken} -> nextToken) (\s@ListPrincipalThingsResponse' {} a -> s {nextToken = a} :: ListPrincipalThingsResponse)

-- | The things.
listPrincipalThingsResponse_things :: Lens.Lens' ListPrincipalThingsResponse (Core.Maybe [Core.Text])
listPrincipalThingsResponse_things = Lens.lens (\ListPrincipalThingsResponse' {things} -> things) (\s@ListPrincipalThingsResponse' {} a -> s {things = a} :: ListPrincipalThingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPrincipalThingsResponse_httpStatus :: Lens.Lens' ListPrincipalThingsResponse Core.Int
listPrincipalThingsResponse_httpStatus = Lens.lens (\ListPrincipalThingsResponse' {httpStatus} -> httpStatus) (\s@ListPrincipalThingsResponse' {} a -> s {httpStatus = a} :: ListPrincipalThingsResponse)

instance Core.NFData ListPrincipalThingsResponse
