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
-- Module      : Amazonka.Nimble.ListStudioComponents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the StudioComponents in a studio.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListStudioComponents
  ( -- * Creating a Request
    ListStudioComponents (..),
    newListStudioComponents,

    -- * Request Lenses
    listStudioComponents_states,
    listStudioComponents_types,
    listStudioComponents_nextToken,
    listStudioComponents_maxResults,
    listStudioComponents_studioId,

    -- * Destructuring the Response
    ListStudioComponentsResponse (..),
    newListStudioComponentsResponse,

    -- * Response Lenses
    listStudioComponentsResponse_studioComponents,
    listStudioComponentsResponse_nextToken,
    listStudioComponentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStudioComponents' smart constructor.
data ListStudioComponents = ListStudioComponents'
  { -- | A list of states.
    states :: Prelude.Maybe [Prelude.Text],
    -- | The types.
    types :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudioComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'states', 'listStudioComponents_states' - A list of states.
--
-- 'types', 'listStudioComponents_types' - The types.
--
-- 'nextToken', 'listStudioComponents_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'listStudioComponents_maxResults' - The maximum number of results to be returned per request.
--
-- 'studioId', 'listStudioComponents_studioId' - The studio ID.
newListStudioComponents ::
  -- | 'studioId'
  Prelude.Text ->
  ListStudioComponents
newListStudioComponents pStudioId_ =
  ListStudioComponents'
    { states = Prelude.Nothing,
      types = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | A list of states.
listStudioComponents_states :: Lens.Lens' ListStudioComponents (Prelude.Maybe [Prelude.Text])
listStudioComponents_states = Lens.lens (\ListStudioComponents' {states} -> states) (\s@ListStudioComponents' {} a -> s {states = a} :: ListStudioComponents) Prelude.. Lens.mapping Lens.coerced

-- | The types.
listStudioComponents_types :: Lens.Lens' ListStudioComponents (Prelude.Maybe [Prelude.Text])
listStudioComponents_types = Lens.lens (\ListStudioComponents' {types} -> types) (\s@ListStudioComponents' {} a -> s {types = a} :: ListStudioComponents) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listStudioComponents_nextToken :: Lens.Lens' ListStudioComponents (Prelude.Maybe Prelude.Text)
listStudioComponents_nextToken = Lens.lens (\ListStudioComponents' {nextToken} -> nextToken) (\s@ListStudioComponents' {} a -> s {nextToken = a} :: ListStudioComponents)

-- | The maximum number of results to be returned per request.
listStudioComponents_maxResults :: Lens.Lens' ListStudioComponents (Prelude.Maybe Prelude.Natural)
listStudioComponents_maxResults = Lens.lens (\ListStudioComponents' {maxResults} -> maxResults) (\s@ListStudioComponents' {} a -> s {maxResults = a} :: ListStudioComponents)

-- | The studio ID.
listStudioComponents_studioId :: Lens.Lens' ListStudioComponents Prelude.Text
listStudioComponents_studioId = Lens.lens (\ListStudioComponents' {studioId} -> studioId) (\s@ListStudioComponents' {} a -> s {studioId = a} :: ListStudioComponents)

instance Core.AWSPager ListStudioComponents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStudioComponentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStudioComponentsResponse_studioComponents
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStudioComponents_nextToken
          Lens..~ rs
          Lens.^? listStudioComponentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStudioComponents where
  type
    AWSResponse ListStudioComponents =
      ListStudioComponentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudioComponentsResponse'
            Prelude.<$> ( x Core..?> "studioComponents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudioComponents where
  hashWithSalt salt' ListStudioComponents' {..} =
    salt' `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` types
      `Prelude.hashWithSalt` states

instance Prelude.NFData ListStudioComponents where
  rnf ListStudioComponents' {..} =
    Prelude.rnf states
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf types

instance Core.ToHeaders ListStudioComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListStudioComponents where
  toPath ListStudioComponents' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/studio-components"
      ]

instance Core.ToQuery ListStudioComponents where
  toQuery ListStudioComponents' {..} =
    Prelude.mconcat
      [ "states"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> states),
        "types"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> types),
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListStudioComponentsResponse' smart constructor.
data ListStudioComponentsResponse = ListStudioComponentsResponse'
  { -- | A collection of studio components.
    studioComponents :: Prelude.Maybe [StudioComponent],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudioComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponents', 'listStudioComponentsResponse_studioComponents' - A collection of studio components.
--
-- 'nextToken', 'listStudioComponentsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listStudioComponentsResponse_httpStatus' - The response's http status code.
newListStudioComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStudioComponentsResponse
newListStudioComponentsResponse pHttpStatus_ =
  ListStudioComponentsResponse'
    { studioComponents =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of studio components.
listStudioComponentsResponse_studioComponents :: Lens.Lens' ListStudioComponentsResponse (Prelude.Maybe [StudioComponent])
listStudioComponentsResponse_studioComponents = Lens.lens (\ListStudioComponentsResponse' {studioComponents} -> studioComponents) (\s@ListStudioComponentsResponse' {} a -> s {studioComponents = a} :: ListStudioComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listStudioComponentsResponse_nextToken :: Lens.Lens' ListStudioComponentsResponse (Prelude.Maybe Prelude.Text)
listStudioComponentsResponse_nextToken = Lens.lens (\ListStudioComponentsResponse' {nextToken} -> nextToken) (\s@ListStudioComponentsResponse' {} a -> s {nextToken = a} :: ListStudioComponentsResponse)

-- | The response's http status code.
listStudioComponentsResponse_httpStatus :: Lens.Lens' ListStudioComponentsResponse Prelude.Int
listStudioComponentsResponse_httpStatus = Lens.lens (\ListStudioComponentsResponse' {httpStatus} -> httpStatus) (\s@ListStudioComponentsResponse' {} a -> s {httpStatus = a} :: ListStudioComponentsResponse)

instance Prelude.NFData ListStudioComponentsResponse where
  rnf ListStudioComponentsResponse' {..} =
    Prelude.rnf studioComponents
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
