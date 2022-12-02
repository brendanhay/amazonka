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
-- Module      : Amazonka.GreengrassV2.ListComponents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of component summaries. This list includes
-- components that you have permission to view.
--
-- This operation returns paginated results.
module Amazonka.GreengrassV2.ListComponents
  ( -- * Creating a Request
    ListComponents (..),
    newListComponents,

    -- * Request Lenses
    listComponents_nextToken,
    listComponents_maxResults,
    listComponents_scope,

    -- * Destructuring the Response
    ListComponentsResponse (..),
    newListComponentsResponse,

    -- * Response Lenses
    listComponentsResponse_nextToken,
    listComponentsResponse_components,
    listComponentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListComponents' smart constructor.
data ListComponents = ListComponents'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The scope of the components to list.
    --
    -- Default: @PRIVATE@
    scope :: Prelude.Maybe ComponentVisibilityScope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponents_nextToken' - The token to be used for the next set of paginated results.
--
-- 'maxResults', 'listComponents_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'scope', 'listComponents_scope' - The scope of the components to list.
--
-- Default: @PRIVATE@
newListComponents ::
  ListComponents
newListComponents =
  ListComponents'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results.
listComponents_nextToken :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Text)
listComponents_nextToken = Lens.lens (\ListComponents' {nextToken} -> nextToken) (\s@ListComponents' {} a -> s {nextToken = a} :: ListComponents)

-- | The maximum number of results to be returned per paginated request.
listComponents_maxResults :: Lens.Lens' ListComponents (Prelude.Maybe Prelude.Natural)
listComponents_maxResults = Lens.lens (\ListComponents' {maxResults} -> maxResults) (\s@ListComponents' {} a -> s {maxResults = a} :: ListComponents)

-- | The scope of the components to list.
--
-- Default: @PRIVATE@
listComponents_scope :: Lens.Lens' ListComponents (Prelude.Maybe ComponentVisibilityScope)
listComponents_scope = Lens.lens (\ListComponents' {scope} -> scope) (\s@ListComponents' {} a -> s {scope = a} :: ListComponents)

instance Core.AWSPager ListComponents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComponentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listComponentsResponse_components
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listComponents_nextToken
          Lens..~ rs
          Lens.^? listComponentsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListComponents where
  type
    AWSResponse ListComponents =
      ListComponentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComponentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "components" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListComponents where
  hashWithSalt _salt ListComponents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ListComponents where
  rnf ListComponents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf scope

instance Data.ToHeaders ListComponents where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListComponents where
  toPath = Prelude.const "/greengrass/v2/components"

instance Data.ToQuery ListComponents where
  toQuery ListComponents' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults,
        "scope" Data.=: scope
      ]

-- | /See:/ 'newListComponentsResponse' smart constructor.
data ListComponentsResponse = ListComponentsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each component.
    components :: Prelude.Maybe [Component],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'components', 'listComponentsResponse_components' - A list that summarizes each component.
--
-- 'httpStatus', 'listComponentsResponse_httpStatus' - The response's http status code.
newListComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComponentsResponse
newListComponentsResponse pHttpStatus_ =
  ListComponentsResponse'
    { nextToken =
        Prelude.Nothing,
      components = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listComponentsResponse_nextToken :: Lens.Lens' ListComponentsResponse (Prelude.Maybe Prelude.Text)
listComponentsResponse_nextToken = Lens.lens (\ListComponentsResponse' {nextToken} -> nextToken) (\s@ListComponentsResponse' {} a -> s {nextToken = a} :: ListComponentsResponse)

-- | A list that summarizes each component.
listComponentsResponse_components :: Lens.Lens' ListComponentsResponse (Prelude.Maybe [Component])
listComponentsResponse_components = Lens.lens (\ListComponentsResponse' {components} -> components) (\s@ListComponentsResponse' {} a -> s {components = a} :: ListComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listComponentsResponse_httpStatus :: Lens.Lens' ListComponentsResponse Prelude.Int
listComponentsResponse_httpStatus = Lens.lens (\ListComponentsResponse' {httpStatus} -> httpStatus) (\s@ListComponentsResponse' {} a -> s {httpStatus = a} :: ListComponentsResponse)

instance Prelude.NFData ListComponentsResponse where
  rnf ListComponentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf httpStatus
