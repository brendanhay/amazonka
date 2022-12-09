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
-- Module      : Amazonka.Greengrass.ListResourceDefinitions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of resource definitions.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListResourceDefinitions
  ( -- * Creating a Request
    ListResourceDefinitions (..),
    newListResourceDefinitions,

    -- * Request Lenses
    listResourceDefinitions_maxResults,
    listResourceDefinitions_nextToken,

    -- * Destructuring the Response
    ListResourceDefinitionsResponse (..),
    newListResourceDefinitionsResponse,

    -- * Response Lenses
    listResourceDefinitionsResponse_definitions,
    listResourceDefinitionsResponse_nextToken,
    listResourceDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceDefinitions' smart constructor.
data ListResourceDefinitions = ListResourceDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResourceDefinitions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listResourceDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
newListResourceDefinitions ::
  ListResourceDefinitions
newListResourceDefinitions =
  ListResourceDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listResourceDefinitions_maxResults :: Lens.Lens' ListResourceDefinitions (Prelude.Maybe Prelude.Text)
listResourceDefinitions_maxResults = Lens.lens (\ListResourceDefinitions' {maxResults} -> maxResults) (\s@ListResourceDefinitions' {} a -> s {maxResults = a} :: ListResourceDefinitions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listResourceDefinitions_nextToken :: Lens.Lens' ListResourceDefinitions (Prelude.Maybe Prelude.Text)
listResourceDefinitions_nextToken = Lens.lens (\ListResourceDefinitions' {nextToken} -> nextToken) (\s@ListResourceDefinitions' {} a -> s {nextToken = a} :: ListResourceDefinitions)

instance Core.AWSPager ListResourceDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceDefinitionsResponse_definitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceDefinitions_nextToken
          Lens..~ rs
          Lens.^? listResourceDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceDefinitions where
  type
    AWSResponse ListResourceDefinitions =
      ListResourceDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceDefinitionsResponse'
            Prelude.<$> (x Data..?> "Definitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceDefinitions where
  hashWithSalt _salt ListResourceDefinitions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListResourceDefinitions where
  rnf ListResourceDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListResourceDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListResourceDefinitions where
  toPath =
    Prelude.const "/greengrass/definition/resources"

instance Data.ToQuery ListResourceDefinitions where
  toQuery ListResourceDefinitions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListResourceDefinitionsResponse' smart constructor.
data ListResourceDefinitionsResponse = ListResourceDefinitionsResponse'
  { -- | Information about a definition.
    definitions :: Prelude.Maybe [DefinitionInformation],
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitions', 'listResourceDefinitionsResponse_definitions' - Information about a definition.
--
-- 'nextToken', 'listResourceDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listResourceDefinitionsResponse_httpStatus' - The response's http status code.
newListResourceDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceDefinitionsResponse
newListResourceDefinitionsResponse pHttpStatus_ =
  ListResourceDefinitionsResponse'
    { definitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a definition.
listResourceDefinitionsResponse_definitions :: Lens.Lens' ListResourceDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listResourceDefinitionsResponse_definitions = Lens.lens (\ListResourceDefinitionsResponse' {definitions} -> definitions) (\s@ListResourceDefinitionsResponse' {} a -> s {definitions = a} :: ListResourceDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listResourceDefinitionsResponse_nextToken :: Lens.Lens' ListResourceDefinitionsResponse (Prelude.Maybe Prelude.Text)
listResourceDefinitionsResponse_nextToken = Lens.lens (\ListResourceDefinitionsResponse' {nextToken} -> nextToken) (\s@ListResourceDefinitionsResponse' {} a -> s {nextToken = a} :: ListResourceDefinitionsResponse)

-- | The response's http status code.
listResourceDefinitionsResponse_httpStatus :: Lens.Lens' ListResourceDefinitionsResponse Prelude.Int
listResourceDefinitionsResponse_httpStatus = Lens.lens (\ListResourceDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListResourceDefinitionsResponse' {} a -> s {httpStatus = a} :: ListResourceDefinitionsResponse)

instance
  Prelude.NFData
    ListResourceDefinitionsResponse
  where
  rnf ListResourceDefinitionsResponse' {..} =
    Prelude.rnf definitions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
