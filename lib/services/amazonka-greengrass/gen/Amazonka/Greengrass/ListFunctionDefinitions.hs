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
-- Module      : Amazonka.Greengrass.ListFunctionDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Lambda function definitions.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListFunctionDefinitions
  ( -- * Creating a Request
    ListFunctionDefinitions (..),
    newListFunctionDefinitions,

    -- * Request Lenses
    listFunctionDefinitions_maxResults,
    listFunctionDefinitions_nextToken,

    -- * Destructuring the Response
    ListFunctionDefinitionsResponse (..),
    newListFunctionDefinitionsResponse,

    -- * Response Lenses
    listFunctionDefinitionsResponse_definitions,
    listFunctionDefinitionsResponse_nextToken,
    listFunctionDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFunctionDefinitions' smart constructor.
data ListFunctionDefinitions = ListFunctionDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFunctionDefinitions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listFunctionDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
newListFunctionDefinitions ::
  ListFunctionDefinitions
newListFunctionDefinitions =
  ListFunctionDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listFunctionDefinitions_maxResults :: Lens.Lens' ListFunctionDefinitions (Prelude.Maybe Prelude.Text)
listFunctionDefinitions_maxResults = Lens.lens (\ListFunctionDefinitions' {maxResults} -> maxResults) (\s@ListFunctionDefinitions' {} a -> s {maxResults = a} :: ListFunctionDefinitions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitions_nextToken :: Lens.Lens' ListFunctionDefinitions (Prelude.Maybe Prelude.Text)
listFunctionDefinitions_nextToken = Lens.lens (\ListFunctionDefinitions' {nextToken} -> nextToken) (\s@ListFunctionDefinitions' {} a -> s {nextToken = a} :: ListFunctionDefinitions)

instance Core.AWSPager ListFunctionDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionDefinitionsResponse_definitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFunctionDefinitions_nextToken
          Lens..~ rs
          Lens.^? listFunctionDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListFunctionDefinitions where
  type
    AWSResponse ListFunctionDefinitions =
      ListFunctionDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionDefinitionsResponse'
            Prelude.<$> (x Data..?> "Definitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFunctionDefinitions where
  hashWithSalt _salt ListFunctionDefinitions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFunctionDefinitions where
  rnf ListFunctionDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListFunctionDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListFunctionDefinitions where
  toPath =
    Prelude.const "/greengrass/definition/functions"

instance Data.ToQuery ListFunctionDefinitions where
  toQuery ListFunctionDefinitions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFunctionDefinitionsResponse' smart constructor.
data ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse'
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
-- Create a value of 'ListFunctionDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitions', 'listFunctionDefinitionsResponse_definitions' - Information about a definition.
--
-- 'nextToken', 'listFunctionDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listFunctionDefinitionsResponse_httpStatus' - The response's http status code.
newListFunctionDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionDefinitionsResponse
newListFunctionDefinitionsResponse pHttpStatus_ =
  ListFunctionDefinitionsResponse'
    { definitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a definition.
listFunctionDefinitionsResponse_definitions :: Lens.Lens' ListFunctionDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listFunctionDefinitionsResponse_definitions = Lens.lens (\ListFunctionDefinitionsResponse' {definitions} -> definitions) (\s@ListFunctionDefinitionsResponse' {} a -> s {definitions = a} :: ListFunctionDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitionsResponse_nextToken :: Lens.Lens' ListFunctionDefinitionsResponse (Prelude.Maybe Prelude.Text)
listFunctionDefinitionsResponse_nextToken = Lens.lens (\ListFunctionDefinitionsResponse' {nextToken} -> nextToken) (\s@ListFunctionDefinitionsResponse' {} a -> s {nextToken = a} :: ListFunctionDefinitionsResponse)

-- | The response's http status code.
listFunctionDefinitionsResponse_httpStatus :: Lens.Lens' ListFunctionDefinitionsResponse Prelude.Int
listFunctionDefinitionsResponse_httpStatus = Lens.lens (\ListFunctionDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionDefinitionsResponse' {} a -> s {httpStatus = a} :: ListFunctionDefinitionsResponse)

instance
  Prelude.NFData
    ListFunctionDefinitionsResponse
  where
  rnf ListFunctionDefinitionsResponse' {..} =
    Prelude.rnf definitions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
