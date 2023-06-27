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
-- Module      : Amazonka.Greengrass.ListConnectorDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connector definitions.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListConnectorDefinitions
  ( -- * Creating a Request
    ListConnectorDefinitions (..),
    newListConnectorDefinitions,

    -- * Request Lenses
    listConnectorDefinitions_maxResults,
    listConnectorDefinitions_nextToken,

    -- * Destructuring the Response
    ListConnectorDefinitionsResponse (..),
    newListConnectorDefinitionsResponse,

    -- * Response Lenses
    listConnectorDefinitionsResponse_definitions,
    listConnectorDefinitionsResponse_nextToken,
    listConnectorDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConnectorDefinitions' smart constructor.
data ListConnectorDefinitions = ListConnectorDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectorDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listConnectorDefinitions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listConnectorDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
newListConnectorDefinitions ::
  ListConnectorDefinitions
newListConnectorDefinitions =
  ListConnectorDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listConnectorDefinitions_maxResults :: Lens.Lens' ListConnectorDefinitions (Prelude.Maybe Prelude.Text)
listConnectorDefinitions_maxResults = Lens.lens (\ListConnectorDefinitions' {maxResults} -> maxResults) (\s@ListConnectorDefinitions' {} a -> s {maxResults = a} :: ListConnectorDefinitions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listConnectorDefinitions_nextToken :: Lens.Lens' ListConnectorDefinitions (Prelude.Maybe Prelude.Text)
listConnectorDefinitions_nextToken = Lens.lens (\ListConnectorDefinitions' {nextToken} -> nextToken) (\s@ListConnectorDefinitions' {} a -> s {nextToken = a} :: ListConnectorDefinitions)

instance Core.AWSPager ListConnectorDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConnectorDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConnectorDefinitionsResponse_definitions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listConnectorDefinitions_nextToken
          Lens..~ rs
          Lens.^? listConnectorDefinitionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListConnectorDefinitions where
  type
    AWSResponse ListConnectorDefinitions =
      ListConnectorDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectorDefinitionsResponse'
            Prelude.<$> (x Data..?> "Definitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConnectorDefinitions where
  hashWithSalt _salt ListConnectorDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListConnectorDefinitions where
  rnf ListConnectorDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListConnectorDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListConnectorDefinitions where
  toPath =
    Prelude.const "/greengrass/definition/connectors"

instance Data.ToQuery ListConnectorDefinitions where
  toQuery ListConnectorDefinitions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListConnectorDefinitionsResponse' smart constructor.
data ListConnectorDefinitionsResponse = ListConnectorDefinitionsResponse'
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
-- Create a value of 'ListConnectorDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitions', 'listConnectorDefinitionsResponse_definitions' - Information about a definition.
--
-- 'nextToken', 'listConnectorDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listConnectorDefinitionsResponse_httpStatus' - The response's http status code.
newListConnectorDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectorDefinitionsResponse
newListConnectorDefinitionsResponse pHttpStatus_ =
  ListConnectorDefinitionsResponse'
    { definitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a definition.
listConnectorDefinitionsResponse_definitions :: Lens.Lens' ListConnectorDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listConnectorDefinitionsResponse_definitions = Lens.lens (\ListConnectorDefinitionsResponse' {definitions} -> definitions) (\s@ListConnectorDefinitionsResponse' {} a -> s {definitions = a} :: ListConnectorDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listConnectorDefinitionsResponse_nextToken :: Lens.Lens' ListConnectorDefinitionsResponse (Prelude.Maybe Prelude.Text)
listConnectorDefinitionsResponse_nextToken = Lens.lens (\ListConnectorDefinitionsResponse' {nextToken} -> nextToken) (\s@ListConnectorDefinitionsResponse' {} a -> s {nextToken = a} :: ListConnectorDefinitionsResponse)

-- | The response's http status code.
listConnectorDefinitionsResponse_httpStatus :: Lens.Lens' ListConnectorDefinitionsResponse Prelude.Int
listConnectorDefinitionsResponse_httpStatus = Lens.lens (\ListConnectorDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListConnectorDefinitionsResponse' {} a -> s {httpStatus = a} :: ListConnectorDefinitionsResponse)

instance
  Prelude.NFData
    ListConnectorDefinitionsResponse
  where
  rnf ListConnectorDefinitionsResponse' {..} =
    Prelude.rnf definitions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
