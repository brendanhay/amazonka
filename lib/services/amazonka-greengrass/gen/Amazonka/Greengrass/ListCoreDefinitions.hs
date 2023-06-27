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
-- Module      : Amazonka.Greengrass.ListCoreDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of core definitions.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListCoreDefinitions
  ( -- * Creating a Request
    ListCoreDefinitions (..),
    newListCoreDefinitions,

    -- * Request Lenses
    listCoreDefinitions_maxResults,
    listCoreDefinitions_nextToken,

    -- * Destructuring the Response
    ListCoreDefinitionsResponse (..),
    newListCoreDefinitionsResponse,

    -- * Response Lenses
    listCoreDefinitionsResponse_definitions,
    listCoreDefinitionsResponse_nextToken,
    listCoreDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoreDefinitions' smart constructor.
data ListCoreDefinitions = ListCoreDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoreDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCoreDefinitions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listCoreDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
newListCoreDefinitions ::
  ListCoreDefinitions
newListCoreDefinitions =
  ListCoreDefinitions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listCoreDefinitions_maxResults :: Lens.Lens' ListCoreDefinitions (Prelude.Maybe Prelude.Text)
listCoreDefinitions_maxResults = Lens.lens (\ListCoreDefinitions' {maxResults} -> maxResults) (\s@ListCoreDefinitions' {} a -> s {maxResults = a} :: ListCoreDefinitions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitions_nextToken :: Lens.Lens' ListCoreDefinitions (Prelude.Maybe Prelude.Text)
listCoreDefinitions_nextToken = Lens.lens (\ListCoreDefinitions' {nextToken} -> nextToken) (\s@ListCoreDefinitions' {} a -> s {nextToken = a} :: ListCoreDefinitions)

instance Core.AWSPager ListCoreDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoreDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoreDefinitionsResponse_definitions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCoreDefinitions_nextToken
          Lens..~ rs
          Lens.^? listCoreDefinitionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCoreDefinitions where
  type
    AWSResponse ListCoreDefinitions =
      ListCoreDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreDefinitionsResponse'
            Prelude.<$> (x Data..?> "Definitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCoreDefinitions where
  hashWithSalt _salt ListCoreDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCoreDefinitions where
  rnf ListCoreDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCoreDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCoreDefinitions where
  toPath = Prelude.const "/greengrass/definition/cores"

instance Data.ToQuery ListCoreDefinitions where
  toQuery ListCoreDefinitions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListCoreDefinitionsResponse' smart constructor.
data ListCoreDefinitionsResponse = ListCoreDefinitionsResponse'
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
-- Create a value of 'ListCoreDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitions', 'listCoreDefinitionsResponse_definitions' - Information about a definition.
--
-- 'nextToken', 'listCoreDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listCoreDefinitionsResponse_httpStatus' - The response's http status code.
newListCoreDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoreDefinitionsResponse
newListCoreDefinitionsResponse pHttpStatus_ =
  ListCoreDefinitionsResponse'
    { definitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a definition.
listCoreDefinitionsResponse_definitions :: Lens.Lens' ListCoreDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listCoreDefinitionsResponse_definitions = Lens.lens (\ListCoreDefinitionsResponse' {definitions} -> definitions) (\s@ListCoreDefinitionsResponse' {} a -> s {definitions = a} :: ListCoreDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionsResponse_nextToken :: Lens.Lens' ListCoreDefinitionsResponse (Prelude.Maybe Prelude.Text)
listCoreDefinitionsResponse_nextToken = Lens.lens (\ListCoreDefinitionsResponse' {nextToken} -> nextToken) (\s@ListCoreDefinitionsResponse' {} a -> s {nextToken = a} :: ListCoreDefinitionsResponse)

-- | The response's http status code.
listCoreDefinitionsResponse_httpStatus :: Lens.Lens' ListCoreDefinitionsResponse Prelude.Int
listCoreDefinitionsResponse_httpStatus = Lens.lens (\ListCoreDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListCoreDefinitionsResponse' {} a -> s {httpStatus = a} :: ListCoreDefinitionsResponse)

instance Prelude.NFData ListCoreDefinitionsResponse where
  rnf ListCoreDefinitionsResponse' {..} =
    Prelude.rnf definitions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
