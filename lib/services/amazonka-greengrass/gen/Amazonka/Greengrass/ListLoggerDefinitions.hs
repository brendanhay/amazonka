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
-- Module      : Amazonka.Greengrass.ListLoggerDefinitions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of logger definitions.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListLoggerDefinitions
  ( -- * Creating a Request
    ListLoggerDefinitions (..),
    newListLoggerDefinitions,

    -- * Request Lenses
    listLoggerDefinitions_maxResults,
    listLoggerDefinitions_nextToken,

    -- * Destructuring the Response
    ListLoggerDefinitionsResponse (..),
    newListLoggerDefinitionsResponse,

    -- * Response Lenses
    listLoggerDefinitionsResponse_definitions,
    listLoggerDefinitionsResponse_nextToken,
    listLoggerDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLoggerDefinitions' smart constructor.
data ListLoggerDefinitions = ListLoggerDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLoggerDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLoggerDefinitions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listLoggerDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
newListLoggerDefinitions ::
  ListLoggerDefinitions
newListLoggerDefinitions =
  ListLoggerDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listLoggerDefinitions_maxResults :: Lens.Lens' ListLoggerDefinitions (Prelude.Maybe Prelude.Text)
listLoggerDefinitions_maxResults = Lens.lens (\ListLoggerDefinitions' {maxResults} -> maxResults) (\s@ListLoggerDefinitions' {} a -> s {maxResults = a} :: ListLoggerDefinitions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listLoggerDefinitions_nextToken :: Lens.Lens' ListLoggerDefinitions (Prelude.Maybe Prelude.Text)
listLoggerDefinitions_nextToken = Lens.lens (\ListLoggerDefinitions' {nextToken} -> nextToken) (\s@ListLoggerDefinitions' {} a -> s {nextToken = a} :: ListLoggerDefinitions)

instance Core.AWSPager ListLoggerDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLoggerDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLoggerDefinitionsResponse_definitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLoggerDefinitions_nextToken
          Lens..~ rs
          Lens.^? listLoggerDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLoggerDefinitions where
  type
    AWSResponse ListLoggerDefinitions =
      ListLoggerDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLoggerDefinitionsResponse'
            Prelude.<$> (x Data..?> "Definitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLoggerDefinitions where
  hashWithSalt _salt ListLoggerDefinitions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLoggerDefinitions where
  rnf ListLoggerDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLoggerDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListLoggerDefinitions where
  toPath =
    Prelude.const "/greengrass/definition/loggers"

instance Data.ToQuery ListLoggerDefinitions where
  toQuery ListLoggerDefinitions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListLoggerDefinitionsResponse' smart constructor.
data ListLoggerDefinitionsResponse = ListLoggerDefinitionsResponse'
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
-- Create a value of 'ListLoggerDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitions', 'listLoggerDefinitionsResponse_definitions' - Information about a definition.
--
-- 'nextToken', 'listLoggerDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listLoggerDefinitionsResponse_httpStatus' - The response's http status code.
newListLoggerDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLoggerDefinitionsResponse
newListLoggerDefinitionsResponse pHttpStatus_ =
  ListLoggerDefinitionsResponse'
    { definitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a definition.
listLoggerDefinitionsResponse_definitions :: Lens.Lens' ListLoggerDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listLoggerDefinitionsResponse_definitions = Lens.lens (\ListLoggerDefinitionsResponse' {definitions} -> definitions) (\s@ListLoggerDefinitionsResponse' {} a -> s {definitions = a} :: ListLoggerDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listLoggerDefinitionsResponse_nextToken :: Lens.Lens' ListLoggerDefinitionsResponse (Prelude.Maybe Prelude.Text)
listLoggerDefinitionsResponse_nextToken = Lens.lens (\ListLoggerDefinitionsResponse' {nextToken} -> nextToken) (\s@ListLoggerDefinitionsResponse' {} a -> s {nextToken = a} :: ListLoggerDefinitionsResponse)

-- | The response's http status code.
listLoggerDefinitionsResponse_httpStatus :: Lens.Lens' ListLoggerDefinitionsResponse Prelude.Int
listLoggerDefinitionsResponse_httpStatus = Lens.lens (\ListLoggerDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListLoggerDefinitionsResponse' {} a -> s {httpStatus = a} :: ListLoggerDefinitionsResponse)

instance Prelude.NFData ListLoggerDefinitionsResponse where
  rnf ListLoggerDefinitionsResponse' {..} =
    Prelude.rnf definitions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
