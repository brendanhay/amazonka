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
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Lambda function definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitions
  ( -- * Creating a Request
    ListFunctionDefinitions (..),
    newListFunctionDefinitions,

    -- * Request Lenses
    listFunctionDefinitions_nextToken,
    listFunctionDefinitions_maxResults,

    -- * Destructuring the Response
    ListFunctionDefinitionsResponse (..),
    newListFunctionDefinitionsResponse,

    -- * Response Lenses
    listFunctionDefinitionsResponse_nextToken,
    listFunctionDefinitionsResponse_definitions,
    listFunctionDefinitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFunctionDefinitions' smart constructor.
data ListFunctionDefinitions = ListFunctionDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'listFunctionDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listFunctionDefinitions_maxResults' - The maximum number of results to be returned per request.
newListFunctionDefinitions ::
  ListFunctionDefinitions
newListFunctionDefinitions =
  ListFunctionDefinitions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitions_nextToken :: Lens.Lens' ListFunctionDefinitions (Prelude.Maybe Prelude.Text)
listFunctionDefinitions_nextToken = Lens.lens (\ListFunctionDefinitions' {nextToken} -> nextToken) (\s@ListFunctionDefinitions' {} a -> s {nextToken = a} :: ListFunctionDefinitions)

-- | The maximum number of results to be returned per request.
listFunctionDefinitions_maxResults :: Lens.Lens' ListFunctionDefinitions (Prelude.Maybe Prelude.Text)
listFunctionDefinitions_maxResults = Lens.lens (\ListFunctionDefinitions' {maxResults} -> maxResults) (\s@ListFunctionDefinitions' {} a -> s {maxResults = a} :: ListFunctionDefinitions)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionDefinitionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Definitions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFunctionDefinitions

instance Prelude.NFData ListFunctionDefinitions

instance Core.ToHeaders ListFunctionDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListFunctionDefinitions where
  toPath =
    Prelude.const "/greengrass/definition/functions"

instance Core.ToQuery ListFunctionDefinitions where
  toQuery ListFunctionDefinitions' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListFunctionDefinitionsResponse' smart constructor.
data ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about a definition.
    definitions :: Prelude.Maybe [DefinitionInformation],
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
-- 'nextToken', 'listFunctionDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listFunctionDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listFunctionDefinitionsResponse_httpStatus' - The response's http status code.
newListFunctionDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionDefinitionsResponse
newListFunctionDefinitionsResponse pHttpStatus_ =
  ListFunctionDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      definitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitionsResponse_nextToken :: Lens.Lens' ListFunctionDefinitionsResponse (Prelude.Maybe Prelude.Text)
listFunctionDefinitionsResponse_nextToken = Lens.lens (\ListFunctionDefinitionsResponse' {nextToken} -> nextToken) (\s@ListFunctionDefinitionsResponse' {} a -> s {nextToken = a} :: ListFunctionDefinitionsResponse)

-- | Information about a definition.
listFunctionDefinitionsResponse_definitions :: Lens.Lens' ListFunctionDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listFunctionDefinitionsResponse_definitions = Lens.lens (\ListFunctionDefinitionsResponse' {definitions} -> definitions) (\s@ListFunctionDefinitionsResponse' {} a -> s {definitions = a} :: ListFunctionDefinitionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listFunctionDefinitionsResponse_httpStatus :: Lens.Lens' ListFunctionDefinitionsResponse Prelude.Int
listFunctionDefinitionsResponse_httpStatus = Lens.lens (\ListFunctionDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionDefinitionsResponse' {} a -> s {httpStatus = a} :: ListFunctionDefinitionsResponse)

instance
  Prelude.NFData
    ListFunctionDefinitionsResponse
