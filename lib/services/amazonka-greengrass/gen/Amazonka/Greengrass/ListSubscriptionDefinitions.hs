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
-- Module      : Amazonka.Greengrass.ListSubscriptionDefinitions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of subscription definitions.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListSubscriptionDefinitions
  ( -- * Creating a Request
    ListSubscriptionDefinitions (..),
    newListSubscriptionDefinitions,

    -- * Request Lenses
    listSubscriptionDefinitions_maxResults,
    listSubscriptionDefinitions_nextToken,

    -- * Destructuring the Response
    ListSubscriptionDefinitionsResponse (..),
    newListSubscriptionDefinitionsResponse,

    -- * Response Lenses
    listSubscriptionDefinitionsResponse_definitions,
    listSubscriptionDefinitionsResponse_nextToken,
    listSubscriptionDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSubscriptionDefinitions' smart constructor.
data ListSubscriptionDefinitions = ListSubscriptionDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscriptionDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSubscriptionDefinitions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listSubscriptionDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
newListSubscriptionDefinitions ::
  ListSubscriptionDefinitions
newListSubscriptionDefinitions =
  ListSubscriptionDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listSubscriptionDefinitions_maxResults :: Lens.Lens' ListSubscriptionDefinitions (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitions_maxResults = Lens.lens (\ListSubscriptionDefinitions' {maxResults} -> maxResults) (\s@ListSubscriptionDefinitions' {} a -> s {maxResults = a} :: ListSubscriptionDefinitions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitions_nextToken :: Lens.Lens' ListSubscriptionDefinitions (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitions_nextToken = Lens.lens (\ListSubscriptionDefinitions' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitions' {} a -> s {nextToken = a} :: ListSubscriptionDefinitions)

instance Core.AWSPager ListSubscriptionDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscriptionDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscriptionDefinitionsResponse_definitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSubscriptionDefinitions_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSubscriptionDefinitions where
  type
    AWSResponse ListSubscriptionDefinitions =
      ListSubscriptionDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscriptionDefinitionsResponse'
            Prelude.<$> (x Data..?> "Definitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSubscriptionDefinitions where
  hashWithSalt _salt ListSubscriptionDefinitions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSubscriptionDefinitions where
  rnf ListSubscriptionDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSubscriptionDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSubscriptionDefinitions where
  toPath =
    Prelude.const
      "/greengrass/definition/subscriptions"

instance Data.ToQuery ListSubscriptionDefinitions where
  toQuery ListSubscriptionDefinitions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSubscriptionDefinitionsResponse' smart constructor.
data ListSubscriptionDefinitionsResponse = ListSubscriptionDefinitionsResponse'
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
-- Create a value of 'ListSubscriptionDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitions', 'listSubscriptionDefinitionsResponse_definitions' - Information about a definition.
--
-- 'nextToken', 'listSubscriptionDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listSubscriptionDefinitionsResponse_httpStatus' - The response's http status code.
newListSubscriptionDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubscriptionDefinitionsResponse
newListSubscriptionDefinitionsResponse pHttpStatus_ =
  ListSubscriptionDefinitionsResponse'
    { definitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a definition.
listSubscriptionDefinitionsResponse_definitions :: Lens.Lens' ListSubscriptionDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listSubscriptionDefinitionsResponse_definitions = Lens.lens (\ListSubscriptionDefinitionsResponse' {definitions} -> definitions) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {definitions = a} :: ListSubscriptionDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitionsResponse_nextToken :: Lens.Lens' ListSubscriptionDefinitionsResponse (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitionsResponse_nextToken = Lens.lens (\ListSubscriptionDefinitionsResponse' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {nextToken = a} :: ListSubscriptionDefinitionsResponse)

-- | The response's http status code.
listSubscriptionDefinitionsResponse_httpStatus :: Lens.Lens' ListSubscriptionDefinitionsResponse Prelude.Int
listSubscriptionDefinitionsResponse_httpStatus = Lens.lens (\ListSubscriptionDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {httpStatus = a} :: ListSubscriptionDefinitionsResponse)

instance
  Prelude.NFData
    ListSubscriptionDefinitionsResponse
  where
  rnf ListSubscriptionDefinitionsResponse' {..} =
    Prelude.rnf definitions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
