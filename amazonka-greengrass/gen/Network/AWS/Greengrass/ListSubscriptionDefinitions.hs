{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of subscription definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListSubscriptionDefinitions
  ( -- * Creating a Request
    ListSubscriptionDefinitions (..),
    newListSubscriptionDefinitions,

    -- * Request Lenses
    listSubscriptionDefinitions_nextToken,
    listSubscriptionDefinitions_maxResults,

    -- * Destructuring the Response
    ListSubscriptionDefinitionsResponse (..),
    newListSubscriptionDefinitionsResponse,

    -- * Response Lenses
    listSubscriptionDefinitionsResponse_nextToken,
    listSubscriptionDefinitionsResponse_definitions,
    listSubscriptionDefinitionsResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSubscriptionDefinitions' smart constructor.
data ListSubscriptionDefinitions = ListSubscriptionDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListSubscriptionDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listSubscriptionDefinitions_maxResults' - The maximum number of results to be returned per request.
newListSubscriptionDefinitions ::
  ListSubscriptionDefinitions
newListSubscriptionDefinitions =
  ListSubscriptionDefinitions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitions_nextToken :: Lens.Lens' ListSubscriptionDefinitions (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitions_nextToken = Lens.lens (\ListSubscriptionDefinitions' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitions' {} a -> s {nextToken = a} :: ListSubscriptionDefinitions)

-- | The maximum number of results to be returned per request.
listSubscriptionDefinitions_maxResults :: Lens.Lens' ListSubscriptionDefinitions (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitions_maxResults = Lens.lens (\ListSubscriptionDefinitions' {maxResults} -> maxResults) (\s@ListSubscriptionDefinitions' {} a -> s {maxResults = a} :: ListSubscriptionDefinitions)

instance Pager.AWSPager ListSubscriptionDefinitions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listSubscriptionDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listSubscriptionDefinitionsResponse_definitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listSubscriptionDefinitions_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListSubscriptionDefinitions
  where
  type
    Rs ListSubscriptionDefinitions =
      ListSubscriptionDefinitionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscriptionDefinitionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "Definitions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSubscriptionDefinitions

instance Prelude.NFData ListSubscriptionDefinitions

instance
  Prelude.ToHeaders
    ListSubscriptionDefinitions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListSubscriptionDefinitions where
  toPath =
    Prelude.const
      "/greengrass/definition/subscriptions"

instance Prelude.ToQuery ListSubscriptionDefinitions where
  toQuery ListSubscriptionDefinitions' {..} =
    Prelude.mconcat
      [ "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListSubscriptionDefinitionsResponse' smart constructor.
data ListSubscriptionDefinitionsResponse = ListSubscriptionDefinitionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about a definition.
    definitions :: Prelude.Maybe [DefinitionInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListSubscriptionDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listSubscriptionDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listSubscriptionDefinitionsResponse_httpStatus' - The response's http status code.
newListSubscriptionDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubscriptionDefinitionsResponse
newListSubscriptionDefinitionsResponse pHttpStatus_ =
  ListSubscriptionDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      definitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitionsResponse_nextToken :: Lens.Lens' ListSubscriptionDefinitionsResponse (Prelude.Maybe Prelude.Text)
listSubscriptionDefinitionsResponse_nextToken = Lens.lens (\ListSubscriptionDefinitionsResponse' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {nextToken = a} :: ListSubscriptionDefinitionsResponse)

-- | Information about a definition.
listSubscriptionDefinitionsResponse_definitions :: Lens.Lens' ListSubscriptionDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listSubscriptionDefinitionsResponse_definitions = Lens.lens (\ListSubscriptionDefinitionsResponse' {definitions} -> definitions) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {definitions = a} :: ListSubscriptionDefinitionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listSubscriptionDefinitionsResponse_httpStatus :: Lens.Lens' ListSubscriptionDefinitionsResponse Prelude.Int
listSubscriptionDefinitionsResponse_httpStatus = Lens.lens (\ListSubscriptionDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionDefinitionsResponse' {} a -> s {httpStatus = a} :: ListSubscriptionDefinitionsResponse)

instance
  Prelude.NFData
    ListSubscriptionDefinitionsResponse
