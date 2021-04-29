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
-- Module      : Network.AWS.Greengrass.ListCoreDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of core definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitions
  ( -- * Creating a Request
    ListCoreDefinitions (..),
    newListCoreDefinitions,

    -- * Request Lenses
    listCoreDefinitions_nextToken,
    listCoreDefinitions_maxResults,

    -- * Destructuring the Response
    ListCoreDefinitionsResponse (..),
    newListCoreDefinitionsResponse,

    -- * Response Lenses
    listCoreDefinitionsResponse_nextToken,
    listCoreDefinitionsResponse_definitions,
    listCoreDefinitionsResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCoreDefinitions' smart constructor.
data ListCoreDefinitions = ListCoreDefinitions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListCoreDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoreDefinitions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listCoreDefinitions_maxResults' - The maximum number of results to be returned per request.
newListCoreDefinitions ::
  ListCoreDefinitions
newListCoreDefinitions =
  ListCoreDefinitions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitions_nextToken :: Lens.Lens' ListCoreDefinitions (Prelude.Maybe Prelude.Text)
listCoreDefinitions_nextToken = Lens.lens (\ListCoreDefinitions' {nextToken} -> nextToken) (\s@ListCoreDefinitions' {} a -> s {nextToken = a} :: ListCoreDefinitions)

-- | The maximum number of results to be returned per request.
listCoreDefinitions_maxResults :: Lens.Lens' ListCoreDefinitions (Prelude.Maybe Prelude.Text)
listCoreDefinitions_maxResults = Lens.lens (\ListCoreDefinitions' {maxResults} -> maxResults) (\s@ListCoreDefinitions' {} a -> s {maxResults = a} :: ListCoreDefinitions)

instance Pager.AWSPager ListCoreDefinitions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listCoreDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listCoreDefinitionsResponse_definitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listCoreDefinitions_nextToken
          Lens..~ rs
          Lens.^? listCoreDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListCoreDefinitions where
  type
    Rs ListCoreDefinitions =
      ListCoreDefinitionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreDefinitionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "Definitions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCoreDefinitions

instance Prelude.NFData ListCoreDefinitions

instance Prelude.ToHeaders ListCoreDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListCoreDefinitions where
  toPath = Prelude.const "/greengrass/definition/cores"

instance Prelude.ToQuery ListCoreDefinitions where
  toQuery ListCoreDefinitions' {..} =
    Prelude.mconcat
      [ "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListCoreDefinitionsResponse' smart constructor.
data ListCoreDefinitionsResponse = ListCoreDefinitionsResponse'
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
-- Create a value of 'ListCoreDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoreDefinitionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'definitions', 'listCoreDefinitionsResponse_definitions' - Information about a definition.
--
-- 'httpStatus', 'listCoreDefinitionsResponse_httpStatus' - The response's http status code.
newListCoreDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoreDefinitionsResponse
newListCoreDefinitionsResponse pHttpStatus_ =
  ListCoreDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      definitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionsResponse_nextToken :: Lens.Lens' ListCoreDefinitionsResponse (Prelude.Maybe Prelude.Text)
listCoreDefinitionsResponse_nextToken = Lens.lens (\ListCoreDefinitionsResponse' {nextToken} -> nextToken) (\s@ListCoreDefinitionsResponse' {} a -> s {nextToken = a} :: ListCoreDefinitionsResponse)

-- | Information about a definition.
listCoreDefinitionsResponse_definitions :: Lens.Lens' ListCoreDefinitionsResponse (Prelude.Maybe [DefinitionInformation])
listCoreDefinitionsResponse_definitions = Lens.lens (\ListCoreDefinitionsResponse' {definitions} -> definitions) (\s@ListCoreDefinitionsResponse' {} a -> s {definitions = a} :: ListCoreDefinitionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listCoreDefinitionsResponse_httpStatus :: Lens.Lens' ListCoreDefinitionsResponse Prelude.Int
listCoreDefinitionsResponse_httpStatus = Lens.lens (\ListCoreDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListCoreDefinitionsResponse' {} a -> s {httpStatus = a} :: ListCoreDefinitionsResponse)

instance Prelude.NFData ListCoreDefinitionsResponse
