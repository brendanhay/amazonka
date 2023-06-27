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
-- Module      : Amazonka.CustomerProfiles.ListCalculatedAttributeDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists calculated attribute definitions for Customer Profiles
module Amazonka.CustomerProfiles.ListCalculatedAttributeDefinitions
  ( -- * Creating a Request
    ListCalculatedAttributeDefinitions (..),
    newListCalculatedAttributeDefinitions,

    -- * Request Lenses
    listCalculatedAttributeDefinitions_maxResults,
    listCalculatedAttributeDefinitions_nextToken,
    listCalculatedAttributeDefinitions_domainName,

    -- * Destructuring the Response
    ListCalculatedAttributeDefinitionsResponse (..),
    newListCalculatedAttributeDefinitionsResponse,

    -- * Response Lenses
    listCalculatedAttributeDefinitionsResponse_items,
    listCalculatedAttributeDefinitionsResponse_nextToken,
    listCalculatedAttributeDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCalculatedAttributeDefinitions' smart constructor.
data ListCalculatedAttributeDefinitions = ListCalculatedAttributeDefinitions'
  { -- | The maximum number of calculated attribute definitions returned per
    -- page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token from the previous call to
    -- ListCalculatedAttributeDefinitions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCalculatedAttributeDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCalculatedAttributeDefinitions_maxResults' - The maximum number of calculated attribute definitions returned per
-- page.
--
-- 'nextToken', 'listCalculatedAttributeDefinitions_nextToken' - The pagination token from the previous call to
-- ListCalculatedAttributeDefinitions.
--
-- 'domainName', 'listCalculatedAttributeDefinitions_domainName' - The unique name of the domain.
newListCalculatedAttributeDefinitions ::
  -- | 'domainName'
  Prelude.Text ->
  ListCalculatedAttributeDefinitions
newListCalculatedAttributeDefinitions pDomainName_ =
  ListCalculatedAttributeDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The maximum number of calculated attribute definitions returned per
-- page.
listCalculatedAttributeDefinitions_maxResults :: Lens.Lens' ListCalculatedAttributeDefinitions (Prelude.Maybe Prelude.Natural)
listCalculatedAttributeDefinitions_maxResults = Lens.lens (\ListCalculatedAttributeDefinitions' {maxResults} -> maxResults) (\s@ListCalculatedAttributeDefinitions' {} a -> s {maxResults = a} :: ListCalculatedAttributeDefinitions)

-- | The pagination token from the previous call to
-- ListCalculatedAttributeDefinitions.
listCalculatedAttributeDefinitions_nextToken :: Lens.Lens' ListCalculatedAttributeDefinitions (Prelude.Maybe Prelude.Text)
listCalculatedAttributeDefinitions_nextToken = Lens.lens (\ListCalculatedAttributeDefinitions' {nextToken} -> nextToken) (\s@ListCalculatedAttributeDefinitions' {} a -> s {nextToken = a} :: ListCalculatedAttributeDefinitions)

-- | The unique name of the domain.
listCalculatedAttributeDefinitions_domainName :: Lens.Lens' ListCalculatedAttributeDefinitions Prelude.Text
listCalculatedAttributeDefinitions_domainName = Lens.lens (\ListCalculatedAttributeDefinitions' {domainName} -> domainName) (\s@ListCalculatedAttributeDefinitions' {} a -> s {domainName = a} :: ListCalculatedAttributeDefinitions)

instance
  Core.AWSRequest
    ListCalculatedAttributeDefinitions
  where
  type
    AWSResponse ListCalculatedAttributeDefinitions =
      ListCalculatedAttributeDefinitionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCalculatedAttributeDefinitionsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCalculatedAttributeDefinitions
  where
  hashWithSalt
    _salt
    ListCalculatedAttributeDefinitions' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` domainName

instance
  Prelude.NFData
    ListCalculatedAttributeDefinitions
  where
  rnf ListCalculatedAttributeDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName

instance
  Data.ToHeaders
    ListCalculatedAttributeDefinitions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListCalculatedAttributeDefinitions
  where
  toPath ListCalculatedAttributeDefinitions' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/calculated-attributes"
      ]

instance
  Data.ToQuery
    ListCalculatedAttributeDefinitions
  where
  toQuery ListCalculatedAttributeDefinitions' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListCalculatedAttributeDefinitionsResponse' smart constructor.
data ListCalculatedAttributeDefinitionsResponse = ListCalculatedAttributeDefinitionsResponse'
  { -- | The list of calculated attribute definitions.
    items :: Prelude.Maybe [ListCalculatedAttributeDefinitionItem],
    -- | The pagination token from the previous call to
    -- ListCalculatedAttributeDefinitions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCalculatedAttributeDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listCalculatedAttributeDefinitionsResponse_items' - The list of calculated attribute definitions.
--
-- 'nextToken', 'listCalculatedAttributeDefinitionsResponse_nextToken' - The pagination token from the previous call to
-- ListCalculatedAttributeDefinitions.
--
-- 'httpStatus', 'listCalculatedAttributeDefinitionsResponse_httpStatus' - The response's http status code.
newListCalculatedAttributeDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCalculatedAttributeDefinitionsResponse
newListCalculatedAttributeDefinitionsResponse
  pHttpStatus_ =
    ListCalculatedAttributeDefinitionsResponse'
      { items =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of calculated attribute definitions.
listCalculatedAttributeDefinitionsResponse_items :: Lens.Lens' ListCalculatedAttributeDefinitionsResponse (Prelude.Maybe [ListCalculatedAttributeDefinitionItem])
listCalculatedAttributeDefinitionsResponse_items = Lens.lens (\ListCalculatedAttributeDefinitionsResponse' {items} -> items) (\s@ListCalculatedAttributeDefinitionsResponse' {} a -> s {items = a} :: ListCalculatedAttributeDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the previous call to
-- ListCalculatedAttributeDefinitions.
listCalculatedAttributeDefinitionsResponse_nextToken :: Lens.Lens' ListCalculatedAttributeDefinitionsResponse (Prelude.Maybe Prelude.Text)
listCalculatedAttributeDefinitionsResponse_nextToken = Lens.lens (\ListCalculatedAttributeDefinitionsResponse' {nextToken} -> nextToken) (\s@ListCalculatedAttributeDefinitionsResponse' {} a -> s {nextToken = a} :: ListCalculatedAttributeDefinitionsResponse)

-- | The response's http status code.
listCalculatedAttributeDefinitionsResponse_httpStatus :: Lens.Lens' ListCalculatedAttributeDefinitionsResponse Prelude.Int
listCalculatedAttributeDefinitionsResponse_httpStatus = Lens.lens (\ListCalculatedAttributeDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListCalculatedAttributeDefinitionsResponse' {} a -> s {httpStatus = a} :: ListCalculatedAttributeDefinitionsResponse)

instance
  Prelude.NFData
    ListCalculatedAttributeDefinitionsResponse
  where
  rnf ListCalculatedAttributeDefinitionsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
