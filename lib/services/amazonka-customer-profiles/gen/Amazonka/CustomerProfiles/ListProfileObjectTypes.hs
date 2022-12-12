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
-- Module      : Amazonka.CustomerProfiles.ListProfileObjectTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the templates available within the service.
module Amazonka.CustomerProfiles.ListProfileObjectTypes
  ( -- * Creating a Request
    ListProfileObjectTypes (..),
    newListProfileObjectTypes,

    -- * Request Lenses
    listProfileObjectTypes_maxResults,
    listProfileObjectTypes_nextToken,
    listProfileObjectTypes_domainName,

    -- * Destructuring the Response
    ListProfileObjectTypesResponse (..),
    newListProfileObjectTypesResponse,

    -- * Response Lenses
    listProfileObjectTypesResponse_items,
    listProfileObjectTypesResponse_nextToken,
    listProfileObjectTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProfileObjectTypes' smart constructor.
data ListProfileObjectTypes = ListProfileObjectTypes'
  { -- | The maximum number of objects returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileObjectTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProfileObjectTypes_maxResults' - The maximum number of objects returned per page.
--
-- 'nextToken', 'listProfileObjectTypes_nextToken' - Identifies the next page of results to return.
--
-- 'domainName', 'listProfileObjectTypes_domainName' - The unique name of the domain.
newListProfileObjectTypes ::
  -- | 'domainName'
  Prelude.Text ->
  ListProfileObjectTypes
newListProfileObjectTypes pDomainName_ =
  ListProfileObjectTypes'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The maximum number of objects returned per page.
listProfileObjectTypes_maxResults :: Lens.Lens' ListProfileObjectTypes (Prelude.Maybe Prelude.Natural)
listProfileObjectTypes_maxResults = Lens.lens (\ListProfileObjectTypes' {maxResults} -> maxResults) (\s@ListProfileObjectTypes' {} a -> s {maxResults = a} :: ListProfileObjectTypes)

-- | Identifies the next page of results to return.
listProfileObjectTypes_nextToken :: Lens.Lens' ListProfileObjectTypes (Prelude.Maybe Prelude.Text)
listProfileObjectTypes_nextToken = Lens.lens (\ListProfileObjectTypes' {nextToken} -> nextToken) (\s@ListProfileObjectTypes' {} a -> s {nextToken = a} :: ListProfileObjectTypes)

-- | The unique name of the domain.
listProfileObjectTypes_domainName :: Lens.Lens' ListProfileObjectTypes Prelude.Text
listProfileObjectTypes_domainName = Lens.lens (\ListProfileObjectTypes' {domainName} -> domainName) (\s@ListProfileObjectTypes' {} a -> s {domainName = a} :: ListProfileObjectTypes)

instance Core.AWSRequest ListProfileObjectTypes where
  type
    AWSResponse ListProfileObjectTypes =
      ListProfileObjectTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfileObjectTypesResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProfileObjectTypes where
  hashWithSalt _salt ListProfileObjectTypes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListProfileObjectTypes where
  rnf ListProfileObjectTypes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders ListProfileObjectTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProfileObjectTypes where
  toPath ListProfileObjectTypes' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainName, "/object-types"]

instance Data.ToQuery ListProfileObjectTypes where
  toQuery ListProfileObjectTypes' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListProfileObjectTypesResponse' smart constructor.
data ListProfileObjectTypesResponse = ListProfileObjectTypesResponse'
  { -- | The list of ListProfileObjectTypes instances.
    items :: Prelude.Maybe [ListProfileObjectTypeItem],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileObjectTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listProfileObjectTypesResponse_items' - The list of ListProfileObjectTypes instances.
--
-- 'nextToken', 'listProfileObjectTypesResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listProfileObjectTypesResponse_httpStatus' - The response's http status code.
newListProfileObjectTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfileObjectTypesResponse
newListProfileObjectTypesResponse pHttpStatus_ =
  ListProfileObjectTypesResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of ListProfileObjectTypes instances.
listProfileObjectTypesResponse_items :: Lens.Lens' ListProfileObjectTypesResponse (Prelude.Maybe [ListProfileObjectTypeItem])
listProfileObjectTypesResponse_items = Lens.lens (\ListProfileObjectTypesResponse' {items} -> items) (\s@ListProfileObjectTypesResponse' {} a -> s {items = a} :: ListProfileObjectTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listProfileObjectTypesResponse_nextToken :: Lens.Lens' ListProfileObjectTypesResponse (Prelude.Maybe Prelude.Text)
listProfileObjectTypesResponse_nextToken = Lens.lens (\ListProfileObjectTypesResponse' {nextToken} -> nextToken) (\s@ListProfileObjectTypesResponse' {} a -> s {nextToken = a} :: ListProfileObjectTypesResponse)

-- | The response's http status code.
listProfileObjectTypesResponse_httpStatus :: Lens.Lens' ListProfileObjectTypesResponse Prelude.Int
listProfileObjectTypesResponse_httpStatus = Lens.lens (\ListProfileObjectTypesResponse' {httpStatus} -> httpStatus) (\s@ListProfileObjectTypesResponse' {} a -> s {httpStatus = a} :: ListProfileObjectTypesResponse)

instance
  Prelude.NFData
    ListProfileObjectTypesResponse
  where
  rnf ListProfileObjectTypesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
