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
-- Module      : Amazonka.Amplify.ListDomainAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the domain associations for an Amplify app.
--
-- This operation returns paginated results.
module Amazonka.Amplify.ListDomainAssociations
  ( -- * Creating a Request
    ListDomainAssociations (..),
    newListDomainAssociations,

    -- * Request Lenses
    listDomainAssociations_maxResults,
    listDomainAssociations_nextToken,
    listDomainAssociations_appId,

    -- * Destructuring the Response
    ListDomainAssociationsResponse (..),
    newListDomainAssociationsResponse,

    -- * Response Lenses
    listDomainAssociationsResponse_nextToken,
    listDomainAssociationsResponse_httpStatus,
    listDomainAssociationsResponse_domainAssociations,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the list domain associations request.
--
-- /See:/ 'newListDomainAssociations' smart constructor.
data ListDomainAssociations = ListDomainAssociations'
  { -- | The maximum number of records to list in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token. Set to null to start listing apps from the start. If
    -- non-null, a pagination token is returned in a result. Pass its value in
    -- here to list more projects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDomainAssociations_maxResults' - The maximum number of records to list in a single response.
--
-- 'nextToken', 'listDomainAssociations_nextToken' - A pagination token. Set to null to start listing apps from the start. If
-- non-null, a pagination token is returned in a result. Pass its value in
-- here to list more projects.
--
-- 'appId', 'listDomainAssociations_appId' - The unique ID for an Amplify app.
newListDomainAssociations ::
  -- | 'appId'
  Prelude.Text ->
  ListDomainAssociations
newListDomainAssociations pAppId_ =
  ListDomainAssociations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appId = pAppId_
    }

-- | The maximum number of records to list in a single response.
listDomainAssociations_maxResults :: Lens.Lens' ListDomainAssociations (Prelude.Maybe Prelude.Natural)
listDomainAssociations_maxResults = Lens.lens (\ListDomainAssociations' {maxResults} -> maxResults) (\s@ListDomainAssociations' {} a -> s {maxResults = a} :: ListDomainAssociations)

-- | A pagination token. Set to null to start listing apps from the start. If
-- non-null, a pagination token is returned in a result. Pass its value in
-- here to list more projects.
listDomainAssociations_nextToken :: Lens.Lens' ListDomainAssociations (Prelude.Maybe Prelude.Text)
listDomainAssociations_nextToken = Lens.lens (\ListDomainAssociations' {nextToken} -> nextToken) (\s@ListDomainAssociations' {} a -> s {nextToken = a} :: ListDomainAssociations)

-- | The unique ID for an Amplify app.
listDomainAssociations_appId :: Lens.Lens' ListDomainAssociations Prelude.Text
listDomainAssociations_appId = Lens.lens (\ListDomainAssociations' {appId} -> appId) (\s@ListDomainAssociations' {} a -> s {appId = a} :: ListDomainAssociations)

instance Core.AWSPager ListDomainAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listDomainAssociationsResponse_domainAssociations
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDomainAssociations_nextToken
          Lens..~ rs
          Lens.^? listDomainAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDomainAssociations where
  type
    AWSResponse ListDomainAssociations =
      ListDomainAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainAssociationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "domainAssociations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDomainAssociations where
  hashWithSalt _salt ListDomainAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appId

instance Prelude.NFData ListDomainAssociations where
  rnf ListDomainAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders ListDomainAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDomainAssociations where
  toPath ListDomainAssociations' {..} =
    Prelude.mconcat
      ["/apps/", Data.toBS appId, "/domains"]

instance Data.ToQuery ListDomainAssociations where
  toQuery ListDomainAssociations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | The result structure for the list domain association request.
--
-- /See:/ 'newListDomainAssociationsResponse' smart constructor.
data ListDomainAssociationsResponse = ListDomainAssociationsResponse'
  { -- | A pagination token. If non-null, a pagination token is returned in a
    -- result. Pass its value in another request to retrieve more entries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of domain associations.
    domainAssociations :: [DomainAssociation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainAssociationsResponse_nextToken' - A pagination token. If non-null, a pagination token is returned in a
-- result. Pass its value in another request to retrieve more entries.
--
-- 'httpStatus', 'listDomainAssociationsResponse_httpStatus' - The response's http status code.
--
-- 'domainAssociations', 'listDomainAssociationsResponse_domainAssociations' - A list of domain associations.
newListDomainAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainAssociationsResponse
newListDomainAssociationsResponse pHttpStatus_ =
  ListDomainAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      domainAssociations = Prelude.mempty
    }

-- | A pagination token. If non-null, a pagination token is returned in a
-- result. Pass its value in another request to retrieve more entries.
listDomainAssociationsResponse_nextToken :: Lens.Lens' ListDomainAssociationsResponse (Prelude.Maybe Prelude.Text)
listDomainAssociationsResponse_nextToken = Lens.lens (\ListDomainAssociationsResponse' {nextToken} -> nextToken) (\s@ListDomainAssociationsResponse' {} a -> s {nextToken = a} :: ListDomainAssociationsResponse)

-- | The response's http status code.
listDomainAssociationsResponse_httpStatus :: Lens.Lens' ListDomainAssociationsResponse Prelude.Int
listDomainAssociationsResponse_httpStatus = Lens.lens (\ListDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListDomainAssociationsResponse' {} a -> s {httpStatus = a} :: ListDomainAssociationsResponse)

-- | A list of domain associations.
listDomainAssociationsResponse_domainAssociations :: Lens.Lens' ListDomainAssociationsResponse [DomainAssociation]
listDomainAssociationsResponse_domainAssociations = Lens.lens (\ListDomainAssociationsResponse' {domainAssociations} -> domainAssociations) (\s@ListDomainAssociationsResponse' {} a -> s {domainAssociations = a} :: ListDomainAssociationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListDomainAssociationsResponse
  where
  rnf ListDomainAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainAssociations
