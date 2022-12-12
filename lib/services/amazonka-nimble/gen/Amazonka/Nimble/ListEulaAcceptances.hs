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
-- Module      : Amazonka.Nimble.ListEulaAcceptances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List Eula Acceptances.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListEulaAcceptances
  ( -- * Creating a Request
    ListEulaAcceptances (..),
    newListEulaAcceptances,

    -- * Request Lenses
    listEulaAcceptances_eulaIds,
    listEulaAcceptances_nextToken,
    listEulaAcceptances_studioId,

    -- * Destructuring the Response
    ListEulaAcceptancesResponse (..),
    newListEulaAcceptancesResponse,

    -- * Response Lenses
    listEulaAcceptancesResponse_eulaAcceptances,
    listEulaAcceptancesResponse_nextToken,
    listEulaAcceptancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEulaAcceptances' smart constructor.
data ListEulaAcceptances = ListEulaAcceptances'
  { -- | The list of EULA IDs that have been previously accepted.
    eulaIds :: Prelude.Maybe [Prelude.Text],
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEulaAcceptances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eulaIds', 'listEulaAcceptances_eulaIds' - The list of EULA IDs that have been previously accepted.
--
-- 'nextToken', 'listEulaAcceptances_nextToken' - The token to request the next page of results.
--
-- 'studioId', 'listEulaAcceptances_studioId' - The studio ID.
newListEulaAcceptances ::
  -- | 'studioId'
  Prelude.Text ->
  ListEulaAcceptances
newListEulaAcceptances pStudioId_ =
  ListEulaAcceptances'
    { eulaIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The list of EULA IDs that have been previously accepted.
listEulaAcceptances_eulaIds :: Lens.Lens' ListEulaAcceptances (Prelude.Maybe [Prelude.Text])
listEulaAcceptances_eulaIds = Lens.lens (\ListEulaAcceptances' {eulaIds} -> eulaIds) (\s@ListEulaAcceptances' {} a -> s {eulaIds = a} :: ListEulaAcceptances) Prelude.. Lens.mapping Lens.coerced

-- | The token to request the next page of results.
listEulaAcceptances_nextToken :: Lens.Lens' ListEulaAcceptances (Prelude.Maybe Prelude.Text)
listEulaAcceptances_nextToken = Lens.lens (\ListEulaAcceptances' {nextToken} -> nextToken) (\s@ListEulaAcceptances' {} a -> s {nextToken = a} :: ListEulaAcceptances)

-- | The studio ID.
listEulaAcceptances_studioId :: Lens.Lens' ListEulaAcceptances Prelude.Text
listEulaAcceptances_studioId = Lens.lens (\ListEulaAcceptances' {studioId} -> studioId) (\s@ListEulaAcceptances' {} a -> s {studioId = a} :: ListEulaAcceptances)

instance Core.AWSPager ListEulaAcceptances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEulaAcceptancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEulaAcceptancesResponse_eulaAcceptances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEulaAcceptances_nextToken
          Lens..~ rs
          Lens.^? listEulaAcceptancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEulaAcceptances where
  type
    AWSResponse ListEulaAcceptances =
      ListEulaAcceptancesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEulaAcceptancesResponse'
            Prelude.<$> ( x Data..?> "eulaAcceptances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEulaAcceptances where
  hashWithSalt _salt ListEulaAcceptances' {..} =
    _salt `Prelude.hashWithSalt` eulaIds
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListEulaAcceptances where
  rnf ListEulaAcceptances' {..} =
    Prelude.rnf eulaIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders ListEulaAcceptances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEulaAcceptances where
  toPath ListEulaAcceptances' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/eula-acceptances"
      ]

instance Data.ToQuery ListEulaAcceptances where
  toQuery ListEulaAcceptances' {..} =
    Prelude.mconcat
      [ "eulaIds"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> eulaIds),
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEulaAcceptancesResponse' smart constructor.
data ListEulaAcceptancesResponse = ListEulaAcceptancesResponse'
  { -- | A collection of EULA acceptances.
    eulaAcceptances :: Prelude.Maybe [EulaAcceptance],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEulaAcceptancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eulaAcceptances', 'listEulaAcceptancesResponse_eulaAcceptances' - A collection of EULA acceptances.
--
-- 'nextToken', 'listEulaAcceptancesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listEulaAcceptancesResponse_httpStatus' - The response's http status code.
newListEulaAcceptancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEulaAcceptancesResponse
newListEulaAcceptancesResponse pHttpStatus_ =
  ListEulaAcceptancesResponse'
    { eulaAcceptances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of EULA acceptances.
listEulaAcceptancesResponse_eulaAcceptances :: Lens.Lens' ListEulaAcceptancesResponse (Prelude.Maybe [EulaAcceptance])
listEulaAcceptancesResponse_eulaAcceptances = Lens.lens (\ListEulaAcceptancesResponse' {eulaAcceptances} -> eulaAcceptances) (\s@ListEulaAcceptancesResponse' {} a -> s {eulaAcceptances = a} :: ListEulaAcceptancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listEulaAcceptancesResponse_nextToken :: Lens.Lens' ListEulaAcceptancesResponse (Prelude.Maybe Prelude.Text)
listEulaAcceptancesResponse_nextToken = Lens.lens (\ListEulaAcceptancesResponse' {nextToken} -> nextToken) (\s@ListEulaAcceptancesResponse' {} a -> s {nextToken = a} :: ListEulaAcceptancesResponse)

-- | The response's http status code.
listEulaAcceptancesResponse_httpStatus :: Lens.Lens' ListEulaAcceptancesResponse Prelude.Int
listEulaAcceptancesResponse_httpStatus = Lens.lens (\ListEulaAcceptancesResponse' {httpStatus} -> httpStatus) (\s@ListEulaAcceptancesResponse' {} a -> s {httpStatus = a} :: ListEulaAcceptancesResponse)

instance Prelude.NFData ListEulaAcceptancesResponse where
  rnf ListEulaAcceptancesResponse' {..} =
    Prelude.rnf eulaAcceptances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
