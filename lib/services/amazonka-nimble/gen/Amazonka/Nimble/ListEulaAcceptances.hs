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
    listEulaAcceptances_nextToken,
    listEulaAcceptances_eulaIds,
    listEulaAcceptances_studioId,

    -- * Destructuring the Response
    ListEulaAcceptancesResponse (..),
    newListEulaAcceptancesResponse,

    -- * Response Lenses
    listEulaAcceptancesResponse_nextToken,
    listEulaAcceptancesResponse_eulaAcceptances,
    listEulaAcceptancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEulaAcceptances' smart constructor.
data ListEulaAcceptances = ListEulaAcceptances'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of EULA IDs that have been previously accepted.
    eulaIds :: Prelude.Maybe [Prelude.Text],
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
-- 'nextToken', 'listEulaAcceptances_nextToken' - The token to request the next page of results.
--
-- 'eulaIds', 'listEulaAcceptances_eulaIds' - The list of EULA IDs that have been previously accepted.
--
-- 'studioId', 'listEulaAcceptances_studioId' - The studio ID.
newListEulaAcceptances ::
  -- | 'studioId'
  Prelude.Text ->
  ListEulaAcceptances
newListEulaAcceptances pStudioId_ =
  ListEulaAcceptances'
    { nextToken = Prelude.Nothing,
      eulaIds = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The token to request the next page of results.
listEulaAcceptances_nextToken :: Lens.Lens' ListEulaAcceptances (Prelude.Maybe Prelude.Text)
listEulaAcceptances_nextToken = Lens.lens (\ListEulaAcceptances' {nextToken} -> nextToken) (\s@ListEulaAcceptances' {} a -> s {nextToken = a} :: ListEulaAcceptances)

-- | The list of EULA IDs that have been previously accepted.
listEulaAcceptances_eulaIds :: Lens.Lens' ListEulaAcceptances (Prelude.Maybe [Prelude.Text])
listEulaAcceptances_eulaIds = Lens.lens (\ListEulaAcceptances' {eulaIds} -> eulaIds) (\s@ListEulaAcceptances' {} a -> s {eulaIds = a} :: ListEulaAcceptances) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "eulaAcceptances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEulaAcceptances where
  hashWithSalt _salt ListEulaAcceptances' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` eulaIds
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListEulaAcceptances where
  rnf ListEulaAcceptances' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eulaIds
      `Prelude.seq` Prelude.rnf studioId

instance Core.ToHeaders ListEulaAcceptances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListEulaAcceptances where
  toPath ListEulaAcceptances' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/eula-acceptances"
      ]

instance Core.ToQuery ListEulaAcceptances where
  toQuery ListEulaAcceptances' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "eulaIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> eulaIds)
      ]

-- | /See:/ 'newListEulaAcceptancesResponse' smart constructor.
data ListEulaAcceptancesResponse = ListEulaAcceptancesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of EULA acceptances.
    eulaAcceptances :: Prelude.Maybe [EulaAcceptance],
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
-- 'nextToken', 'listEulaAcceptancesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'eulaAcceptances', 'listEulaAcceptancesResponse_eulaAcceptances' - A collection of EULA acceptances.
--
-- 'httpStatus', 'listEulaAcceptancesResponse_httpStatus' - The response's http status code.
newListEulaAcceptancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEulaAcceptancesResponse
newListEulaAcceptancesResponse pHttpStatus_ =
  ListEulaAcceptancesResponse'
    { nextToken =
        Prelude.Nothing,
      eulaAcceptances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listEulaAcceptancesResponse_nextToken :: Lens.Lens' ListEulaAcceptancesResponse (Prelude.Maybe Prelude.Text)
listEulaAcceptancesResponse_nextToken = Lens.lens (\ListEulaAcceptancesResponse' {nextToken} -> nextToken) (\s@ListEulaAcceptancesResponse' {} a -> s {nextToken = a} :: ListEulaAcceptancesResponse)

-- | A collection of EULA acceptances.
listEulaAcceptancesResponse_eulaAcceptances :: Lens.Lens' ListEulaAcceptancesResponse (Prelude.Maybe [EulaAcceptance])
listEulaAcceptancesResponse_eulaAcceptances = Lens.lens (\ListEulaAcceptancesResponse' {eulaAcceptances} -> eulaAcceptances) (\s@ListEulaAcceptancesResponse' {} a -> s {eulaAcceptances = a} :: ListEulaAcceptancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEulaAcceptancesResponse_httpStatus :: Lens.Lens' ListEulaAcceptancesResponse Prelude.Int
listEulaAcceptancesResponse_httpStatus = Lens.lens (\ListEulaAcceptancesResponse' {httpStatus} -> httpStatus) (\s@ListEulaAcceptancesResponse' {} a -> s {httpStatus = a} :: ListEulaAcceptancesResponse)

instance Prelude.NFData ListEulaAcceptancesResponse where
  rnf ListEulaAcceptancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf eulaAcceptances
      `Prelude.seq` Prelude.rnf httpStatus
