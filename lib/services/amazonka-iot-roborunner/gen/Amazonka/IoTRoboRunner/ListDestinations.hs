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
-- Module      : Amazonka.IoTRoboRunner.ListDestinations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to list destinations
--
-- This operation returns paginated results.
module Amazonka.IoTRoboRunner.ListDestinations
  ( -- * Creating a Request
    ListDestinations (..),
    newListDestinations,

    -- * Request Lenses
    listDestinations_maxResults,
    listDestinations_nextToken,
    listDestinations_state,
    listDestinations_site,

    -- * Destructuring the Response
    ListDestinationsResponse (..),
    newListDestinationsResponse,

    -- * Response Lenses
    listDestinationsResponse_destinations,
    listDestinationsResponse_nextToken,
    listDestinationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDestinations' smart constructor.
data ListDestinations = ListDestinations'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe DestinationState,
    site :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDestinations_maxResults' - Undocumented member.
--
-- 'nextToken', 'listDestinations_nextToken' - Undocumented member.
--
-- 'state', 'listDestinations_state' - Undocumented member.
--
-- 'site', 'listDestinations_site' - Undocumented member.
newListDestinations ::
  -- | 'site'
  Prelude.Text ->
  ListDestinations
newListDestinations pSite_ =
  ListDestinations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      state = Prelude.Nothing,
      site = pSite_
    }

-- | Undocumented member.
listDestinations_maxResults :: Lens.Lens' ListDestinations (Prelude.Maybe Prelude.Natural)
listDestinations_maxResults = Lens.lens (\ListDestinations' {maxResults} -> maxResults) (\s@ListDestinations' {} a -> s {maxResults = a} :: ListDestinations)

-- | Undocumented member.
listDestinations_nextToken :: Lens.Lens' ListDestinations (Prelude.Maybe Prelude.Text)
listDestinations_nextToken = Lens.lens (\ListDestinations' {nextToken} -> nextToken) (\s@ListDestinations' {} a -> s {nextToken = a} :: ListDestinations)

-- | Undocumented member.
listDestinations_state :: Lens.Lens' ListDestinations (Prelude.Maybe DestinationState)
listDestinations_state = Lens.lens (\ListDestinations' {state} -> state) (\s@ListDestinations' {} a -> s {state = a} :: ListDestinations)

-- | Undocumented member.
listDestinations_site :: Lens.Lens' ListDestinations Prelude.Text
listDestinations_site = Lens.lens (\ListDestinations' {site} -> site) (\s@ListDestinations' {} a -> s {site = a} :: ListDestinations)

instance Core.AWSPager ListDestinations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDestinationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDestinationsResponse_destinations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDestinations_nextToken
          Lens..~ rs
          Lens.^? listDestinationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDestinations where
  type
    AWSResponse ListDestinations =
      ListDestinationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDestinationsResponse'
            Prelude.<$> (x Data..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDestinations where
  hashWithSalt _salt ListDestinations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` site

instance Prelude.NFData ListDestinations where
  rnf ListDestinations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf site

instance Data.ToHeaders ListDestinations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDestinations where
  toPath = Prelude.const "/listDestinations"

instance Data.ToQuery ListDestinations where
  toQuery ListDestinations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "state" Data.=: state,
        "site" Data.=: site
      ]

-- | /See:/ 'newListDestinationsResponse' smart constructor.
data ListDestinationsResponse = ListDestinationsResponse'
  { destinations :: Prelude.Maybe [Destination],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'listDestinationsResponse_destinations' - Undocumented member.
--
-- 'nextToken', 'listDestinationsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listDestinationsResponse_httpStatus' - The response's http status code.
newListDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDestinationsResponse
newListDestinationsResponse pHttpStatus_ =
  ListDestinationsResponse'
    { destinations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listDestinationsResponse_destinations :: Lens.Lens' ListDestinationsResponse (Prelude.Maybe [Destination])
listDestinationsResponse_destinations = Lens.lens (\ListDestinationsResponse' {destinations} -> destinations) (\s@ListDestinationsResponse' {} a -> s {destinations = a} :: ListDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listDestinationsResponse_nextToken :: Lens.Lens' ListDestinationsResponse (Prelude.Maybe Prelude.Text)
listDestinationsResponse_nextToken = Lens.lens (\ListDestinationsResponse' {nextToken} -> nextToken) (\s@ListDestinationsResponse' {} a -> s {nextToken = a} :: ListDestinationsResponse)

-- | The response's http status code.
listDestinationsResponse_httpStatus :: Lens.Lens' ListDestinationsResponse Prelude.Int
listDestinationsResponse_httpStatus = Lens.lens (\ListDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListDestinationsResponse' {} a -> s {httpStatus = a} :: ListDestinationsResponse)

instance Prelude.NFData ListDestinationsResponse where
  rnf ListDestinationsResponse' {..} =
    Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
